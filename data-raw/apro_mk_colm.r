
library(tidyverse)
library(xlsx)
library(restatapi)

# local output folder
extraction_folder <- "C:/Users/himicmi/OneDrive - European Commission/GRP-AGRI-A2 - Documents/Market Analysis and Outlook/05. Short Term Market Forecasts/Dairy/eurostat/2026_2/Eurostat download with R"

#
# get the date of last update
# this will define the name of the output file to track changes of the EUROSTAT versions
#
s_update <- search_eurostat_toc("milk") |> filter(code == "apro_mk_colm")
s_update$lastUpdate


# PART I - Get the data
#======================

# Function to get the apro_mk_colm data from the Eurostat website
#
get_apro_mk_colm <- function(){
  
  apro_mk_colm <- get_eurostat_data(id="apro_mk_colm")
  apro_mk_colm <- as_tibble(apro_mk_colm)
  
  # get only the data after 2010
  apro_mk_colm$year <-  as.numeric(substring(as.character(apro_mk_colm$time), 1, 4))
  apro_mk_colm <- apro_mk_colm %>% filter(year > 2009)
  apro_mk_colm <- apro_mk_colm %>% select(-year)
  
  
  
  # convert the format of the dates (year_M_month)
  apro_mk_colm$time <-  gsub("-","M",apro_mk_colm$time)

  
  # create a variable name by combining (geo,dairyprod,milkitem)
  # this will be used in the vlookup's in Excel...
  apro_mk_colm <- apro_mk_colm %>% mutate(varname = str_c(geo,dairyprod,unit,milkitem, sep = "_"))
  
  #get dictionary
  apro_mk_colm_dic <- get_eurostat_dsd(id = "apro_mk_colm")
  apro_mk_colm_dic <- as_tibble(apro_mk_colm_dic)
  
  mk_colm_dairyprod <- apro_mk_colm_dic %>% filter(concept == "dairyprod")
    
  # merge to get the labels
  apro_mk_colm <- apro_mk_colm %>% left_join(mk_colm_dairyprod, by = c("dairyprod"="code"))
  apro_mk_colm <- apro_mk_colm %>% mutate(varlabel = str_c(geo,name, sep = "_"))
 
  # merge unit and milkitem (e.g. to get PC_FAT from PC and FAT)
  apro_mk_colm <- apro_mk_colm %>% mutate(unit=str_c(unit,milkitem, sep = "_"))
 
  # select only the columns we need
  apro_mk_colm <- apro_mk_colm %>% select(varname,varlabel,dairyprod,unit,geo,time,values)
  
  # rearrange to have the years in the columns
  colnames(apro_mk_colm)[7] <- "value"
  
  
  return(apro_mk_colm)
  
}

# use the function and get the data
apro_mk_colm <- get_apro_mk_colm()


# write downloaded and processed data to Excel 
source("R/save_to_excel.r")

save_to_excel(tibble_to_save = apro_mk_colm, folder_to_save = extraction_folder)


# PART II - Check for data updates at Eurostat server
#====================================================

rm(apro_mk_colm)

# load the old file... 
old_folder <- "C:/Users/himicmi/OneDrive - European Commission/GRP-AGRI-A2 - Documents/Market Analysis and Outlook/05. Short Term Market Forecasts/Dairy/eurostat/2026_1/Eurostat download with R/"
load(file = paste(old_folder,"apro_mk_colm_2026.03.16.RData",sep = ""))
old <- tibble_to_save
rm(tibble_to_save)

# then use the code above: go to Eurostat and grab the latest version
# save the new apro_mk_colm dataset on 'new'
load(file = paste(extraction_folder,"/apro_mk_colm_2026.06.04.RData",sep = ""))
new <- tibble_to_save
rm(tibble_to_save)

# merge the old and the new versions and check what was updated
x <- new %>% left_join(old, by = c("varname","varlabel","dairyprod","unit","geo","time"))
data_update1 <- x %>% filter(!is.na(value.x)) %>% filter(is.na(value.y))
data_update2 <- x %>% filter(value.x!=value.y)
data_update3 <- x %>% filter(is.na(value.x)) %>% filter(!is.na(value.y))

data_update <- rbind(data_update1, data_update2, data_update3)

# save data update
save(data_update, file = paste(extraction_folder,"apro_mk_colm_update", format(Sys.time(), "%Y-%m-%d"), ".RData", sep = ""))

# clean up
rm(data_update, data_update1, data_update3, data_update2, x, old, new)
