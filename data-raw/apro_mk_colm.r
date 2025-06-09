
library(eurostat)
library(tidyverse)
library(xlsx)
library(restatapi)

# output folder on U: drive
extraction_folder <- "U:/4-Market Analysis/4-2 Short-Term Outlook/Outlook Dairy/Short term dairy/2025_1/Eurostat download with R/"

# PART I - Get the data
#======================

# Function to get the apro_mk_colm data from the Eurostat website
#
get_apro_mk_colm <- function(){
  
  apro_mk_colm <- get_eurostat(id="apro_mk_colm")
  apro_mk_colm <- as_tibble(apro_mk_colm)
  
  # get only the data after 2010
  apro_mk_colm$year <-  as.numeric(format(apro_mk_colm$TIME_PERIOD, format="%Y"))
  apro_mk_colm <- apro_mk_colm %>% filter(year > 2009)
  apro_mk_colm <- apro_mk_colm %>% select(-year)
  
  # convert the format of the dates (we only need years)
  apro_mk_colm$time <-  format(apro_mk_colm$TIME_PERIOD, format="%YM%m")
  
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

# Note that pivot_wider would put the columns/years in the order of the first appearance
# Therefore, we first sort the table from 1960 to the latest year (increasing order)
#! need a filter on years!! >2010
excel_out <- apro_mk_colm %>% arrange(time) %>% pivot_wider(names_from = time)

write.xlsx(as.data.frame(excel_out), file = paste(extraction_folder,"apro_mk_colm_fromR.xlsx",sep = ""), 
           row.names = FALSE, col.names = TRUE, sheetName = "apro_mk_colm",
           showNA = TRUE)

# add timestamp
timestamp <- format(Sys.time(), "data extracted on %Y.%m.%d-%H:%M:%S")
write.xlsx(timestamp, file = paste(extraction_folder,"apro_mk_colm_fromR.xlsx",sep = ""), 
           row.names = FALSE, col.names = TRUE, sheetName = "timestamp",
           showNA = TRUE, append = TRUE)

# save data extraction also in R data format
# Add time stamp (day) to indicate the date of extraction
#save(apro_mk_colm, file = "data/apro_mk_colm.RData")
save(apro_mk_colm, file = paste(extraction_folder,"apro_mk_colm_", format(Sys.time(), "%Y-%m-%d"), ".RData", sep = ""))


# PART II - Check for data updates at Eurostat server
#====================================================

rm(apro_mk_colm)

# load the old file... 
old_folder <- "U:/4-Market Analysis/4-2 Short-Term Outlook/Outlook Dairy/Short term dairy/2024_2/Eurostat download with R/"
load(file = paste(old_folder,"apro_mk_colm_2024-08-30.RData",sep = ""))
old <- apro_mk_colm
rm(apro_mk_colm)

# then use the code above: go to Eurostat and grab the latest version
# save the new apro_mk_colm dataset on 'new'
load(file = paste(extraction_folder,"apro_mk_colm_2025-06-09.RData",sep = ""))
new <- apro_mk_colm
rm(apro_mk_colm)

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
