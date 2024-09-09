# get slaughtering statistics from Eurostat
# Cows and heifers

library(eurostat)
library(tidyverse)
library(xlsx)
library(restatapi)

# output folder on U: drive
extraction_folder <- "U:/4-Market Analysis/4-2 Short-Term Outlook/Outlook Dairy/Short term dairy/2024_2/Eurostat download with R/"


# PART I - Get the data
#======================

# Function to get the apro_mk_colm data from the Eurostat website
#
get_apro_mt_pwgtm <- function(){
  
  apro_mt_pwgtm <- get_eurostat(id="apro_mt_pwgtm")
  apro_mt_pwgtm <- as_tibble(apro_mt_pwgtm)
  
  # get only the data after 2010
  apro_mt_pwgtm$year <-  as.numeric(format(apro_mt_pwgtm$TIME_PERIOD, format="%Y"))
  apro_mt_pwgtm <- apro_mt_pwgtm %>% filter(year > 2009)
  apro_mt_pwgtm <- apro_mt_pwgtm %>% select(-year)
  
  # convert the format of the dates (we only need years)
  apro_mt_pwgtm$time <-  format(apro_mt_pwgtm$TIME_PERIOD, format="%YM%m")
  
  # drop meat item (always SL)
  apro_mt_pwgtm <-  apro_mt_pwgtm %>% select(-meatitem)
  
  # create a variable name by combining (geo,meat)
  # this will be used in the vlookup's in Excel...
  apro_mt_pwgtm <- apro_mt_pwgtm %>% mutate(varname = str_c(geo,meat,unit, sep = "_"))
  
  #get dictionary
  apro_mt_pwgtm_dic <- get_eurostat_dsd(id = "apro_mt_pwgtm")
  apro_mt_pwgtm_dic <- as_tibble(apro_mt_pwgtm_dic)
  
  mt_pwgtm_meat <- apro_mt_pwgtm_dic %>% filter(concept == "meat")  
  
  # merge to get the labels
  apro_mt_pwgtm <- apro_mt_pwgtm %>% left_join(mt_pwgtm_meat, by = c("meat"="code"))
  apro_mt_pwgtm <- apro_mt_pwgtm %>% mutate(varlabel = str_c(geo,name,unit, sep = "_"))
  
  # select only the columns we need
  apro_mt_pwgtm <- apro_mt_pwgtm %>% select(geo,meat,unit,varlabel,time,values)
  
  # rearrange to have the years in the columns
  colnames(apro_mt_pwgtm)[6] <- "value"
  
  apro_mt_pwgtm <- apro_mt_pwgtm %>% filter(meat %in% c("B1230","B1240"))
  
  return(apro_mt_pwgtm)
  
}

# use the function and get the data
apro_mt_pwgtm <- get_apro_mt_pwgtm()




# write downloaded and processed data to Excel 


# save data extraction also in R data format
# Add time stamp (day) to indicate the date of extraction
save(apro_mt_pwgtm, file = paste(extraction_folder,"apro_mt_pwgtm_", format(Sys.time(), "%Y-%m-%d"), ".RData", sep = ""))



# Note that pivot_wider would put the columns/years in the order of the first appearance
# Therefore, we first sort the table from 1960 to the latest year (increasing order)
#! need a filter on years!! >2010

# load table if not in memory already
#load(paste(extraction_folder,"data/apro_mt_pwgtm_2023-09-18.RData", sep = ""))

# only get the EU countries
EU_contries <- c(
  
                 "AT",
                 "BE",
                 "BG",
                 "CY",
                 "CZ",
                 "DE",
                 "DK",
                 "EE",
                 "EL",
                 "ES",
                 "FI",
                 "FR",
                 "HR",
                 "HU",
                 "IE",
                 "IT",
                 "LT",
                 "LU",
                 "LV",
                 "MT",
                 "NL",
                 "PL",
                 "PT",
                 "RO",
                 "SE",
                 "SI",
                 "SK",
                 "EU27_2007",
                 "EU27_2020"
               
               
)

apro_mt_pwgtm <- apro_mt_pwgtm %>% filter(geo %in% EU_contries)

excel_out <- apro_mt_pwgtm %>% arrange(time) %>% pivot_wider(names_from = time)

write.xlsx(as.data.frame(excel_out), file = paste(extraction_folder,"apro_mt_pwgtm_fromR.xlsx",sep = ""), 
           row.names = FALSE, col.names = TRUE, sheetName = "apro_mt_pwgtm",
           showNA = TRUE)

# add timestamp
timestamp <- format(Sys.time(), "data extracted on %Y.%m.%d-%H:%M:%S")
write.xlsx(timestamp, file = paste(extraction_folder,"apro_mt_pwgtm_fromR.xlsx",sep = ""), 
           row.names = FALSE, col.names = TRUE, sheetName = "timestamp",
           showNA = TRUE, append = TRUE)

# PART II - Check for data updates at Eurostat server
#====================================================

# load the old file... 
load(file = "data/apro_mt_pwgtm_2023-09-12.RData")
old <- apro_mt_pwgtm

# then use the code above: go to Eurostat and grab the latest version
# save the new apro_mt_pwgtm dataset on 'new'
load(file = "data/apro_mt_pwgtm_2023-09-18.RData")
new <- apro_mt_pwgtm

# merge the old and the new versions and check what was updated
x <- new %>% left_join(old, by = c("varlabel","meat","unit","geo","time"))
data_update1 <- x %>% filter(!is.na(value.x)) %>% filter(is.na(value.y))
data_update2 <- x %>% filter(value.x!=value.y)
data_update3 <- x %>% filter(is.na(value.x)) %>% filter(!is.na(value.y))

data_update <- rbind(data_update1, data_update2, data_update3)

# save data update
save(data_update, file = paste("data/apro_mt_pwgtm_update", format(Sys.time(), "%Y-%m-%d"), ".RData", sep = ""))

# clean up
rm(data_update, data_update1, data_update3, data_update2, x, old, new)

