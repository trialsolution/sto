# get EUROSTAT data on the dairy herd

library(eurostat)
library(tidyverse)
library(xlsx)


# output folder on U: drive
extraction_folder <- "U:/4-Market Analysis/4-2 Short-Term Outlook/Outlook Dairy/Short term dairy/2025_1/Eurostat download with R/"



# DOWNLOAD DATA
#--------------

apro_mk_farm <- get_eurostat(id="apro_mk_farm")
apro_mk_farm <- as_tibble(apro_mk_farm)


# PROCESS AND REARRANGE DATA
#---------------------------

# convert the format of the dates (we only need years)
apro_mk_farm$TIME_PERIOD <-  as.numeric(format(apro_mk_farm$TIME_PERIOD, format="%Y"))

# create a variable name by combining (geo,dairyprod,milkitem)
# this will be used in the vlookup's in Excel...
apro_mk_farm <- apro_mk_farm %>% mutate(varname = str_c(geo,dairyprod,milkitem, sep = "_"))


# Add labels to the combined column
# read the labels from agriprod.xls
agriprod <- read.xlsx2(file = "U:/4-Market Analysis/4-2 Short-Term Outlook/Outlook Dairy/Short term dairy/Database/EUROSTAT monthly/new/agriprod.xls", 
                       sheetName = "agriprod", colIndex = 2:4, startRow = 4, header = TRUE)


# merge to get the labels
apro_mk_farm <- apro_mk_farm %>% left_join(agriprod, by = c("dairyprod"="ANIMALS"))
# check if we have labels not found on the list
apro_mk_farm %>% filter(is.na(Live.animals))
# create combined variable with long labels
apro_mk_farm <- apro_mk_farm %>% mutate(varlabel = str_c(geo,Live.animals,milkitem, sep = "_"))

# select only the columns we need
apro_mk_farm <- apro_mk_farm %>% select(varname,varlabel,dairyprod,milkitem,geo,TIME_PERIOD,values)

# rearrange to have the years in the columns
colnames(apro_mk_farm)[7] <- "value"
colnames(apro_mk_farm)[6] <- "time"


# WRITE OUT TO EXCEL
#-------------------

# Note that pivot_wider would put the columns/years in the order of the first appearance
# Therefore, we first sort the table from 1960 to the latest year (increasing order)
excel_out <- apro_mk_farm %>% filter(time > 1989) %>% arrange(time) %>% pivot_wider(names_from = time)


# write to Excel file for further processing
# write.xlsx2 would be faster, but we could not keep the missing values (NA) in the Excel file
# so the preferred option is to use write.xlsx, even if that's slower
#write.xlsx2(as.data.frame(excel_out), file = "apro_mk_farm_fromR.xlsx", row.names = FALSE, col.names = TRUE, sheetName = "apro_mk_farm")

# write to Excel 
write.xlsx(as.data.frame(excel_out), file = paste(extraction_folder,"apro_mk_farm_fromR.xlsx", sep = ""), 
           row.names = FALSE, col.names = TRUE, sheetName = "apro_mk_farm",
           showNA = TRUE)

# add timestamp
timestamp <- format(Sys.time(), "data extracted on %Y.%m.%d-%H:%M:%S")
write.xlsx(timestamp, file = paste(extraction_folder,"apro_mk_farm_fromR.xlsx", sep = ""), 
           row.names = FALSE, col.names = TRUE, sheetName = "timestamp",
           showNA = TRUE, append = TRUE)

# save data extraction also in R data format
save(apro_mk_farm, file = paste(extraction_folder,"apro_mk_farm_", format(Sys.time(), "%Y-%m-%d"), ".RData", sep = ""))
save(agriprod, file = paste(extraction_folder,"agriprod_", format(Sys.time(), "%Y-%m-%d"), ".RData", sep = ""))


# PART II - Check for data updates at Eurostat server
#====================================================

rm(apro_mk_farm)

# load the old file... 
#load(file = paste(extraction_folder,"apro_mk_farm_2024-07-02.RData",sep = ""))
load(file = paste("C:/Users/himicmi/Downloads/eurostat/data/",
                  "apro_mk_farm.RData",sep = ""))
old <- apro_mk_farm
rm(apro_mk_farm)

# save the new apro_mk_farm dataset on 'new'
load(file = paste(extraction_folder,"apro_mk_farm_2025-06-08.RData",sep = ""))
new <- apro_mk_farm
rm(apro_mk_farm)

# merge the old and the new versions and check what was updated
x <- new %>% left_join(old, by = c("varname","varlabel","dairyprod","milkitem","geo","time"))
data_update1 <- x %>% filter(!is.na(value.x)) %>% filter(is.na(value.y))
data_update2 <- x %>% filter(value.x!=value.y)
data_update3 <- x %>% filter(is.na(value.x)) %>% filter(!is.na(value.y))

data_update <- rbind(data_update1, data_update2, data_update3)

# give meaningful variable names
colnames(data_update)[7] <- "value_new"
colnames(data_update)[8] <- "value_old"


# save data update
save(data_update, file = paste(extraction_folder,"apro_mk_farm_update", format(Sys.time(), "%Y-%m-%d"), ".RData", sep = ""))

# clean up
rm(data_update, data_update1, data_update3, data_update2, x, old, new)


