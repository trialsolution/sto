# get EUROSTAT data on the dairy herd

library(tidyverse)
library(xlsx)
library(restatapi)

#
# Note that some 'milkitem' can be negative
#
# From EUROSTAT Metadata:
# "Use of raw milk is followed through production of its two main components, fat and protein content. 
#  Milk processed is thus accounted for as an aggregate of UWM (utilised whole milk, with the full content of fat and proteins) 
#  and USM (utilised skimmed milk, with the full content of proteins,without fat). 
#  As a process can produce skimmed milk further to the main (fat) product and, in such a case, USM can be negative. 
#  This is especially the case for cream and butter production."
#
# In our case it means that some dairy products yield skimmed milk, so the utilized skimmed milk is reported as a negative number
#

# output folder on U: drive
#
# create folder if it does not exist (only have to be done once per STO exercise)
#
main_STO_dir <- "U:/4-Market Analysis/4-2 Short-Term Outlook/Outlook Dairy/Short term dairy"
sub_STO_dir <- "2026_1"
dir.create(file.path(main_STO_dir, sub_STO_dir))

main_STO_dir <- "U:/4-Market Analysis/4-2 Short-Term Outlook/Outlook Dairy/Short term dairy/2026_1"
sub_STO_dir <- "Eurostat download with R"
dir.create(file.path(main_STO_dir, sub_STO_dir))


extraction_folder <- paste(main_STO_dir,"/",sub_STO_dir, sep = "")

#
# get the data
#
apro_mk_pobta <- get_eurostat_data(id="apro_mk_pobta")
apro_mk_pobta <- as_tibble(apro_mk_pobta)

#
# get the date of last update
# this will define the name of the output file to track changes of the EUROSTAT versions
#
s_update <- search_eurostat_toc("milk") |> filter(code == "apro_mk_pobta")
s_update$lastUpdate

#
# convert the format of the columns (from factors to other data types)
#
apro_mk_pobta$time <-  as.numeric(levels(apro_mk_pobta$time))[apro_mk_pobta$time]
apro_mk_pobta$dairyprod <-  as.character(apro_mk_pobta$dairyprod)
apro_mk_pobta$milkitem <-  as.character(apro_mk_pobta$milkitem)

#
# create a variable name by combining (geo,dairyprod,milkitem)
# this will be used in the vlookup's in Excel...
#
apro_mk_pobta <- apro_mk_pobta %>% mutate(varname = str_c(geo,dairyprod,milkitem, sep = "_"))

#
# get the dictionary from EUROSTAT with the codes
#
apro_mk_pobta_dic <- get_eurostat_dsd(id = "apro_mk_pobta")
apro_mk_pobta_dic <- as_tibble(apro_mk_pobta_dic)

dairyprod <- apro_mk_pobta_dic %>% filter(concept == "dairyprod")

#
# merge the dictionary with the data 
# then create a long variable name used in the filters of the Excel files
#
apro_mk_pobta <- apro_mk_pobta %>% left_join(dairyprod, by = c("dairyprod"="code"))
apro_mk_pobta <- apro_mk_pobta %>% mutate(varlabel = str_c(geo,name,milkitem, sep = "_"))

#
# select only the columns we need
# rearrange to have the years in the columns
#
apro_mk_pobta <- apro_mk_pobta %>% select(varname,varlabel,dairyprod,milkitem,geo,time,values)
colnames(apro_mk_pobta)[7] <- "value"

#
# Note that pivot_wider would put the columns/years in the order of the first appearance
# Therefore, we first sort the table from 1960 to the latest year (increasing order)
#
excel_out <- apro_mk_pobta %>% filter(time > 1989) %>% arrange(time) %>% pivot_wider(names_from = time)

#
# create an Excel file for further processing
# write.xlsx2 would be faster, but we could not keep the missing values (NA) in the Excel file
# so the preferred option is to use write.xlsx, even if that's slower
#write.xlsx2(as.data.frame(excel_out), file = "apro_mk_pobta_fromR.xlsx", row.names = FALSE, col.names = TRUE, sheetName = "apro_mk_pobta")

# write to Excel 
write.xlsx(as.data.frame(excel_out), file = paste(extraction_folder,"/apro_mk_pobta_fromR.xlsx",sep = ""), 
            row.names = FALSE, col.names = TRUE, sheetName = "apro_mk_pobta",
            showNA = TRUE)

# add date of last data update on Eurostat
write.xlsx(s_update$lastUpdate, file = paste(extraction_folder,"/apro_mk_pobta_fromR.xlsx",sep = ""), 
           row.names = FALSE, col.names = TRUE, sheetName = "releasedate",
           showNA = TRUE, append = TRUE)


# save data extraction also in R data format
save(apro_mk_pobta, file = paste(extraction_folder,"/apro_mk_pobta_", 
                                 as.character(s_update$lastUpdate), ".RData", sep = ""))
save(apro_mk_pobta_dic, file = paste(extraction_folder,"/apro_mk_pobta_dic_", 
                                     as.character(s_update$lastUpdate), ".RData", sep = ""))



# PART II - Check for data updates at Eurostat server
#====================================================

rm(apro_mk_pobta)

# load the old file... 
load(file = paste("U:/4-Market Analysis/4-2 Short-Term Outlook/Outlook Dairy/Short term dairy/2025_2/Eurostat download with R/",
                  "apro_mk_pobta_2025-09-03.RData",sep = ""))
old <- apro_mk_pobta
rm(apro_mk_pobta)

# save the new apro_mk_pobta dataset on 'new'
load(file = paste(extraction_folder,"/apro_mk_pobta_2025.10.07.RData",sep = ""))
new <- apro_mk_pobta
rm(apro_mk_pobta)

#
# merge the old and the new versions and check what was updated
#
x <- new %>% left_join(old, by = c("varname","varlabel","dairyprod","milkitem","geo","time"))

# new data added since last release
data_update1 <- x %>% filter(!is.na(value.x)) %>% filter(is.na(value.y))
# updated data points
data_update2 <- x %>% filter(value.x!=value.y)
# data disappeared since last release
data_update3 <- x %>% filter(is.na(value.x)) %>% filter(!is.na(value.y))
# all together
data_update <- rbind(data_update1, data_update2, data_update3)

# give meaningful variable names
colnames(data_update)[7] <- "value_new"
colnames(data_update)[8] <- "value_old"


# save data update
save(data_update, file = paste(extraction_folder,"/apro_mk_pobta_update", format(Sys.time(), "%Y-%m-%d"), ".RData", sep = ""))

# clean up
rm(data_update, data_update1, data_update3, data_update2, x, old, new)

