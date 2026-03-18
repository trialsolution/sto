# get EUROSTAT data on the dairy herd

library(tidyverse)
library(xlsx)
library(restatapi)

# output folder on U: drive
extraction_folder <- "U:/4-Market Analysis/4-2 Short-Term Outlook/Outlook Dairy/Short term dairy/2025_1/Eurostat download with R/"

# option B: use a local folder
extraction_folder <- "c:/Users/himicmi/Downloads/eurostat/2026_1/Eurostat download with R/"



# DOWNLOAD DATA
#--------------

apro_mt_lscatl <- get_eurostat_data(id="apro_mt_lscatl")
apro_mt_lscatl <- as_tibble(apro_mt_lscatl)


#
# get the date of last update
# this will define the name of the output file to track changes of the EUROSTAT versions
#
s_update <- search_eurostat_toc("Bovine") |> filter(code == "apro_mt_lscatl")
s_update$lastUpdate

# PROCESS AND REARRANGE DATA
#---------------------------

# convert the format of the dates (we only need years)
apro_mt_lscatl$time <- as.numeric(levels(apro_mt_lscatl$time))[apro_mt_lscatl$time]
apro_mt_lscatl$animals <-  as.character(apro_mt_lscatl$animals)
apro_mt_lscatl$month <-  as.character(apro_mt_lscatl$month)
apro_mt_lscatl$geo <-  as.character(apro_mt_lscatl$geo)


# create a variable name by combining (geo,animals,month)
# month here refers to the month of the survey: either May-June or December
# this will be used in the vlookup's in Excel...
apro_mt_lscatl <- apro_mt_lscatl %>% mutate(varname = str_c(geo,animals,month, sep = "_"))


# Add labels to the combined column

# Get the dictionary with the restatapi package
apro_mt_lscatl_dic <- get_eurostat_dsd(id = "apro_mt_lscatl")
apro_mt_lscatl_dic <- as_tibble(apro_mt_lscatl_dic)

animals <- apro_mt_lscatl_dic %>% filter(concept == "animals")
 
# The old Eurostat dictionary file is here:
# U:\4-Market Analysis\4-2 Short-Term Outlook\Outlook Dairy\Short term dairy\Database\EUROSTAT Annual\[EUROSTAT_dictionary.xlsx


# merge to get the labels
apro_mt_lscatl <- apro_mt_lscatl %>% left_join(animals, by = c("animals"="code"))
# check if we have labels not found on the list
apro_mt_lscatl %>% filter(is.na(name))
# create combined variable with long labels
apro_mt_lscatl <- apro_mt_lscatl %>% mutate(varlabel = str_c(geo,name,unit, sep = "_"))


# select only the columns we need
apro_mt_lscatl <- apro_mt_lscatl %>% select(varname,varlabel,animals,month,unit,geo,time,values)

# rearrange to have the years in the columns
colnames(apro_mt_lscatl)[8] <- "value"



# WRITE OUT TO EXCEL
#-------------------

apro_mt_lscatl <- apro_mt_lscatl %>% filter(time > 1989)

source("R/save_to_excel.r")

save_to_excel(tibble_to_save = apro_mt_lscatl, folder_to_save = extraction_folder)


# save data extraction also in R data format
save(apro_mt_lscatl_dic, file = paste(extraction_folder, "apro_mt_lscatl_dic_", as.character(s_update$lastUpdate), ".RData", sep = ""))


# PART II - Check for data updates at Eurostat server
#====================================================

rm(apro_mt_lscatl)

# load the old file... 
#load(file = paste(extraction_folder,"apro_mt_lscatl_2024-07-02.RData",sep = ""))
load(file = paste("C:/Users/himicmi/Downloads/eurostat/data/",
                  "apro_mt_lscatl.RData",sep = ""))
old <- apro_mt_lscatl
rm(apro_mt_lscatl)

# save the new apro_mt_lscatl dataset on 'new'
load(file = paste(extraction_folder,"apro_mt_lscatl_2025-06-08.RData",sep = ""))
new <- apro_mt_lscatl
rm(apro_mt_lscatl)

# merge the old and the new versions and check what was updated
x <- new %>% left_join(old, by = c("varname","varlabel","animals","month","geo","time"))
data_update1 <- x %>% filter(!is.na(value.x)) %>% filter(is.na(value.y))
data_update2 <- x %>% filter(value.x!=value.y)
data_update3 <- x %>% filter(is.na(value.x)) %>% filter(!is.na(value.y))

data_update <- rbind(data_update1, data_update2, data_update3)

# give meaningful variable names
colnames(data_update)[7] <- "value_new"
colnames(data_update)[8] <- "value_old"


# save data update
save(data_update, file = paste(extraction_folder,"apro_mt_lscatl_update", format(Sys.time(), "%Y-%m-%d"), ".RData", sep = ""))

# clean up
rm(data_update, data_update1, data_update3, data_update2, x, old, new)



