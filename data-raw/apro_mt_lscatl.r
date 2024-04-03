# get EUROSTAT data on the dairy herd

library(eurostat)
library(tidyverse)
library(xlsx)
library(restatapi)

# DOWNLOAD DATA
#--------------

apro_mt_lscatl <- get_eurostat(id="apro_mt_lscatl")
apro_mt_lscatl <- as_tibble(apro_mt_lscatl)


# PROCESS AND REARRANGE DATA
#---------------------------

# convert the format of the dates (we only need years)
apro_mt_lscatl$TIME_PERIOD <-  as.numeric(format(apro_mt_lscatl$TIME_PERIOD, format="%Y"))

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
apro_mt_lscatl <- apro_mt_lscatl %>% select(varname,varlabel,animals,month,unit,geo,TIME_PERIOD,values)

# rearrange to have the years in the columns
colnames(apro_mt_lscatl)[8] <- "value"
colnames(apro_mt_lscatl)[7] <- "time"



# WRITE OUT TO EXCEL
#-------------------

# Note that pivot_wider would put the columns/years in the order of the first appearance
# Therefore, we first sort the table from 1960 to the latest year (increasing order)
excel_out <- apro_mt_lscatl %>% filter(time > 1989) %>% arrange(time) %>% pivot_wider(names_from = time)


# write to Excel file for further processing
# write.xlsx2 would be faster, but we could not keep the missing values (NA) in the Excel file
# so the preferred option is to use write.xlsx, even if that's slower
#write.xlsx2(as.data.frame(excel_out), file = "apro_mt_lscatl_fromR.xlsx", row.names = FALSE, col.names = TRUE, sheetName = "apro_mt_lscatl")

# write to Excel 
write.xlsx(as.data.frame(excel_out), file = "apro_mt_lscatl_fromR.xlsx", 
           row.names = FALSE, col.names = TRUE, sheetName = "apro_mt_lscatl",
           showNA = TRUE)

# add timestamp
timestamp <- format(Sys.time(), "data extracted on %Y.%m.%d-%H:%M:%S")
write.xlsx(timestamp, file = "apro_mt_lscatl_fromR.xlsx", 
           row.names = FALSE, col.names = TRUE, sheetName = "timestamp",
           showNA = TRUE, append = TRUE)

# save data extraction also in R data format
save(apro_mt_lscatl, file = "data/apro_mt_lscatl.RData")
save(apro_mt_lscatl_dic, file = "data/apro_mt_lscatl_dic.RData")



