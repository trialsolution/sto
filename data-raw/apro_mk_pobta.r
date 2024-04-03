# get EUROSTAT data on the dairy herd

library(eurostat)
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



apro_mk_pobta <- get_eurostat(id="apro_mk_pobta")
apro_mk_pobta <- as_tibble(apro_mk_pobta)

# convert the format of the dates (we only need years)
apro_mk_pobta$time <-  as.numeric(format(apro_mk_pobta$TIME_PERIOD, format="%Y"))

# create a variable name by combining (geo,dairyprod,milkitem)
# this will be used in the vlookup's in Excel...
apro_mk_pobta <- apro_mk_pobta %>% mutate(varname = str_c(geo,dairyprod,milkitem, sep = "_"))

#get dictionary
apro_mk_pobta_dic <- get_eurostat_dsd(id = "apro_mk_pobta")
apro_mk_pobta_dic <- as_tibble(apro_mk_pobta_dic)

dairyprod <- apro_mk_pobta_dic %>% filter(concept == "dairyprod")



# merge to get the labels
apro_mk_pobta <- apro_mk_pobta %>% left_join(dairyprod, by = c("dairyprod"="code"))
apro_mk_pobta <- apro_mk_pobta %>% mutate(varlabel = str_c(geo,name,milkitem, sep = "_"))

# select only the columns we need
apro_mk_pobta <- apro_mk_pobta %>% select(varname,varlabel,dairyprod,milkitem,geo,time,values)

# rearrange to have the years in the columns
colnames(apro_mk_pobta)[7] <- "value"

# Note that pivot_wider would put the columns/years in the order of the first appearance
# Therefore, we first sort the table from 1960 to the latest year (increasing order)
excel_out <- apro_mk_pobta %>% filter(time > 1989) %>% arrange(time) %>% pivot_wider(names_from = time)


# write to Excel file for further processing
# write.xlsx2 would be faster, but we could not keep the missing values (NA) in the Excel file
# so the preferred option is to use write.xlsx, even if that's slower
#write.xlsx2(as.data.frame(excel_out), file = "apro_mk_pobta_fromR.xlsx", row.names = FALSE, col.names = TRUE, sheetName = "apro_mk_pobta")

# write to Excel 
write.xlsx(as.data.frame(excel_out), file = "apro_mk_pobta_fromR.xlsx", 
            row.names = FALSE, col.names = TRUE, sheetName = "apro_mk_pobta",
            showNA = TRUE)

# add timestamp
timestamp <- format(Sys.time(), "data extracted on %Y.%m.%d-%H:%M:%S")
write.xlsx(timestamp, file = "apro_mk_pobta_fromR.xlsx", 
           row.names = FALSE, col.names = TRUE, sheetName = "timestamp",
           showNA = TRUE, append = TRUE)

# save data extraction also in R data format
save(apro_mk_pobta, file = "data/apro_mk_pobta.RData")
save(apro_mk_pobta_dic, file = "data/apro_mk_pobta_dic.RData")
