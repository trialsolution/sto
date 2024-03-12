# get EUROSTAT organic data on dairy 
# This is data input for the file U:\4-Market Analysis\4-2 Short-Term Outlook\Outlook Dairy\Short term dairy\2021_1\ad-hoc\MMO-organic\2021_3_organic.xlsx


library(eurostat)
library(tidyverse)
library(xlsx)


org_aprod <- get_eurostat(id="org_aprod")
org_aprod <- as_tibble(org_aprod)

# convert the format of the dates (we only need years)
org_aprod$time <-  as.numeric(format(org_aprod$time, format="%Y"))

# create a variable name by combining (geo,dairyprod,milkitem)
# this will be used in the vlookup's in Excel...
org_aprod <- org_aprod %>% mutate(varname = str_c(geo,agriprod,unit, sep = "_"))

# read the labels from agriprod.xls
agriprod <- read.xlsx2(file = "U:/4-Market Analysis/4-2 Short-Term Outlook/Outlook Dairy/Short term dairy/Database/EUROSTAT monthly/new/agriprod.xls", 
                       sheetName = "agriprod", colIndex = 2:4, startRow = 4, header = TRUE)

# merge to get the labels
org_aprod <- org_aprod %>% left_join(agriprod, by = c("agriprod"="ANIMALS"))
org_aprod <- org_aprod %>% mutate(varlabel = str_c(geo,Live.animals,unit, sep = "_"))

# select only the columns we need
org_aprod <- org_aprod %>% select(varname,varlabel,agriprod,unit,geo,time,values)

# rearrange to have the years in the columns
colnames(org_aprod)[7] <- "value"

# Note that pivot_wider would put the columns/years in the order of the first appearance
# Therefore, we first sort the table from 1960 to the latest year (increasing order)
excel_out <- org_aprod %>% filter(time > 1989) %>% arrange(time) %>% pivot_wider(names_from = time)

# write to Excel file for further processing
# write.xlsx2 would be faster, but we could not keep the missing values (NA) in the Excel file
# so the preferred option is to use write.xlsx, even if that's slower
#write.xlsx2(as.data.frame(excel_out), file = "org_aprod_fromR.xlsx", row.names = FALSE, col.names = TRUE, sheetName = "org_aprod")

# write to Excel 
write.xlsx(as.data.frame(excel_out), file = "org_aprod_fromR.xlsx", 
            row.names = FALSE, col.names = TRUE, sheetName = "org_aprod",
            showNA = TRUE)

# add timestamp
timestamp <- format(Sys.time(), "data extracted on %Y.%m.%d-%H:%M:%S")
write.xlsx(timestamp, file = "org_aprod_fromR.xlsx", 
           row.names = FALSE, col.names = TRUE, sheetName = "timestamp",
           showNA = TRUE, append = TRUE)

# save data extraction also in R data format
save(org_aprod, file = "data/org_aprod.RData")

