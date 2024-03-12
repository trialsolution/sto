# get EUROSTAT organic data on dairy 
# This is data input for the file U:\4-Market Analysis\4-2 Short-Term Outlook\Outlook Dairy\Short term dairy\2021_1\ad-hoc\MMO-organic\2021_3_organic.xlsx


library(eurostat)
library(tidyverse)
library(xlsx)

org_lstspec <- get_eurostat(id="org_lstspec")
org_lstspec <- as_tibble(org_lstspec)

# convert the format of the dates (we only need years)
org_lstspec$time <-  as.numeric(format(org_lstspec$time, format="%Y"))

# create a variable name by combining (geo,dairyprod,milkitem)
# this will be used in the vlookup's in Excel...
org_lstspec <- org_lstspec %>% mutate(varname = str_c(geo,animals,unit, sep = "_"))

# read the labels from agriprod.xls
agriprod <- read.xlsx2(file = "U:/4-Market Analysis/4-2 Short-Term Outlook/Outlook Dairy/Short term dairy/Database/EUROSTAT monthly/new/agriprod.xls", 
                       sheetName = "agriprod", colIndex = 2:4, startRow = 4, header = TRUE)

# merge to get the labels
org_lstspec <- org_lstspec %>% left_join(agriprod, by = c("animals"="ANIMALS"))
org_lstspec <- org_lstspec %>% mutate(varlabel = str_c(geo,Live.animals,unit, sep = "_"))

# select only the columns we need
org_lstspec <- org_lstspec %>% select(varname,varlabel,animals,unit,geo,time,values)

# rearrange to have the years in the columns
colnames(org_lstspec)[7] <- "value"

# Note that pivot_wider would put the columns/years in the order of the first appearance
# Therefore, we first sort the table from 1960 to the latest year (increasing order)
excel_out <- org_lstspec %>% filter(time > 1989) %>% arrange(time) %>% pivot_wider(names_from = time)

# write to Excel file for further processing
# write.xlsx2 would be faster, but we could not keep the missing values (NA) in the Excel file
# so the preferred option is to use write.xlsx, even if that's slower
#write.xlsx2(as.data.frame(excel_out), file = "org_lstspec_fromR.xlsx", row.names = FALSE, col.names = TRUE, sheetName = "org_lstspec")

# write to Excel 
write.xlsx(as.data.frame(excel_out), file = "org_lstspec_fromR.xlsx", 
           row.names = FALSE, col.names = TRUE, sheetName = "org_lstspec",
           showNA = TRUE)

# add timestamp
timestamp <- format(Sys.time(), "data extracted on %Y.%m.%d-%H:%M:%S")
write.xlsx(timestamp, file = "org_lstspec_fromR.xlsx", 
           row.names = FALSE, col.names = TRUE, sheetName = "timestamp",
           showNA = TRUE, append = TRUE)

# save data extraction also in R data format
save(org_lstspec, file = "data/org_lstspec.RData")
