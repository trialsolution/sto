# get data from agrifood data portal of DG AGRI via JSON

library(jsonlite)
library(tidyverse)
library(xlsx)

# output folder on U: drive
extraction_folder <- "U:/4-Market Analysis/4-2 Short-Term Outlook/Outlook Dairy/Short term dairy/2025_1/Eurostat download with R/"


# get data from data portal API
json_data <- fromJSON("https://ec.europa.eu/agrifood/api/dairy/production?memberStateCodes=AT,BE,BG,HR,CY,CZ,DK,EE,FI,FR,DE,EL,HU,IE,IT,LV,LT,MT,NL,PL,PT,RO,SK,SI,ES,SE&years=2021,2022,2023,2024,2025")

df <- as_tibble(json_data)

# filter to data on raw milk (exclude dairy products)
df <- df %>% filter(category == "Total raw cow's milk delivered to dairies")


# rename one-digit month to start with zero
# this allows a good order of the months afterwards
for(i in 1:9){
  df$month[df$month == i] <- str_c("0",i,sep="")
} 

# create a single time column combining year and month
df <- df %>% mutate(time=str_c(year,"M",month, sep=""))

# rip table to statistics on milk deliveries, fat content, and protein content
deliver <- df %>% select(memberStateCode, category, time, unit, production) 
fat <- df %>% select(memberStateCode, category, time, fatUnit, fat) 
protein <- df %>% select(memberStateCode, category, time, proteinUnit, protein) 


# write to Excel
#===============

#-------------------
# 1. Milk deliveries
#-------------------


colnames(deliver)[5] <- "value"

excel_out <- deliver %>% arrange(time) %>% pivot_wider(names_from = time)

write.xlsx(as.data.frame(excel_out), file = paste(extraction_folder,"portal_deliveries_fromR.xlsx",sep = ""), 
           row.names = FALSE, col.names = TRUE, sheetName = "deliveries",
           showNA = TRUE)

# add timestamp
timestamp <- format(Sys.time(), "data extracted on %Y.%m.%d-%H:%M:%S")
write.xlsx(timestamp, file = paste(extraction_folder,"portal_deliveries_fromR.xlsx",sep = ""), 
           row.names = FALSE, col.names = TRUE, sheetName = "timestamp",
           showNA = TRUE, append = TRUE)

# save data extraction also in R data format
# Add time stamp (day) to indicate the date of extraction
save(deliver, file = paste(extraction_folder,"deliver_", format(Sys.time(), "%Y-%m-%d"), ".RData", sep = ""))


#-----------------
# 2. Fat content
#-----------------

colnames(fat)[5] <- "value"

excel_out <- fat %>% arrange(time) %>% pivot_wider(names_from = time)

write.xlsx(as.data.frame(excel_out), file = paste(extraction_folder,"portal_fat_fromR.xlsx",sep = ""), 
           row.names = FALSE, col.names = TRUE, sheetName = "fat",
           showNA = TRUE)

# add timestamp
timestamp <- format(Sys.time(), "data extracted on %Y.%m.%d-%H:%M:%S")
write.xlsx(timestamp, file = paste(extraction_folder,"portal_fat_fromR.xlsx",sep = ""), 
           row.names = FALSE, col.names = TRUE, sheetName = "timestamp",
           showNA = TRUE, append = TRUE)

# save data extraction also in R data format
# Add time stamp (day) to indicate the date of extraction
save(fat, file = paste(extraction_folder,"fat_", format(Sys.time(), "%Y-%m-%d"), ".RData", sep = ""))

#-----------------
# 3. Protein content
#-----------------

colnames(protein)[5] <- "value"

excel_out <- protein %>% arrange(time) %>% pivot_wider(names_from = time)

write.xlsx(as.data.frame(excel_out), file = paste(extraction_folder,"portal_protein_fromR.xlsx",sep = ""), 
           row.names = FALSE, col.names = TRUE, sheetName = "protein",
           showNA = TRUE)

# add timestamp
timestamp <- format(Sys.time(), "data extracted on %Y.%m.%d-%H:%M:%S")
write.xlsx(timestamp, file = paste(extraction_folder,"portal_protein_fromR.xlsx",sep = ""), 
           row.names = FALSE, col.names = TRUE, sheetName = "timestamp",
           showNA = TRUE, append = TRUE)

# save data extraction also in R data format
# Add time stamp (day) to indicate the date of extraction
save(protein, file = paste(extraction_folder,"protein_", format(Sys.time(), "%Y-%m-%d"), ".RData", sep = ""))




