# get data from agrifood data portal of DG AGRI via JSON

library(jsonlite)
library(tidyverse)
library(xlsx)

# output folder on U: drive
extraction_folder <- "U:/4-Market Analysis/4-2 Short-Term Outlook/Outlook Dairy/Short term dairy/2026_1/Eurostat download with R/"

# local output folder
extraction_folder <- "c:/Users/himicmi/Downloads/eurostat/Eurostat download with R/"

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

# calculate year on year and monthly changes
df <- df %>% mutate(time=str_c(year,"-",month, sep="")) %>% mutate(date = lubridate::ymd(paste0(time, '-01')))
df$month_numeric <- as.numeric(df$month)

# year on year monthly change
df <- df %>% arrange(memberStateCode,-year,-month_numeric) %>% mutate(prod_yoy = (production-lead(production,12))/abs(lead(production,12))*100) 

# cumulative change from January until given month - y.o.y
df <- df %>% group_by(memberStateCode,year) %>% arrange(year,month_numeric) %>% mutate(prod_cum_month=cumsum(production))
df <- ungroup(df)
df <- df %>% arrange(memberStateCode,-year,-month_numeric) %>% 
  mutate(prod_cum_yoy = (prod_cum_month-lead(prod_cum_month,12))/abs(lead(prod_cum_month,12))*100) %>% view()


# Formatting header of columns
# create a single time column combining year and month
  df <- df %>% mutate(time=str_c(year,"M",month, sep=""))

# rip table to statistics on milk deliveries, fat content, and protein content
deliver <- df %>% select(memberStateCode, category, time, unit, production) 
deliver_yoy <- df %>% select(memberStateCode, category, time, unit, prod_yoy)
deliver_cum <- df %>% select(memberStateCode, category, time, unit, prod_cum_yoy)

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


# 1 - b.
# deliveries, year on year monthly changes (%)
colnames(deliver_yoy)[5] <- "value"

excel_out <- deliver_yoy %>% arrange(time) %>% pivot_wider(names_from = time)

write.xlsx(as.data.frame(excel_out), file = paste(extraction_folder,"portal_deliveries_yoy_fromR.xlsx",sep = ""), 
           row.names = FALSE, col.names = TRUE, sheetName = "deliveries_yoy",
           showNA = TRUE)

# add timestamp
timestamp <- format(Sys.time(), "data extracted on %Y.%m.%d-%H:%M:%S")
write.xlsx(timestamp, file = paste(extraction_folder,"portal_deliveries_yoy_fromR.xlsx",sep = ""), 
           row.names = FALSE, col.names = TRUE, sheetName = "timestamp",
           showNA = TRUE, append = TRUE)

# save data extraction also in R data format
# Add time stamp (day) to indicate the date of extraction
save(deliver_yoy, file = paste(extraction_folder,"deliver_yoy", format(Sys.time(), "%Y-%m-%d"), ".RData", sep = ""))

# 1 - c.
# deliveries, cumulative monthly changes (Jan. to current month), year on year (%)
colnames(deliver_cum)[5] <- "value"

excel_out <- deliver_cum %>% arrange(time) %>% pivot_wider(names_from = time)

write.xlsx(as.data.frame(excel_out), file = paste(extraction_folder,"portal_deliveries_cum_fromR.xlsx",sep = ""), 
           row.names = FALSE, col.names = TRUE, sheetName = "deliveries_cum",
           showNA = TRUE)

# add timestamp
timestamp <- format(Sys.time(), "data extracted on %Y.%m.%d-%H:%M:%S")
write.xlsx(timestamp, file = paste(extraction_folder,"portal_deliveries_cum_fromR.xlsx",sep = ""), 
           row.names = FALSE, col.names = TRUE, sheetName = "timestamp",
           showNA = TRUE, append = TRUE)

# save data extraction also in R data format
# Add time stamp (day) to indicate the date of extraction
save(deliver_cum, file = paste(extraction_folder,"deliver_cum", format(Sys.time(), "%Y-%m-%d"), ".RData", sep = ""))



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

#--------------------------
# 4. Milk solids evolution
#--------------------------


json_data <- fromJSON("https://ec.europa.eu/agrifood/api/dairy/production?memberStateCodes=AT,BE,BG,HR,CY,CZ,DK,EE,FI,FR,DE,EL,HU,IE,IT,LV,LT,MT,NL,PL,PT,RO,SK,SI,ES,SE&years=2021,2022,2023,2024,2025")

df <- as_tibble(json_data)

# filter to data on raw milk (exclude dairy products)
df <- df %>% filter(category == "Total raw cow's milk delivered to dairies")

# calculate fat/protein  availability in total
evolution <- df %>% group_by(year,memberStateCode) %>% 
  summarise(protein_mean=weighted.mean(protein,production,na.rm=TRUE), 
            fat_mean=weighted.mean(fat,production,na.rm=TRUE))

# to excel
write.xlsx(as.data.frame(evolution), file = paste(extraction_folder,"milk_solids_evolution_fromR.xlsx",sep = ""), 
           row.names = FALSE, col.names = TRUE, sheetName = "dataportal",
           showNA = TRUE)

# save data extraction also in R data format
# Add time stamp (day) to indicate the date of extraction
save(evolution, file = paste(extraction_folder,"milk_solids_", format(Sys.time(), "%Y-%m-%d"), ".RData", sep = ""))



#--------------------------
# 5. Annual milk deliveries
#--------------------------
json_data <- fromJSON("https://ec.europa.eu/agrifood/api/dairy/production?memberStateCodes=AT,BE,BG,HR,CY,CZ,DK,EE,FI,FR,DE,EL,HU,IE,IT,LV,LT,MT,NL,PL,PT,RO,SK,SI,ES,SE&years=2021,2022,2023,2024,2025")

df <- as_tibble(json_data)

# filter to data on raw milk (exclude dairy products)
df <- df %>% filter(category == "Total raw cow's milk delivered to dairies")

isamm_annual <- df %>% group_by(year,memberStateCode) %>% summarise(annual_production=sum(production), nobs=n())
isamm_annual <- isamm_annual %>% filter(nobs==12)
isamm_annual <- isamm_annual %>% select(-nobs)

isamm_annual <- isamm_annual %>% pivot_wider(names_from = year, values_from = annual_production)

# to excel
write.xlsx(as.data.frame(isamm_annual), file = paste(extraction_folder,"isamm_annual_fromR.xlsx",sep = ""), 
           row.names = FALSE, col.names = TRUE, sheetName = "dataportal",
           showNA = TRUE)

# save data extraction also in R data format
# Add time stamp (day) to indicate the date of extraction
save(isamm_annual, file = paste(extraction_folder,"isamm_annual_", format(Sys.time(), "%Y-%m-%d"), ".RData", sep = ""))
