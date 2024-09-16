# to process trade data extraction from Beate

library(eurostat)
library(tidyverse)
library(xlsx)
library(restatapi)
library(readxl)

# input and output folder on U: drive
input_folder <- "U:/4-Market Analysis/4-2 Short-Term Outlook/2024/09_September/Dairy/"
extraction_folder <- "U:/4-Market Analysis/4-2 Short-Term Outlook/Outlook Dairy/Short term dairy/2024_2/Eurostat download with R/"
mapping_file <- "U:/4-Market Analysis/4-2 Short-Term Outlook/Outlook Dairy/Short term dairy/Database/Trade/Dairy_trade_from1990_Brexit_GTA.xlsx"


# --- READ TABLES

# read EU imports from extra-EU 
eu27imp_extra <- read_excel(paste(input_folder,"f.xlsx",sep = ""), sheet = "EU27-M", range = "b9:kw55")

# read EU imports from UK
eu27imp_uk <- read_excel(paste(input_folder,"f.xlsx",sep = ""), sheet = "EU27-M", range = "b57:kw103")

# read EU exports to extra-EU 
eu27exp_extra <- read_excel(paste(input_folder,"f.xlsx",sep = ""), sheet = "EU27-M", range = "b105:kw151")

# read EU exports to UK
eu27exp_uk <- read_excel(paste(input_folder,"f.xlsx",sep = ""), sheet = "EU27-M", range = "b153:kw199")


# read mapping
prodmap <- read_excel(mapping_file, sheet = "prodmap", range = "b1:c17")



calculate_annual_trade <- function(input_table,prodmap,minmonth=1,maxmonth=12){
  
  x <- input_table %>% pivot_longer(names_to = "date", values_to = "value", cols = 2:ncol(eu27imp_extra))
  
  # create columns for year and month
  x$year <- str_sub(x$date, 1, 4)
  x$month <- str_sub(x$date, 5, 6)
  
  # filter for specific time interval (months) within a year
  # use if one of the years is incomplete 
  x$month <- as.numeric(x$month)
  x <- x %>% filter(month>=minmonth) %>% filter(month<=maxmonth)

  # calculate annual totals
  y <- x %>% group_by(`PRODUCT/PERIOD`, year) %>% summarize(value=sum(value, na.rm = TRUE)) 
  y <- y %>% pivot_wider(names_from = year)
  
  # link to mapping
  z <- prodmap %>% left_join(y, by = c("extraction"="PRODUCT/PERIOD"))
  
  z <- z %>% select(-extraction)
  z <- z %>% pivot_longer(cols = 2:ncol(z), names_to = "year")
  z <- z %>% group_by(dairy_trade, year) %>% summarise(value = sum(value))
  
  z <- z %>% pivot_wider(names_from = year)
  
  # calculate FDP from the corresponding rows
  # we use an auxiliary subset FDP_z for the calculation
  z <- z %>% filter(dairy_trade != "FDP")
  
  # FDP calculation
  FDP_z <- z %>% filter(dairy_trade %in% c("Drinking milk", "Cream", "Yogurt", "Buttermilk (liquid)"))
  FDP_z <- FDP_z %>% pivot_longer(cols = 2:ncol(FDP_z), names_to = "year")
  FDP_z <- FDP_z %>% group_by(year) %>% summarize(value=sum(value))
  FDP_z$dairy_trade <- "FDP"
  
  FDP_z <- FDP_z %>% pivot_wider(names_from = year)
  
  # bind the two tables by the rows
  z <- z %>% bind_rows(FDP_z)
  
  return(z)
  
}

# calculate annual trade statistics with the above function

annual_eu27imp_extra <- calculate_annual_trade(input_table = eu27imp_extra, prodmap = prodmap)
annual_eu27imp_uk <- calculate_annual_trade(input_table = eu27imp_uk, prodmap = prodmap)

annual_eu27exp_extra <- calculate_annual_trade(input_table = eu27exp_extra, prodmap = prodmap)
annual_eu27exp_uk <- calculate_annual_trade(input_table = eu27exp_uk, prodmap = prodmap)

# calculate annuals Jan-July
annual_eu27imp_extra_JanJul <- calculate_annual_trade(input_table = eu27imp_extra, prodmap = prodmap, minmonth = 1, maxmonth = 7)
annual_eu27imp_uk_JanJul <- calculate_annual_trade(input_table = eu27imp_uk, prodmap = prodmap, minmonth = 1, maxmonth = 7)
annual_eu27exp_extra_JanJul <- calculate_annual_trade(input_table = eu27exp_extra, prodmap = prodmap, minmonth = 1, maxmonth = 7)
annual_eu27exp_uk_JanJul <- calculate_annual_trade(input_table = eu27exp_uk, prodmap = prodmap, minmonth = 1, maxmonth = 7)



# --- ANNUAL CHANGE
# 

# definiton of generic function
# gives back only percentage changes
calculate_pchg <- function(annual_values){

  chg <- annual_values 
  chg <- chg %>% pivot_longer(cols = 2:ncol(chg), names_to = "year")
  chg <- chg %>% mutate(pchg = (value/lag(value)-1)*100)
  chg <- chg %>% select(-value)
  chg <- chg %>% pivot_wider(names_from = year, values_from = pchg)
  
  return(chg)
}


# annual changes Jan-Dec
# watch out that if year is  not complete than the calculation is flawed
# --> always check if last year is complete
pchg_eu27imp_extra <- calculate_pchg(annual_eu27imp_extra)
pchg_eu27imp_uk <- calculate_pchg(annual_eu27imp_uk)
pchg_eu27exp_extra <- calculate_pchg(annual_eu27exp_extra)
pchg_eu27exp_uk <- calculate_pchg(annual_eu27exp_uk)


# annual changes Jan-July
# watch out that if year is  not complete than the calculation is flawed
# --> always check if last year is complete
pchg_eu27imp_extra_JanJul <- calculate_pchg(annual_eu27imp_extra_JanJul)
pchg_eu27imp_uk_JanJul <- calculate_pchg(annual_eu27imp_uk_JanJul)
pchg_eu27exp_extra_JanJul <- calculate_pchg(annual_eu27exp_extra_JanJul)
pchg_eu27exp_uk_JanJul <- calculate_pchg(annual_eu27exp_uk_JanJul)




# --- write downloaded and processed data to Excel 

# -- defines a generic write function

write_trade <- function(mytable,myfolder,myfilename="xyz",mysheetname="xyz",toappend=FALSE){
  
  write.xlsx(as.data.frame(mytable), file = paste(myfolder,myfilename,".xlsx",sep = ""), 
             row.names = FALSE, col.names = TRUE, sheetName = mysheetname,
             showNA = TRUE, append = toappend)
  
}

# -- annual data

write_trade(annual_eu27imp_extra,extraction_folder,myfilename = "dairy_trade",mysheetname = "annual_eu27imp_extra")
write_trade(annual_eu27imp_uk,extraction_folder,myfilename = "dairy_trade",mysheetname = "annual_eu27imp_uk",toappend = TRUE)
write_trade(annual_eu27exp_extra,extraction_folder,myfilename = "dairy_trade",mysheetname = "annual_eu27exp_extra",toappend = TRUE)
write_trade(annual_eu27exp_uk,extraction_folder,myfilename = "dairy_trade",mysheetname = "annual_eu27exp_uk",toappend = TRUE)


write_trade(annual_eu27imp_extra_JanJul,extraction_folder,myfilename = "dairy_trade",mysheetname = "annual_eu27imp_extra_JanJul",toappend = TRUE)
write_trade(annual_eu27imp_uk_JanJul,extraction_folder,myfilename = "dairy_trade",mysheetname = "annual_eu27imp_uk_JanJul",toappend = TRUE)
write_trade(annual_eu27exp_extra_JanJul,extraction_folder,myfilename = "dairy_trade",mysheetname = "annual_eu27exp_extra_JanJul",toappend = TRUE)
write_trade(annual_eu27exp_uk_JanJul,extraction_folder,myfilename = "dairy_trade",mysheetname = "annual_eu27exp_uk_JanJul",toappend = TRUE)


# -- percentage changes

write_trade(pchg_eu27imp_extra_JanJul,extraction_folder,myfilename = "dairy_trade",mysheetname = "pchg_eu27imp_extra_JanJul",toappend = TRUE)
write_trade(pchg_eu27imp_uk_JanJul,extraction_folder,myfilename = "dairy_trade",mysheetname = "pchg_eu27imp_uk_JanJul",toappend = TRUE)
write_trade(pchg_eu27exp_extra_JanJul,extraction_folder,myfilename = "dairy_trade",mysheetname = "pchg_eu27exp_extra_JanJul",toappend = TRUE)
write_trade(pchg_eu27exp_uk_JanJul,extraction_folder,myfilename = "dairy_trade",mysheetname = "pchg_eu27exp_uk_JanJul",toappend = TRUE)

write_trade(pchg_eu27imp_extra,extraction_folder,myfilename = "dairy_trade",mysheetname = "pchg_eu27imp_extra",toappend = TRUE)
write_trade(pchg_eu27imp_uk,extraction_folder,myfilename = "dairy_trade",mysheetname = "pchg_eu27imp_uk",toappend = TRUE)
write_trade(pchg_eu27exp_extra,extraction_folder,myfilename = "dairy_trade",mysheetname = "pchg_eu27exp_extra",toappend = TRUE)
write_trade(pchg_eu27exp_uk,extraction_folder,myfilename = "dairy_trade",mysheetname = "pchg_eu27exp_uk",toappend = TRUE)


# -- add timestamp

timestamp <- format(Sys.time(), "data extracted on %Y.%m.%d-%H:%M:%S")
write.xlsx(timestamp, file = paste(extraction_folder,"dairy_trade.xlsx",sep = ""), 
           row.names = FALSE, col.names = TRUE, sheetName = "timestamp",
           showNA = TRUE, append = TRUE)


# save data extraction also in R data format

save(annual_eu27imp_extra, file = paste(extraction_folder,"annual_eu27imp_extra", format(Sys.time(), "%Y-%m-%d"), ".RData", sep = ""))
save(annual_eu27imp_uk, file = paste(extraction_folder,"annual_eu27imp_uk", format(Sys.time(), "%Y-%m-%d"), ".RData", sep = ""))
save(annual_eu27exp_extra, file = paste(extraction_folder,"annual_eu27exp_extra", format(Sys.time(), "%Y-%m-%d"), ".RData", sep = ""))
save(annual_eu27exp_uk, file = paste(extraction_folder,"annual_eu27exp_uk", format(Sys.time(), "%Y-%m-%d"), ".RData", sep = ""))

save(annual_eu27imp_extra_JanJul, file = paste(extraction_folder,"annual_eu27imp_extra_JanJul", format(Sys.time(), "%Y-%m-%d"), ".RData", sep = ""))
save(annual_eu27imp_uk_JanJul, file = paste(extraction_folder,"annual_eu27imp_uk_JanJul", format(Sys.time(), "%Y-%m-%d"), ".RData", sep = ""))
save(annual_eu27exp_extra_JanJul, file = paste(extraction_folder,"annual_eu27exp_extra_JanJul", format(Sys.time(), "%Y-%m-%d"), ".RData", sep = ""))
save(annual_eu27exp_uk_JanJul, file = paste(extraction_folder,"annual_eu27exp_uk_JanJul", format(Sys.time(), "%Y-%m-%d"), ".RData", sep = ""))

save(pchg_eu27imp_extra, file = paste(extraction_folder,"pchg_eu27imp_extra", format(Sys.time(), "%Y-%m-%d"), ".RData", sep = ""))
save(pchg_eu27imp_uk, file = paste(extraction_folder,"pchg_eu27imp_uk", format(Sys.time(), "%Y-%m-%d"), ".RData", sep = ""))
save(pchg_eu27exp_extra, file = paste(extraction_folder,"pchg_eu27exp_extra", format(Sys.time(), "%Y-%m-%d"), ".RData", sep = ""))
save(pchg_eu27exp_uk, file = paste(extraction_folder,"pchg_eu27exp_uk", format(Sys.time(), "%Y-%m-%d"), ".RData", sep = ""))

save(pchg_eu27imp_extra_JanJul, file = paste(extraction_folder,"pchg_eu27imp_extra_JanJul", format(Sys.time(), "%Y-%m-%d"), ".RData", sep = ""))
save(pchg_eu27imp_uk_JanJul, file = paste(extraction_folder,"pchg_eu27imp_uk_JanJul", format(Sys.time(), "%Y-%m-%d"), ".RData", sep = ""))
save(pchg_eu27exp_extra_JanJul, file = paste(extraction_folder,"pchg_eu27exp_extra_JanJul", format(Sys.time(), "%Y-%m-%d"), ".RData", sep = ""))
save(pchg_eu27exp_uk_JanJul, file = paste(extraction_folder,"pchg_eu27exp_uk_JanJul", format(Sys.time(), "%Y-%m-%d"), ".RData", sep = ""))
