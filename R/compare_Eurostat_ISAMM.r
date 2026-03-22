
library(tidyverse)
library(xlsx)
library(restatapi)
library(jsonlite)

#
# 1. Get monthly deliveries from ISAMM
#

# output folder on U: drive
extraction_folder <- "C:/Users/himicmi/OneDrive - European Commission/GRP-AGRI-A2 - Documents/Market Analysis and Outlook/05. Short Term Market Forecasts/Dairy/eurostat/2026_1/Eurostat download with R/"


# get data from data portal API
json_data <- fromJSON("https://ec.europa.eu/agrifood/api/dairy/production?memberStateCodes=AT,BE,BG,HR,CY,CZ,DK,EE,FI,FR,DE,EL,HU,IE,IT,LV,LT,MT,NL,PL,PT,RO,SK,SI,ES,SE&years=2021,2022,2023,2024,2025,2026")

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
isamm <- df %>% select(memberStateCode, category, time, unit, production) 

# filter for relevant columns
isamm <- isamm %>% select(-category, -unit)



#
# 2. Get apro_mk_colm
#

get_apro_mk_colm <- function(){
  
  apro_mk_colm <- get_eurostat_data(id="apro_mk_colm")
  apro_mk_colm <- as_tibble(apro_mk_colm)
  
  # get only the data after 2010
  apro_mk_colm$year <-  as.numeric(substring(as.character(apro_mk_colm$time), 1, 4))
  apro_mk_colm <- apro_mk_colm %>% filter(year > 2009)
  apro_mk_colm <- apro_mk_colm %>% select(-year)
  
  
  
  # convert the format of the dates (year_M_month)
  apro_mk_colm$time <-  gsub("-","M",apro_mk_colm$time)
  
  
  # create a variable name by combining (geo,dairyprod,milkitem)
  # this will be used in the vlookup's in Excel...
  apro_mk_colm <- apro_mk_colm %>% mutate(varname = str_c(geo,dairyprod,unit,milkitem, sep = "_"))
  
  #get dictionary
  apro_mk_colm_dic <- get_eurostat_dsd(id = "apro_mk_colm")
  apro_mk_colm_dic <- as_tibble(apro_mk_colm_dic)
  
  mk_colm_dairyprod <- apro_mk_colm_dic %>% filter(concept == "dairyprod")
  
  # merge to get the labels
  apro_mk_colm <- apro_mk_colm %>% left_join(mk_colm_dairyprod, by = c("dairyprod"="code"))
  apro_mk_colm <- apro_mk_colm %>% mutate(varlabel = str_c(geo,name, sep = "_"))
  
  # merge unit and milkitem (e.g. to get PC_FAT from PC and FAT)
  apro_mk_colm <- apro_mk_colm %>% mutate(unit=str_c(unit,milkitem, sep = "_"))
  
  # select only the columns we need
  apro_mk_colm <- apro_mk_colm %>% select(varname,varlabel,dairyprod,unit,geo,time,values)
  
  # rearrange to have the years in the columns
  colnames(apro_mk_colm)[7] <- "value"
  
  
  return(apro_mk_colm)
  
}


# use the function and get the data
apro_mk_colm <- get_apro_mk_colm()

# filter for milk deliveries in product weight
deliveries_eurostat <- apro_mk_colm %>% filter(dairyprod=="D1110D",unit=="THS_T_PRD")
deliveries_eurostat <- deliveries_eurostat %>% select(-varname,-varlabel,-unit,-dairyprod)


#
# 3. merge and compare
#

rm(apro_mk_colm, df)

compare <- isamm %>% left_join(deliveries_eurostat, by = join_by(memberStateCode==geo, time==time))
colnames(compare)[4] <- "eurostat"
colnames(compare)[3] <- "isamm"

compare <- compare %>% mutate(diff=abs(eurostat - isamm))
mydiffs <- compare %>% filter(abs(diff)>0)
excel_out <- mydiffs %>% pivot_wider(names_from = time, values_from = diff)

# to excel
write.xlsx(as.data.frame(excel_out), file = paste(extraction_folder,"compare_isamm_eurostat.xlsx",sep = ""), 
           row.names = FALSE, col.names = TRUE, sheetName = "dataportal",
           showNA = TRUE)

