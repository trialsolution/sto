# aggregate monthly data to annual
# needed in case annual data is missing

library(tidyverse)
library(xlsx)
library(restatapi)

# apro_mk_colm (monthly) to apro_mk_pobta (annual)

# data will be picked up from U: drive
extraction_folder <- "C:/Users/himicmi/OneDrive - European Commission/GRP-AGRI-A2 - Documents/Market Analysis and Outlook/05. Short Term Market Forecasts/Dairy/eurostat/2026_1/Eurostat download with R/"

#load("U:/4-Market Analysis/4-2 Short-Term Outlook/Outlook Dairy/Short term dairy/2024_2/Eurostat download with R/apro_mk_colm_2024-08-29.RData")

#
# read data directly from Eurostat website
#
apro_mk_colm <- get_eurostat_data(id="apro_mk_colm")
apro_mk_colm <- as_tibble(apro_mk_colm)

apro_mk_colm$year <-  as.numeric(substring(as.character(apro_mk_colm$time), 1, 4))
apro_mk_colm$month <-  as.numeric(substring(as.character(apro_mk_colm$time), 6, 7))

# sum up only for THS_T unit
apro_mk_colm <- apro_mk_colm %>% filter(unit == "THS_T")

# add up monthly to annual values
apro_mk_colm_annual <- apro_mk_colm %>% group_by(dairyprod, geo, year) %>% summarise(nr_months=n(),annual_values=sum(values))

# Example: see LU cow's milk production
apro_mk_colm_annual %>% filter(dairyprod=="D1110D") %>% filter(geo=="LU") %>% View()


# to report to the Datafortheweb file for 2024 - milk deliveries

countries <- c("BE",
               "BG",
               "CZ",
               "DK",
               "DE",
               "EE",
               "IE",
               "EL",
               "ES",
               "FR",
               "HR",
               "IT",
               "CY",
               "LV",
               "LT",
               "LU",
               "HU",
               "MT",
               "NL",
               "AT",
               "PL",
               "PT",
               "RO",
               "SI",
               "SK",
               "FI",
               "SE")

apro_mk_colm_annual %>% filter(geo %in% countries, year == 2024, dairyprod == "D1110D") %>% view()

# save data update
save(data_update, file = paste(extraction_folder,"apro_mk_colm_annual", format(Sys.time(), "%Y-%m-%d"), ".RData", sep = ""))
