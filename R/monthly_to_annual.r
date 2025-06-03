# aggregate monthly data to annual
# needed in case annual data is missing

library(eurostat)
library(tidyverse)
library(xlsx)
library(restatapi)

# apro_mk_colm (monthly) to apro_mk_pobta (annual)

# data will be picked up from U: drive
extraction_folder <- "U:/4-Market Analysis/4-2 Short-Term Outlook/Outlook Dairy/Short term dairy/2024_2/Eurostat download with R/"

#load("U:/4-Market Analysis/4-2 Short-Term Outlook/Outlook Dairy/Short term dairy/2024_2/Eurostat download with R/apro_mk_colm_2024-08-29.RData")


# read data directly from Eurostat website
apro_mk_colm <- get_eurostat(id="apro_mk_colm")
apro_mk_colm <- as_tibble(apro_mk_colm)

apro_mk_colm$year <-  as.numeric(format(apro_mk_colm$TIME_PERIOD, format="%Y"))
apro_mk_colm$month <-  as.character(format(apro_mk_colm$TIME_PERIOD, format="%m"))

# sum up only for THS_T unit
apro_mk_colm <- apro_mk_colm %>% filter(unit == "THS_T")

# add up monthly to annual values
apro_mk_colm_annual <- apro_mk_colm %>% group_by(dairyprod, geo, year) %>% summarise(nr_months=n(),annual_values=sum(values))

# Example: see LU cow's milk production
apro_mk_colm_annual %>% filter(dairyprod=="D1110D") %>% filter(geo=="LU") %>% View()
