# get EUROSTAT data on the dair herd

library(eurostat)
library(tidyverse)

# get dairy herd data; annual; for current EU-27 countries; years 2012-2022

# herd stats include many animal categories (we only need dairy cows)
herd_stat <- get_eurostat(id="apro_mt_lscatl")
herd_stat <- as_tibble(herd_stat)

dairy_cows <- herd_stat %>% filter(animals == "A2300F") %>% filter(month == "M12")

# calculate an aggregate from  current EU-27 countries
EU27 <- c("AT","BE","BG","CY","CZ","DE","DK","EE","EL","ES","FI","FR","HR","HU","IE","IT","LT","LU","LV","MT","NL","PL","PT","RO","SE","SI","SK")

# check the number of countries! must be 27
#length(EU27)

dairy_cows_eu <- dairy_cows %>% filter(geo %in% EU27)
dairy_cows_eu <- dairy_cows_eu %>% group_by(animals,time,unit,month) %>% summarise(values=sum(values))

# convert the format of the dates (we only need years)
dairy_cows_eu$time <-  as.numeric(format(dairy_cows_eu$time, format="%Y"))

# get the last 10 years only
dairy_cows_eu <- dairy_cows_eu %>% filter(time>2010)


# calculate the growth rates

#compound and linear growth rates 2012-2022
comp_grate <- (1/(2022-2012)) *
         (log(dairy_cows_eu[dairy_cows_eu$time==2022,]$values) - log(dairy_cows_eu[dairy_cows_eu$time==2012,]$values))

lin_grate <-(dairy_cows_eu[dairy_cows_eu$time==2022,]$values - dairy_cows_eu[dairy_cows_eu$time==2012,]$values)/
             dairy_cows_eu[dairy_cows_eu$time==2012,]$values / (2022-2012)

#compound and linear growth rates 2011-2021
lin_grate2 <-(dairy_cows_eu[dairy_cows_eu$time==2021,]$values - dairy_cows_eu[dairy_cows_eu$time==2011,]$values)/
  dairy_cows_eu[dairy_cows_eu$time==2011,]$values / (2021-2011)

comp_grate2 <- (1/(2021-2011)) *
  (log(dairy_cows_eu[dairy_cows_eu$time==2021,]$values) - log(dairy_cows_eu[dairy_cows_eu$time==2011,]$values))


# to calculate (apparent) milk yield, we need raw cow milk production and the dairy herd
apro_mk <- get_eurostat("apro_mk_farm")
apro_mk <- as_tibble(apro_mk)

# raw milk produced on farms is variable D1110A
cow_milk <- apro_mk %>% filter(milkitem == "PRO") %>% filter(dairyprod == "D1110A")

cow_milk_eu <- cow_milk %>% filter(geo %in% EU27)
cow_milk_eu <- cow_milk_eu %>% group_by(milkitem,time) %>% summarise(values=sum(values,na.rm = TRUE))

# convert the format of the dates (we only need years)
cow_milk_eu$time <-  as.numeric(format(cow_milk_eu$time, format="%Y"))

# get the last 10 years only
cow_milk_eu <- cow_milk_eu %>% filter(time>2010)


# to calculate milk yields, we need to merge the milk production and herd tables
milk_yield <- cow_milk
milk_yield <- milk_yield %>% select(-milkitem,-dairyprod) %>% filter(geo %in% EU27)
colnames(milk_yield)[3] <- "milk"

dairy_cows_merge <- dairy_cows %>% select(-month, -unit,-animals) %>% filter(geo %in% EU27)
colnames(dairy_cows_merge)[3] <- "herd"

milk_yield <- dairy_cows_merge %>% left_join(milk_yield, by = c("geo" = "geo", "time"="time"))
milk_yield <- milk_yield %>% mutate(yield=milk/herd)
milk_yield$time <-  as.numeric(format(milk_yield$time, format="%Y"))
milk_yield <- milk_yield %>% filter(!is.na(milk))


# calculate the EU average milk yield
milk_yield_eu <- milk_yield %>% group_by(time) %>% 
  summarise(milk=sum(milk,na.rm = TRUE),herd=sum(herd,na.rm = TRUE))

milk_yield_eu <- milk_yield_eu %>% mutate(yield=milk/herd*1000)

# calculate growth rates for the milk yield
comp_yield_grate <- (1/(2021-2011)) *
  (log(milk_yield_eu[milk_yield_eu$time==2021,]$yield) - log(milk_yield_eu[milk_yield_eu$time==2011,]$yield))

lin_yield_grate <-(milk_yield_eu[milk_yield_eu$time==2021,]$yield - milk_yield_eu[milk_yield_eu$time==2011,]$yield)/
  milk_yield_eu[milk_yield_eu$time==2011,]$yield / (2021-2011)

# check where the same growth rate would lead us in 10 years
milk_yield_eu[milk_yield_eu$time==2011,]$yield * exp(comp_yield_grate*(2032-2011))

# 
milk_yield_eu[milk_yield_eu$time==2011,]$yield * (1 + lin_yield_grate*(2021-2011)) 
milk_yield_eu[milk_yield_eu$time==2011,]$yield * (1 + lin_yield_grate*(2032-2011)) 
