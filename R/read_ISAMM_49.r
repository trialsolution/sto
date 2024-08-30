# read notifications from the file provided by E3
# ISAMM form No. 49

library(tidyverse)
library(readxl)
library(xlsx)


#
# General settings
#

read_folder <- "U:/4-Market Analysis/4-2 Short-Term Outlook/Outlook Dairy/Short term dairy/Database/EUROSTAT monthly/"

#extraction_folder <- "U:/4-Market Analysis/4-2 Short-Term Outlook/Outlook Dairy/Short term dairy/2024_2/Eurostat download with R/"
extraction_folder <- read_folder

# file provided by E3 (Damien)
isamm_filename <- "Milk_Monthly_Deliveries (R. 479-2010) CSV.xlsm"


#
# Load and process data
#

isamm <- read_excel(paste(read_folder,isamm_filename, sep = ""),
                    sheet = "Summary (CSV-Mkt-trans)", 
                    range = "A4:AB111",
                    col_types = c("date",rep("numeric",27)))

# put countries in rows
isamm <- isamm %>% pivot_longer(cols = 2:ncol(isamm), names_to = "countries")

# add year as column
isamm$year <- as.numeric(format(isamm$date, format="%Y"))

# calculate annuals
isamm <- isamm %>% group_by(year,countries) %>% summarise(nr_obs=n(),value=sum(value))

# keep annual values only if all 12 months are available
isamm <- isamm %>% filter(nr_obs == 12) %>% select(-nr_obs) %>% pivot_wider(names_from = year, values_from = value, values_fn = sum)

# annual values and number of observations(months with data) per year
#isamm <- isamm %>% pivot_wider(names_from = year, values_from = c(value,nr_obs), values_fn = sum)


#
# save table as EXcel file
#

write.xlsx(as.data.frame(isamm), file = paste(extraction_folder,"isamm_49_annual.xlsx",sep = ""), 
           row.names = FALSE, col.names = TRUE, sheetName = "isamm_49_annual",
           showNA = TRUE)

# add timestamp
timestamp <- format(Sys.time(), "data extracted on %Y.%m.%d-%H:%M:%S")

# save to disk
write.xlsx(timestamp, file = paste(extraction_folder,"isamm_49_annual.xlsx",sep = ""), 
           row.names = FALSE, col.names = TRUE, sheetName = "timestamp",
           showNA = TRUE, append = TRUE)


