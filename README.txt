The scripts in data-raw download and process data from Eurostat for the 
Short-Term Outlook

# data-raw/apro_mk_pobta.r

- Used for POBTA update
- Creates Excel file apro_mk_pobta_fromR.xlsx

# data-raw/org_aprod.r

- Used for organic update
- Creates Excel file org_aprod.xlsx

# data-raw/org_lstspec.r

- Used for organic update
- Creates Excel file org_lstspec.xlsx



# comments

- The scripts also save the downloaded data into 'data' in .RData format
- Timestamps are added in the Excel files (Sheet timestamp) with the time/date
of the execution of the R script