#
# function to save to Excel and add a timestamp
#

save_to_excel <- function(tibble_to_save,folder_to_save){

  
  #
  # Note that pivot_wider would put the columns/years in the order of the first appearance
  #
  excel_out <- tibble_to_save %>% arrange(time) %>% pivot_wider(names_from = time)
  
  #
  # create an Excel file for further processing
  # write.xlsx2 would be faster, but we could not keep the missing values (NA) in the Excel file
  # so the preferred option is to use write.xlsx, even if that's slower
 
  # write to Excel 
  write.xlsx(as.data.frame(excel_out), 
             file = paste(folder_to_save,"/", print(substitute(tibble_to_save)), "_fromR.xlsx",sep = ""), 
             row.names = FALSE, col.names = TRUE, sheetName = paste(print(substitute(tibble_to_save))),
             showNA = TRUE)
  
  # add date of last data update on Eurostat
  write.xlsx(s_update$lastUpdate, 
             file = paste(folder_to_save,"/", print(substitute(tibble_to_save)), "_fromR.xlsx",sep = ""),
             row.names = FALSE, col.names = TRUE, sheetName = "releasedate",
             showNA = TRUE, append = TRUE)
  
  
  # save data extraction also in R data format
  save(tibble_to_save, 
       file = paste(folder_to_save,"/", print(substitute(tibble_to_save)), "_", as.character(s_update$lastUpdate), ".RData", sep = ""))
  
  
}
