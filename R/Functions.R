###################
### IMPORT DATA ###
###################
# download csv file from the John Hopkins university github repo
# write the csv file to the file 'time_series_covid19_confirmed_global.csv' in a new folder 'csv'
# if the file already exists then read the data from the file
importData <- function(verbose = T){
  # library
  suppressPackageStartupMessages(library('readr')) # read csv
  
  # folder name + file name and path
  folder_name <- 'csv' 
  file_name <- 'time_series_covid19_confirmed_global.csv'
  file_path <- paste0(folder_name, '/', file_name)
  
  # create new folder that will hold all the csv files
  dir.create(folder_name, showWarnings = F) # if it already exists will only throw an error
  
  if(!file.exists(file_path)){
    if(verbose) print('Downloading csv files from open john hopkins university github repo...')
    
    # confirmed cases - global
    url <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'
    data_raw <- read_csv(url, 
                         col_types = cols(`Province/State` = col_character(), 
                                          `Country/Region` = col_character()))
    write.csv(confirmed_global_raw, file = file_path, row.names = F)
  }else{
    if(verbose) print('Importing csv file from folder')
    data_raw <- read_csv(file = file_path, 
                         col_types = cols(`Province/State` = col_character(), 
                                          `Country/Region` = col_character()))
  }
  
  return(data_raw)
}