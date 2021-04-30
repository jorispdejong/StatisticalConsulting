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
    
    # read csv file from web
    url <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'
    data_raw <- read_csv(url, 
                         col_types = cols(`Province/State` = col_character(), 
                                          `Country/Region` = col_character()))
    
    # write csv file to folder
    write.csv(confirmed_global_raw, file = file_path, row.names = F)
  }else{
    if(verbose) print('Importing csv file from folder')
    # read csv file from file
    data_raw <- read_csv(file = file_path, 
                         col_types = cols(`Province/State` = col_character(), 
                                          `Country/Region` = col_character()))
  }
  
  return(data_raw)
}

######################
### STRUCTURE DATA ###
######################
# create a structured list of the data
structureData <- function(data_raw){
  # reformat data into list
  data <- apply(as.matrix(data_raw), 1, 
                function(row){
                  lat <- as.numeric(row[3])
                  lon <- as.numeric(row[4])
                  confirmed_cases <- as.numeric(row[5:length(row)])
                  t <- as.Date(names(row[5:length(row)]), '%m/%d/%y')
                  
                  return(list('lat'=lat,
                              'lon'=lon,
                              'confirmed_cases'=confirmed_cases,
                              't'= t))
                })
  names(data) <- apply(data_raw, 1, function(row) if(is.na(row[1])) row[2] else paste0(row[2], '-', row[1]))
  
  return(data)
}
