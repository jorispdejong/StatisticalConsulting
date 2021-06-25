################################
### REPRODUCTION NUMBER RIVM ###
################################
getReproductionValueFromRIVM <- function(){
  suppressPackageStartupMessages(library('jsonlite'))
  url <- "https://data.rivm.nl/covid-19/COVID-19_reproductiegetal.json"
  data <- jsonlite::fromJSON(url)
  return(data)
}


###################
### IMPORT DATA ###
###################
importData <- function(preprocess_data=T, download_from_web=T,
                       folder_path = 'data/reproduction number/',
                       rdata_file_name = 'global_covid_data.RData',
                       verbose=T){
  if(preprocess_data){
    preprocessData(download_from_web = download_from_web, folder_path = folder_path,
                   rdata_file_name = rdata_file_name, verbose = verbose) 
  }else{
    if(!file.exists(paste0(folder_path,rdata_file_name))) stop('the rdata file could not be found... have you preprocessed the data?')
  }
  
  # load RData
  load(paste0(folder_path,rdata_file_name))
  return(global_covid_data)
}



#####################
### PREPROCESSING ###
#####################
# download csv files from the John Hopkins university github repo
# write the csv files to the given folder
# if the files already exists then read the data from the files
# restructure the data 
# save as RData file
preprocessData <- function(download_from_web=T, folder_path='data/reproduction number/',
                           rdata_file_name = 'global_covid_data.RData', verbose = T){
  # library
  suppressPackageStartupMessages(library('readr')) # read csv
  
  # file names and paths
  list_names <- c('confirmed', 'recovered', 'deaths', 'populations')
  file_names <- c('time_series_covid19_confirmed_global.csv',
                  'time_series_covid19_recovered_global.csv',
                  'time_series_covid19_deaths_global.csv')
  urls <- c('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv',
            'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv',
            'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv')
  file_paths <- paste0(folder_path, file_names)
  
  global_covid_data_raw <- list() # list that will hold the raw data
  if(download_from_web){
    if(verbose) print(paste0('downloading covid global time series from open john hopkins university github repo... please wait'))
    for(i in 1:length(file_paths)){
      # download data from web
      global_covid_data_raw[[list_names[i]]] <- read_csv(urls[i], 
                                             col_types = cols(`Province/State` = col_character(), 
                                                              `Country/Region` = col_character()))
      
      
      # write csv file to folder
      write.csv(global_covid_data_raw[[list_names[i]]], file = file_paths[i], row.names = F)
    }
    if(verbose) print('done!')
  }else{
    if(sum(file.exists(file_paths))!=3) stop('cannot find the csv files, one or more is missing')
    if(verbose) print(paste0('Importing covid global time series from folder'))
    for(i in 1:length(file_paths)){
      # read csv file from file
      global_covid_data_raw[[list_names[i]]] <- read_csv(file = file_paths[i], 
                                             col_types = cols(`Province/State` = col_character(), 
                                                              `Country/Region` = col_character())) 
    }
    if(verbose) print('done!')
  }
  
  # POPULATION SIZE (https://data.worldbank.org/indicator/SP.POP.TOTL?end=2019&start=2019)
  population_file_path <- paste0(folder_path,'time_series_population_per_country.csv')
  if(!file.exists(population_file_path)) stop('cannot find the population csv file')
  
  if(verbose) print('Importing populations per country csv file from folder')
  populations_raw <- read_csv(population_file_path, 
                              col_types = cols_only(Location = col_character(), 
                                                    Variant = col_character(),
                                                    Time = col_double(), PopTotal = col_double()))
  if(verbose) print('done!')
  
  # keep only the correct rows
  populations_raw <- populations_raw[as.integer(populations_raw$Time==2021) 
                                     + as.integer(populations_raw$Variant=="Medium")==2,c(1,4)]
  populations_raw$PopTotal <- populations_raw$PopTotal * 1000
  global_covid_data_raw[[list_names[4]]] <- populations_raw
  
  # structure the data
  global_covid_data <- structureData(global_covid_data_raw, verbose = verbose)
  
  # save to RData file
  save(global_covid_data, file=paste0(folder_path, rdata_file_name))
}

######################
### STRUCTURE DATA ###
######################
# create a structured list of the data
structureData <- function(global_covid_data_raw, verbose=T){
  if(verbose) print('Restructuring covid data... please wait')
  # get common countries from the confirmed,recovered,deaths datasets
  common_countries_raw <- Reduce(intersect,
                             sapply(1:length(global_covid_data_raw[1:3]),
                                    function(i)
                                      apply(global_covid_data_raw[[i]], 1,
                                            function(row)
                                              if(is.na(row[1])) row[2] else paste0(row[2], '-', row[1]))))
  # get common countries with population dataset
  common_countries <- intersect(common_countries_raw,global_covid_data_raw$populations$Location)
  
  # keep only the common countries
  global_covid_data <- lapply(1:length(global_covid_data_raw), function(i){
    if(i==1 || i==2 || i==3){
      l <- apply(global_covid_data_raw[[i]], 1, function(j) if(is.na(j[1])) j[2] else paste0(j[2],'-',j[1])) 
    }else{
      l <- global_covid_data_raw[[i]]$Location
    }
    return(global_covid_data_raw[[i]][l %in% common_countries,])
  })
  
  # reformat data to list
  data <- list()
  for(i in 1:nrow(global_covid_data[[1]])){
    row_c <- global_covid_data[[1]][i,]
    row_r <- global_covid_data[[2]][i,]
    row_d <- global_covid_data[[3]][i,]
    row_p <- global_covid_data[[4]][i,]
    
    lat <- as.numeric(row_c[3])
    lon <- as.numeric(row_c[4])
    t <- as.Date(names(row_c[5:length(row_c)]), '%m/%d/%y')
    confirmed <- as.numeric(row_c[5:length(row_c)])
    recovered <- as.numeric(row_r[5:length(row_r)])
    deaths <- as.numeric(row_d[5:length(row_d)])
    population <- as.double(row_p[,2])
    
    data[[common_countries[i]]] <- list('lat'=lat,
                                        'lon'=lon,
                                        'confirmed'=confirmed,
                                        'recovered'=recovered,
                                        'deaths'=deaths,
                                        't'= t,
                                        'population'=population)
  }
  
  if(verbose) print('Done!')
  
  return(data)
}

