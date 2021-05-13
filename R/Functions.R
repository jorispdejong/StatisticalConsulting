#####################
### PREPROCESSING ###
#####################
# download csv files from the John Hopkins university github repo
# write the csv files to the new folder 'csv'
# if the files already exists then read the data from the files
# restructure the data into a list
# save as rdata file
preprocessData <- function(rdata_file_path, verbose = T){
  # library
  suppressPackageStartupMessages(library('readr')) # read csv
  
  # create new folder that will hold the data
  dir.create('data', showWarnings = F) # if it already exists will only throw an error
  
  # folder name + file name and path
  folder_name <- 'data/csv' 
  
  # create new folder that will hold all the csv files
  dir.create(folder_name, showWarnings = F) # if it already exists will only throw an error
  
  # CONFIRMED
  file_name_c <- 'time_series_covid19_confirmed_global.csv'
  file_path_c <- paste0(folder_name, '/', file_name_c)
  if(!file.exists(file_path_c)){
    if(verbose) print('Downloading confirmed global cases from open john hopkins university github repo...')
    
    # confirmed cases - global 
    url_confirmed <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
    confirmed_raw <- read_csv(url_confirmed, 
                              col_types = cols(`Province/State` = col_character(), 
                                               `Country/Region` = col_character()))
    # write csv file to folder
    write.csv(confirmed_raw, file = file_path_c, row.names = F)
  }else{
    if(verbose) print('Importing confirmed cases file from folder')
    # read csv file from file
    confirmed_raw <- read_csv(file = file_path_c, 
                              col_types = cols(`Province/State` = col_character(), 
                                               `Country/Region` = col_character())) 
  }
  
  # RECOVERED
  file_name_r <- 'time_series_covid19_recovered_global.csv'
  file_path_r <- paste0(folder_name, '/', file_name_r)
  if(!file.exists(file_path_r)){
    if(verbose) print('Downloading recovered global cases from open john hopkins university github repo...')
    
    # recovered cases - global
    url_recovered <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
    recovered_raw <- read_csv(url_recovered, 
                              col_types = cols(`Province/State` = col_character(), 
                                               `Country/Region` = col_character()))
    # write csv file to folder
    write.csv(recovered_raw, file = file_path_r, row.names = F)
  }else{
    if(verbose) print('Importing recovered cases file from folder')
    # read csv file from file
    recovered_raw <- read_csv(file = file_path_r, 
                              col_types = cols(`Province/State` = col_character(), 
                                               `Country/Region` = col_character())) 
  }
  
  # DEATHS
  file_name_d <- 'time_series_covid19_deaths_global.csv'
  file_path_d <- paste0(folder_name, '/', file_name_d)
  if(!file.exists(file_path_d)){
    if(verbose) print('Downloading deaths global cases from open john hopkins university github repo...')
    
    # deaths cases - global
    url_deaths <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
    deaths_raw <- read_csv(url_deaths, 
                           col_types = cols(`Province/State` = col_character(), 
                                            `Country/Region` = col_character()))
    # write csv file to folder
    write.csv(deaths_raw, file = file_path_d, row.names = F)
  }else{
    if(verbose) print('Importing deaths cases file from folder')
    # read csv file from file
    deaths_raw <- read_csv(file = file_path_c, 
                           col_types = cols(`Province/State` = col_character(), 
                                            `Country/Region` = col_character())) 
  }
  
  # POPULATION SIZE (https://data.worldbank.org/indicator/SP.POP.TOTL?end=2019&start=2019)
  file_name_p <- 'time_series_population_per_country.csv'
  file_path_p <- paste0(folder_name, '/', file_name_p)
  if(verbose) print('Importing populations per country csv file from folder')
  populations_raw <- read_csv(file_path_p, 
                              col_types = cols_only(Location = col_character(), 
                                                    Variant = col_character(),
                                                    Time = col_double(), PopTotal = col_double()))
  # keep only the correct rows
  populations_raw <- populations_raw[as.integer(populations_raw$Time==2021) 
                                     + as.integer(populations_raw$Variant=="Medium")==2,c(1,4)]
  populations_raw$PopTotal <- populations_raw$PopTotal * 1000
  
  
  if(verbose) print('Done!')
  global_covid_data_raw <- list('confirmed'=confirmed_raw,
                                'recovered'=recovered_raw,
                                'deaths'=deaths_raw,
                                'populations'=populations_raw)
  
  # structure the data
  global_covid_data <- structureData(global_covid_data_raw, verbose = verbose)
  
  # create new folder that will hold all the csv files
  dir.create('data/RData' , showWarnings = F) # if it already exists will only throw an error
  
  # save to RData file
  save(global_covid_data, file=rdata_file_path)
}

######################
### STRUCTURE DATA ###
######################
# create a structured list of the data
structureData <- function(global_covid_data_raw, verbose=T){
  if(verbose) print('Restructuring the data to a list...')
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

