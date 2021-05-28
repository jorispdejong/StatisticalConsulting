#####################
### PREPROCESSING ###
#####################
getPreprocessedDataGlobal <- function(download_from_web=F, preprocess_data=F){
  # libraries
  suppressPackageStartupMessages(library('readr'))
  
  begin_date <- as.Date("06-01-2020", "%m-%d-%Y")
  daily_report_csv_date <- as.Date("09-01-2020", "%m-%d-%Y")
  # import data
  if(download_from_web){
    # confirmed cases - global 
    url_confirmed <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
    C_raw <- read_csv(url_confirmed, 
                      col_types = cols(`Province/State` = col_character(), 
                                       `Country/Region` = col_character()))
    # recovered cases - global
    url_recovered <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
    R_raw <- read_csv(url_recovered, 
                      col_types = cols(`Province/State` = col_character(), 
                                       `Country/Region` = col_character()))
    # deaths cases - global
    url_deaths <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
    D_raw <- read_csv(url_deaths, 
                      col_types = cols(`Province/State` = col_character(), 
                                       `Country/Region` = col_character()))
    
    # import daily cases of June 1st 2020 to find N
    date_char <- paste0(substr(as.character(daily_report_csv_date),6,8),
                        substr(as.character(daily_report_csv_date),9,10),'-',
                        substr(as.character(daily_report_csv_date),1,4))
    url_daily_cases_06_01_2020 <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/", date_char,".csv")
    DC_raww <- read_csv(url_daily_cases_06_01_2020,
                        col_types = cols(FIPS = col_skip(), Admin2 = col_skip(), 
                                         Province_State = col_character(), 
                                         Country_Region = col_character(), 
                                         Last_Update = col_skip(), Lat = col_skip(), 
                                         Long_ = col_skip(), Deaths = col_skip(), 
                                         Recovered = col_skip(), Active = col_skip(), 
                                         Combined_Key = col_skip(), `Case-Fatality_Ratio` = col_skip()))
    DC_raw <- DC_raww[-which(DC_raww$Country_Region=='US'),] # remove US, because it is not in the global data sets
    
    # read the csv files to the local folder
    write.csv(C_raw, "time_series_covid19_confirmed_global.csv", row.names = F)
    write.csv(R_raw, "time_series_covid19_recovered_global.csv", row.names = F)
    write.csv(D_raw, "time_series_covid19_deaths_global.csv", row.names = F)
    write.csv(DC_raw, "covid19_daily_report_09_01_2020.csv", row.names = F)
  }else{
    C_raw <- read_csv("time_series_covid19_confirmed_global.csv", 
                      col_types = cols(`Province/State` = col_character(), 
                                       `Country/Region` = col_character()))
    R_raw <- read_csv("time_series_covid19_recovered_global.csv", 
                      col_types = cols(`Province/State` = col_character(), 
                                       `Country/Region` = col_character()))
    D_raw <- read_csv("time_series_covid19_deaths_global.csv", 
                      col_types = cols(`Province/State` = col_character(), 
                                       `Country/Region` = col_character()))
    DC_raw <- read_csv("covid19_daily_report_09_01_2020.csv", 
                       col_types = cols(Province_State = col_character(), 
                                        Country_Region = col_character()))
  }
  
  if(preprocess_data){
    # find intersecting columns
    countries_C_raw <- sapply(1:nrow(C_raw), function(i) paste0(C_raw[i,2], ifelse(is.na(C_raw[i,1]), '', '-'), ifelse(is.na(C_raw[i,1]), '', C_raw[i,1])))
    countries_R_raw <- sapply(1:nrow(R_raw), function(i) paste0(R_raw[i,2], ifelse(is.na(R_raw[i,1]), '', '-'), ifelse(is.na(R_raw[i,1]), '', R_raw[i,1])))
    countries_D_raw <- sapply(1:nrow(D_raw), function(i) paste0(D_raw[i,2], ifelse(is.na(D_raw[i,1]), '', '-'), ifelse(is.na(D_raw[i,1]), '', D_raw[i,1])))
    countries_DC_raw <- sapply(1:nrow(DC_raw), function(i) paste0(DC_raw[i,2], ifelse(is.na(DC_raw[i,1]), '', '-'), ifelse(is.na(DC_raw[i,1]), '', DC_raw[i,1])))
    intersecting_countries <- Reduce(intersect, list(countries_C_raw, countries_R_raw, countries_D_raw, countries_DC_raw))
    
    # retain only intersecting countries rows
    DC_raw1 <- DC_raw[which(countries_DC_raw %in% intersecting_countries),]
    C_raw1 <- C_raw[which(countries_C_raw %in% intersecting_countries),]
    R_raw1 <- R_raw[which(countries_R_raw %in% intersecting_countries),]
    D_raw1 <- D_raw[which(countries_D_raw %in% intersecting_countries),]
    
    # compute N
    N <- round(DC_raw1[,3]/DC_raw1[,4]*100000)
    
    # remove the rows that have NA population and remove columns before begin date
    info_df <- cbind(C_raw1[-which(is.na(N)),1:2], N[-which(is.na(N)),], C_raw1[-which(is.na(N)),3:4])
    colnames(info_df) <- c(colnames(info_df)[1:2], 'N', colnames(info_df)[4:5])
    
    which_cols_to_keep <- which(as.Date(sapply(colnames(C_raw1[5:ncol(C_raw1)]), function(x){
      s <- strsplit(x, "/")[[1]]
      return(paste0(s[1], '-', s[2], '-', '20', s[3]))
    }), "%m-%d-%Y")>=begin_date) + 4
    C <- cbind(info_df, C_raw1[-which(is.na(N)),which_cols_to_keep])
    R <- cbind(info_df, R_raw1[-which(is.na(N)),which_cols_to_keep])
    D <- cbind(info_df, D_raw1[-which(is.na(N)),which_cols_to_keep])
    I <- cbind(info_df, C[,6:ncol(C)] - R[,6:ncol(R)] - D[,6:ncol(D)])
    S <- cbind(info_df, matrix(rep(info_df$N,ncol(C)-6),nrow(C)) - I[,6:ncol(I)] - R[,6:ncol(R)] - D[,6:ncol(D)])
    
    write.csv(C, "C.csv", row.names = F)
    write.csv(S, "S.csv", row.names = F)
    write.csv(I, "I.csv", row.names = F)
    write.csv(R, "R.csv", row.names = F)
    write.csv(D, "D.csv", row.names = F)
  }else{
    C <- read_csv("C.csv", col_types = cols(`Province/State` = col_character(), 
                                            `Country/Region` = col_character()))
    S <- read_csv("S.csv", col_types = cols(`Province/State` = col_character(), 
                                            `Country/Region` = col_character()))
    I <- read_csv("I.csv", col_types = cols(`Province/State` = col_character(), 
                                            `Country/Region` = col_character()))
    R <- read_csv("R.csv", col_types = cols(`Province/State` = col_character(), 
                                            `Country/Region` = col_character()))
    D <- read_csv("D.csv", col_types = cols(`Province/State` = col_character(), 
                                            `Country/Region` = col_character()))
  }
  
  t <- as.Date(sapply(colnames(C[6:ncol(C)]), function(x){
    s <- strsplit(x, "/")[[1]]
    return(paste0(s[1], '-', s[2], '-', '20', s[3]))
  }), "%m-%d-%Y")
  
  return(list('t'=t, 'C'=C, 'S'=S, 'I'=I, 'R'=R, 'D'=D))
}

getPreprocessedData <- function(preprocess_data=F, download_from_web=F, 
                                folder_name_csv="csv", json_file_name="SIRD_covid_19_global.json", 
                                verbose=T){
  if(!dir.exists(folder_name_csv) && !download_from_web){
    stop(paste0('The directory ', folder_name_csv, ' does not exist. Is the directory name correct? Have you already downloaded the csv files from web?'))
  }
  if(dir.exists(folder_name_csv) && length(list.files(folder_name_csv))==0){
    stop(paste0('The directory ', folder_name_csv, ' exists, but there are no files in it. Did you already download the files from web?'))
  }
  if(!preprocess_data && !file.exists(json_file_name)){
    stop(paste0('The file ', json_file_name, ' does not exist. Is the file name correct? Have you already preprocessed the data?'))
  }
  
  # export json
  suppressPackageStartupMessages(library('rjson')) # read and write JSON
  
  # preprocessing only needs to be done once
  if(preprocess_data){
    ###################
    ### IMPORT DATA ###
    ###################
    # read csv
    suppressPackageStartupMessages(library('readr'))
    
    # data format of the given data
    date_format <- "%m-%d-%Y"
    
    if(download_from_web){
      # create new folder that will hold all the csv files
      dir.create(folder_name_csv, showWarnings = F) # if it already exists will only throw an errow
      
      # set the begin and end date of the data
      begin_date <- as.Date("06-01-2020", date_format) # rough starting point for current data format
      end_date <- as.Date("04-01-2021", date_format)
      
      # loop over all the dates to download and save the csv file from github
      dates <- seq(begin_date, end_date, by="days")
      print('downloading csv files from open john hopkins university github repo')
      for(i in 1:length(dates)){
        # github repo uses mm-dd-YYYY format, so we need to rewrite the given date
        year <- substr(dates[i],1,4)
        month <- substr(dates[i],6,7)
        day <- substr(dates[i],9,10)
        file_name <- paste0(month, '-', day, '-', year, '.csv')
        # url of the file
        url_path <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/"
        # import (download) the csv file from the given url
        csv_raw <- read_csv(paste0(url_path,file_name))
        # save as csv
        write.csv(csv_raw, paste0(folder_name_csv, '/', file_name), row.names = FALSE)
      }  
    }
    
    #####################
    ### PREPROCESSING ###
    #####################
    # get the names of all the csv files in the directory
    all_file_names_raw <- list.files(folder_name_csv)
    # retrieve the dates of all the csv files
    dates <- as.Date(substr(all_file_names_raw,1,10), date_format)
    # order the file names and dates from old to new
    all_file_names <- all_file_names_raw[order(dates)]
    dates <- sort(dates)
    
    all_data <- list()
    # create progress bar
    pb <- txtProgressBar(min = 1, max = length(all_file_names), style = 3)
    if(verbose) print('importing csv files and compressing data')
    for(i in 1:length(all_file_names)){
      # import raw csv file
      data_raw <- read.csv(paste0(folder_name_csv, '/', all_file_names[i]))
      
      # remove the rows where Confirmed, Deaths, Recovered, Active, Incident_Rate is NA
      # also remove the rows where Incident_Rate is 0
      NA_data_raw_indices <- c(which(is.na(data_raw[,8])), which(is.na(data_raw[,9])), 
                               which(is.na(data_raw[,10])), which(is.na(data_raw[,11])), 
                               which(is.na(data_raw[,13])), which(data_raw[,13]==0))
      if(length(NA_data_raw_indices)==0){
        data_raw_cleaned <- data_raw
      }else{
        data_raw_cleaned <- data_raw[-NA_data_raw_indices,] 
      }
      
      # compute N
      N_col <- round(data_raw_cleaned[,8]/data_raw_cleaned[,13]*100000) # incident_rate is number of total infected per 100.000 persons
      
      # create columns with the desired variables: I, R, D (S will be computed later with S=N-I-R-D)
      I_col <- data_raw_cleaned[,11] # number of currently infected people
      R_col <- data_raw_cleaned[,10] # number of recovered people
      D_col <- data_raw_cleaned[,9] # number of deceased people
      country_col <- data_raw_cleaned[,4]
      
      # put the data in a df and sum all rows belonging to the same country
      data <- aggregate(data.frame("N"=N_col, "I"=I_col, "R"=R_col, "D"=D_col), list(country_col), sum)
      colnames(data) <- c('country', colnames(data)[2:ncol(data)])
      
      # put the cleaned compressed data in the all_data list
      all_data[[i]] <- data
      
      # update progress bar
      setTxtProgressBar(pb, i)
    }
    if(verbose) print('done!')
    
    # retain only the countries that appear in all data
    intersecting_countries <- Reduce(intersect, sapply(all_data, function(x) x[,1]))
    
    # retrieve N from the data (simply the N of the first csv file)
    N <- all_data[[1]][all_data[[1]][,1] %in% intersecting_countries,1:2]
    
    # initialize empty list that will hold the json object
    countries_json <- list()
    
    # create progress bar
    pb <- txtProgressBar(min = 1, max = length(intersecting_countries), style = 3)
    if(verbose) print('retrieving required parameters and converting to json')
    for(j in 1:length(intersecting_countries)){
      # initialize empty list that will hold the json entry for a single country
      l <- list()
      for(i in 1:length(all_data)){
        # get the I, R, D of all countries at time i
        temp_data <- all_data[[i]][all_data[[i]][,1] %in% intersecting_countries,3:5]
        # compute S
        S <- N[j,2] - sum(temp_data[j,])
        # get the vector containing S, I, R, D of country j
        vec <- c(S, as.numeric(temp_data[j,]))
        names(vec) <- c('S', 'I', 'R', 'D')
        # add the vector and date to the list
        l[[i]] <- list('t'=as.character(dates[i]), 'data'=vec)
      }
      
      # add the country json entry to the json list
      countries_json[[j]] <- list("N"=N[j,2], "data_points"=l)
      
      # update progress bar
      setTxtProgressBar(pb, j)
    }
    if(verbose) print('done!')
    names(countries_json) <- intersecting_countries
    
    if(verbose) print('writing to json file')
    write(toJSON(countries_json), json_file_name)
    if(verbose) print('done!')
  }
  
  # read json file
  data <- fromJSON(file=json_file_name)
  
  return(data)
}

########################
### HELPER FUNCTIONS ###
########################
get.dX <- function(y){
  T <-nrow(y)
  yt <- t(y)
  dx <- as.vector(yt[,-1]-yt[,-T])
  names(dx) <- rep(colnames(y),times=length(dx)/ncol(y))
  return(dx)
}
get.V <- function(){
  V <- matrix(0,4,3)
  rownames(V) <- c("S", "I", "R", "D")
  colnames(V) <- c("S->I", "I->R", "I->D")
  V [c("S","I"), "S->I"] <- c(-1,1)
  V [c("I","R"), "I->R"] <- c(-1,1)
  V [c("I","D"), "I->D"] <- c(-1,1)
  return(V)
}
get.M <- function(V,X,dT){
  ## this inner function returns the t-th replicate Mt of M
  get.Mt <- function(V,x,dt) Matrix(V%*%diag(get.d(x))*dt, sparse=T)
  
  ## stack together all the Mt ’s
  M <- do.call(rbind,lapply(1:nrow(X), function(t) get.Mt(V,X[t,],dT[t])))
  return(M)
}
get.d <- function(x){
  N <- sum(x) ## get the total population size
  ## get the diagonal of the matrix D(x):
  d <- rep(NA,3)
  d[1] <- x["S"]*x["I"]/N
  d[2] <- x["I"]
  d[3] <- x["I"]
  return(d)
}
get.h <- function(x,theta){
  d <- get.d(x)
  h <- d * theta
  return(h)
}
get.W <- function(V,X,theta,dT){
  ## this inner function returns the t-th replicate Wt of W
  get.Wt <- function (V,x,theta,dt) Matrix(V %*% diag(get.h(x,theta)) %*% t(V) * dt, sparse = T)
  
  ## stack together all the Wt ’s in a block diagonal shape
  W <- bdiag(lapply(1:nrow(X), function(t) get.Wt(V,X[t,],theta,dT[t])))
  return(W)
}

############
### IWLS ###
############
# This function takes in input an initial guess theta _0 for the parameters
# and returns the IWLS estimate theta_k of the parameters .
iwls <- function(theta_0,V,M,dX,dT,tol=1e-3, verbose=F){
  k <- 1
  ERR <- 1e10
  while(ERR > tol){
    if(verbose) print(paste0("IWLS - iteration ",k))
    theta_o <- theta_k ## save the current theta_k to theta_o
    Wk <- get.W(V,head(X,-1),theta_k,dT=dT)
    ## compute its nearest PD matrix in case Wk is singular 
    Wk <- nearPD(Wk, do2eigen=T, doSym=T, doDykstra=T)$mat
    Wki <- solve(Wk)
    theta_k <- as.numeric(solve(t(M)%*% Wki %*% M) %*% t(M) %*% Wki %*% dX)
    ## update theta_k
    ERR <- t(theta_k - theta_o) %*% (theta_k - theta_o)/(t(theta_o) %*% theta_o)
    ## update the error
    k <- k + 1 ## update the iteration number
  }
  return(theta_k)
}

#######################
### OTHER FUNCTIONS ###
#######################
movingAverage <- function(x,n=7){
  cx <- c(0,cumsum(x))
  rsum <- (cx[(n+1):length(cx)] - cx[1:(length(cx) - n)]) / n
  return(rsum)
}