# clear global environment
rm(list = ls(all.names = TRUE))

# reproducibility
set.seed(6)

# source
source('scripts/DataPreprocessing.R')

# libraries
suppressPackageStartupMessages(library('EpiEstim'))

#############################
### LOAD AND PREPARE DATA ###
#############################
preprocess_data <- F
download_from_web <- F
verbose <- T

# import data
global_covid_data <- importData(preprocess_data = preprocess_data, download_from_web = download_from_web, verbose = verbose)

####################
### PREPARE DATA ###
####################
# covid data of the Netherlands
covid_data <- global_covid_data$Netherlands 
infected_cumulative <- covid_data$confirmed 
infected_daily <- diff(infected_cumulative)

# create reported cases data frame
incid <- infected_daily[(tail(which(infected_daily==0),1)+1):length(infected_daily)]
t <- covid_data$t[(tail(which(infected_daily==0),1)+1):length(infected_daily)]
names(incid) <- t
  
####################################
### GENERATION TIME DISTRIBUTION ###
####################################
# lognormal: 4.7, 2.9 (complete data set with right truncation)
# weibull: 4.8, 2.3 (only most reliable sources with right truncation)
lognormal <- T
config <- make_config(list(
  mean_si = if(lognormal) 4.7 else 4.8,
  std_si = if(lognormal) 2.9 else 2.3,
  si_parametric_distr = if(lognormal) 'L' else 'W'))

##################
### ESTIMATE R ###
##################
## Estimate R with assumptions on serial interval
res <- estimate_R(incid, method = "parametric_si", config = config)
Rt <- res$R$`Mean(R)`
t_epi <- t[res$R$t_end]

####################
### PLOT RESULTS ###
####################
plot(res)

