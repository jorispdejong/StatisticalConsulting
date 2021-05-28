# clear global environment
rm(list = ls(all.names = TRUE))

# reproducibility
set.seed(6)

# source
source('Functions.R')

# libraries
suppressPackageStartupMessages(library('R0'))

###################
### LOADING DATA ##
###################
# RData file path
rdata_file_path <- 'data/RData/global_covid_data.RData'
# the function 'prepocessData' exports an .RData file with the name 'global_covid_data'
if(!file.exists(rdata_file_path)) preprocessData(rdata_file_path=rdata_file_path, verbose = F)
# load RData
load(rdata_file_path)

####################
### PREPARE DATA ###
####################
covid_data <- global_covid_data$Netherlands
confirmed <- covid_data$confirmed
names(confirmed) <- covid_data$t
daily <- diff(confirmed)
# plot(as.Date(names(daily)), daily, type = 'l')

##########
### R0 ###
##########

