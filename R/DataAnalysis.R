# clear global environment
rm(list = ls(all.names = TRUE))

# reproducibility
set.seed(6)

# source
source('Functions.R')

# rdata file path
rdata_file_path <- 'data/RData/data.RData'

if(!file.exists(rdata_file_path)){
  preprocessData(verbose = F)
}
# load RData
load(rdata_file_path)

# time series for NL
country <- 181
NL_confirmed <- data[[country]]$confirmed
times_confirmed <- data[[country]]$t
NL_daily <- diff(NL_confirmed)
times_daily <- times_confirmed[2:length(times_confirmed)]
  
# plot time series
par(mfrow=c(2,1), mar=c(3,3,2,2))
plot(times_confirmed, NL_confirmed, type = 'l', 
     main = paste0('Cumulative infected cases - ', names(data)[country]), xlab = '', ylab = '')
plot(times_daily, NL_daily, type='l', main = 'Daily cases', xlab = '', ylab = '')

