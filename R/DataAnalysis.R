# clear global environment
rm(list = ls(all.names = TRUE))

# reproducibility
set.seed(6)

# source
source('Functions.R')

# import data
data_raw <- importData()
data <- structureData(data_raw)

# time series for NL
NL_confirmed <- data$Netherlands$confirmed_cases
times_confirmed <- data$Netherlands$t
NL_daily <- diff(NL_confirmed)
times_daily <- times_confirmed[2:length(times_confirmed)]
  
# plot time series
par(mfrow=c(2,1), mar=c(3,3,2,2))
plot(times_confirmed, NL_confirmed, type = 'l', main = 'Cumulative cases', xlab = '', ylab = '')
plot(times_daily, NL_daily, type='l', main = 'Daily cases', xlab = '', ylab = '')

