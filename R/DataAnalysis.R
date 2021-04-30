# clear global environment
rm(list = ls(all.names = TRUE))

# reproducibility
set.seed(6)

# source
source('Functions.R')

# import data
data_raw <- importData()

# time series for NL
NL_confirmed <- t(as.matrix(data_raw[197,5:ncol(data_raw)]))
times_confirmed <- as.Date(rownames(NL_confirmed), '%m/%d/%y')
NL_daily <- diff(NL_confirmed)
times_daily <- as.Date(rownames(NL_daily), '%m/%d/%y')
  
# plot time series
par(mfrow=c(2,1), mar=c(3,3,2,2))
plot(times_confirmed, NL_confirmed, type = 'l', main = 'Cumulative cases', xlab = '', ylab = '')
plot(times_daily, NL_daily, type='l', main = 'Daily cases', xlab = '', ylab = '')

