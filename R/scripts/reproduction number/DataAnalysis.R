# clear global environment
rm(list = ls(all.names = TRUE))

# reproducibility
set.seed(6)

# source
source('scripts/reproduction number/DataPreprocessing.R')
source('scripts/reproduction number/Functions.R')

#############################
### LOAD AND PREPARE DATA ###
#############################
# import data
global_covid_data <- importData(preprocess_data = F, download_from_web = T)

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

# 7-day moving average
smooth_incid <- ma(incid)

# plot daily new cases
par(mfrow=c(1,1), mar=c(5.1, 5.1, 4.1, 2.1))
plot(t[1:200], incid[1:200], type='h',
     xlab = 't', ylab = 'Number of infected people',
     main = 'Number of people infected with COVID-19 in the Netherlands over time',
     xaxt='n', cex.lab=1.3)
axis(1, t[seq(1,length(t),50)], 
     format(t[seq(1,length(t),50)], "%d %b %y"))
lines(t[1:200], ma(incid[1:200],n=5), col=1, lwd=3)
legend("topleft", 
       legend = c("Daily number of infected people", "7-day average of infected people"), 
       lty=c(1,1), col=c(1,1), lwd=c(1,3), cex=1.25)
