# clear global environment
rm(list = ls(all.names = TRUE))

# reproducibility
set.seed(6)

# source
source('scripts/DataPreprocessing.R')

#############################
### LOAD AND PREPARE DATA ###
#############################
preprocess_data <- T
download_from_web <- T
verbose <- T

# import data
global_covid_data <- importData(preprocess_data = preprocess_data, download_from_web = download_from_web, verbose = verbose)

# preparing an example (NL covid data)
covid_data <- global_covid_data$Netherlands # covid data of NL
infected_cumulative <- covid_data$confirmed # cumulative number of infected individuals
names(infected_cumulative) <- covid_data$t # dates
epid <- diff(infected_cumulative) # daily number of infected individuals
population_size <- covid_data$population

# create incid vector
incid <- epid[which(epid!=0)[1]:length(epid)]
t <- as.Date(names(incid))

# plot daily new cases
ma <- function(x, n = 7){filter(x, rep(1 / n, n), sides = 2)}
smooth_incid <- ma(incid)
par(mfrow=c(1,1), mar=c(5.1, 4.1, 4.1, 2.1))
plot(t, incid, type='h', 
     xaxt='n', xlab = '', ylab = 'Number of infected people',
     main = 'Number of people infected with covid-19 in the Netherlands over time')
t_steps <- 60
axis(1, t[seq(1,length(t),t_steps)], format(t[seq(1,length(t),t_steps)], "%d %b %y"), cex.axis = .85)
lines(t, smooth_incid, col=1, lwd=3)
legend("topleft", legend = c("Daily number of infected people", "7-day average of infected people"), 
       lty=c(1,1), col=c(1,1), lwd=c(1,3))