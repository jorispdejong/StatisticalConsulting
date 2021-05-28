# clear global environment
rm(list = ls(all.names = TRUE))

# reproducibility
set.seed(6)

# library
library("R0") 

###########################
### EXAMPLE GERMANY FLU ###
###########################
# create generation time : gamma distribution, with mean 2.6 time units and standard deviation 1 time unit
GT.flu <- generation.time("gamma", c(2.6,1))

# loads example dataset
data(Germany.1918)
res.R <- estimate.R(Germany.1918, GT=GT.flu, methods=c("EG","ML","SB","TD"))

# applies methods EG, ML, SB, TD to the dataset
plot(res.R) # display results
plotfit(res.R) # displays fit to the epidemic curve

# sensitivity analysis according to choice of time window for exponential growth
sensitivity.analysis(Germany.1918, GT.flu, 
                     begin=1:15, end=16:30, 
                     est.method="EG", sa.type="time") 

# sensitivity analysis according to generation time
sensitivity.analysis(Germany.1918, GT.type="gamma", 
                     GT.mean=seq(1,5,1), GT.sd.range=1, 
                     begin=1, end=27, 
                     est.method="EG", sa.type="GT")

############################
### COVID 19 NETHERLANDS ###
############################
# source functions
source('Functions.R')

### Load and prepare data ###
# loading
rdata_file_path <- 'data/RData/global_covid_data.RData' # RData file path
# the function 'prepocessData' exports an .RData file with the name 'global_covid_data'
if(!file.exists(rdata_file_path)) preprocessData(rdata_file_path=rdata_file_path, verbose = F)
load(rdata_file_path) # load RData

# preparing an example (NL covid data)
covid_data <- global_covid_data$Netherlands # covid data of NL
infected_cumulative <- covid_data$confirmed # cumulative number of infected individuals
names(infected_cumulative) <- covid_data$t # dates
epid <- diff(infected_cumulative) # daily number of infected individuals
population_size <- covid_data$population

# create incidence vector
incid <- epid[which(epid!=0)[1]:length(epid)]
t <- as.Date(names(incid))

# plot daily new cases
ma <- function(x, n = 7){filter(x, rep(1 / n, n), sides = 2)}
smooth_incid <- ma(incid)
par(mfrow=c(1,1), mar=c(5.1, 4.1, 4.1, 2.1))
plot(t, incid, type='l', 
     xaxt='n', xlab = '', ylab = 'Number of infected people',
     main = 'Daily new cases of covid-19 in NL')
t_steps <- 60
axis(1, t[seq(1,length(t),t_steps)], format(t[seq(1,length(t),t_steps)], "%d %b %y"), cex.axis = .7)
lines(t, smooth_incid, col=4, lwd=3)

### Estimate R
GT <- generation.time(type = 'weibull', val = c(4.8,2.3)) # val = c(mean,sd)
res_R <- estimate.R(incid, GT=GT, methods=c("EG","ML","SB","TD"))

# applies methods EG, ML, SB, TD to the data set
plot(res.R) # display results

