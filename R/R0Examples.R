# clear global environment
rm(list = ls(all.names = TRUE))

# reproducibility
set.seed(6)

# source
source('Functions.R')

### Load and prepare data ##
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

###################
### check.incid ###
###################
# checks the validity of the data and outputs an incidents object
incid <- check.incid(infected_daily)

#######################
### generation.time ###
#######################
# source = Serial interval of novel corona virus infections
GT <- generation.time(type = 'weibull', val = c(4.8,2.3)) # val = c(mean,sd)

##################
### estimate.R ###
##################
ma <- function(x, n = 7){filter(x, rep(1 / n, n), sides = 2)}
smooth_epid <- ma(epid)
plot(as.Date(names(epid)), epid, type='l')
lines(as.Date(names(epid)), smooth_epid, col=2, lwd=3)


estimate_R <- estimate.R(epid = smooth_epid[52:length(epid)], GT = GT, pop.size = population_size, 
                         methods="TD")

par(mfrow=c(1,1))
R_TD <- estimate_R$estimates$TD$R
plot(as.Date(names(R_TD)), R_TD[20:270], type = 'l')

