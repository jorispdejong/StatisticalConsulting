# clear global environment
rm(list = ls(all.names = TRUE))

# reproducibility
set.seed(6)

# source
source('Functions.R')
source("TimeVaryingPoissonSIRModelFunctions.R")

#################
### LOAD DATA ###
#################
# RData file path
rdata_file_path <- 'data/RData/global_covid_data.RData'
# the function 'prepocessData' exports an .RData file with the name 'global_covid_data'
if(!file.exists(rdata_file_path)) preprocessData(rdata_file_path=rdata_file_path, verbose = F)
# load RData
load(rdata_file_path)

#############
### MODEL ###
#############
# compute model
out <- tvpSIR(country="Afghanistan",esp=.5,P=0,p=8,q=5)

# plot results
par(mfrow=c(2,1), mar=rep(2,4))
times <- as.Date(out$all$newtime)
plot(times[1:(length(times)-1)], diff(out$all$obs.i), type = 'l')
plot(times, out$all$R0, type = 'l')
lines(times, rep(1, length(times)), lty=2)
