# clear global environment
rm(list = ls(all.names = TRUE))

# reproducibility
set.seed(6)

# source
source('Functions.R')

# libraries
suppressPackageStartupMessages(library('EpiEstim'))
suppressPackageStartupMessages(library('R0'))

#############################
### LOAD AND PREPARE DATA ###
#############################
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

####################################
### GENERATION TIME DISTRIBUTION ###
####################################
# lognormal: 4.7, 2.9 (complete data set with right truncation)
# weibull: 4.8, 2.3 (only most reliable sources with right truncation)

##################
### R0 PACKAGE ###
##################
# source = Serial interval of novel corona virus infections
GT1 <- generation.time(type = 'weibull', val = c(4.8,2.3)) # val = c(mean,sd)
GT2 <- generation.time(type = 'lognormal', val = c(4.7,2.9)) # val = c(mean,sd)
estimate_R_TD1 <- est.R0.TD(epid = incid, GT = GT1)
estimate_R_TD2 <- est.R0.TD(epid = incid, GT = GT2)
R1_R0 <- estimate_R_TD1$R
R2_R0 <- estimate_R_TD2$R
t_R0 <- as.Date(names(R1_R0))

#################
### EPI ESTIM ###
#################
## Estimate R with assumptions on serial interval
res1 <- estimate_R(incid, method = "parametric_si",
                  config = make_config(list(
                    mean_si = 4.8, std_si = 2.3,
                    si_parametric_distr = "W")))
res2 <- estimate_R(incid, method = "parametric_si",
                   config = make_config(list(
                     mean_si = 4.7, std_si = 2.9,
                     si_parametric_distr = "L")))
R1_epi <- res1$R$`Mean(R)`
R2_epi <- res2$R$`Mean(R)`
t_epi <- t[res1$R$t_end]

common_t <- as.Date(intersect(as.character(t_epi),as.character(t_R0)))


##################
### SAME RANGE ###
##################
indices_t_R0 <- which(t_R0 %in% common_t)
R1_R0_good <- R1_R0[indices_t_R0]
R2_R0_good <- R2_R0[indices_t_R0]
t_R0_good <- t_R0[indices_t_R0]

indices_t_epi <- which(t_epi %in% common_t)
R1_epi_good <- R1_epi[indices_t_epi]
R2_epi_good <- R2_epi[indices_t_epi]
t_epi_good <- t_epi[indices_t_epi]

####################
### PLOT RESULTS ###
####################
# plotting R
plot_R <- function(t,R1,R2, ylim=c(0,2.5), t_steps=25, main=''){
  plot(t, R1, type = 'l', ylim = ylim, xaxt='n', xlab = '', ylab = 'R', main = main)
  axis(1, t[seq(1,length(t),t_steps)], format(t[seq(1,length(t),t_steps)], "%d %b %y"), cex.axis = .7)
  lines(t,rep(0.5,length(t)))
  lines(t,rep(1,length(t)), lty=2)
  lines(t,rep(1.5,length(t)))
  lines(t,rep(2,length(t)))
  lines(t, R1, lwd=3, col=4) 
  lines(t, R2, lwd=3, col=7, lty=2)
  legend('topright', legend = c('Weibull (mean=4.8, std=2.3)', 'Lognormal (mean=4.7, std=2.9)'), col=c(4,7), lty = c(1,2), lwd=c(3,3))
}
# plot results for R0 package and EpiEstim package
par(mfrow=c(2,1), mar=c(3,4,3,2))
plot_R(t_R0_good, R1_R0_good, R2_R0_good, t_steps = 40, main = 'R computed with R0 package') # R0
plot_R(t_epi_good, R1_epi_good, R2_epi_good,t_steps = 40, main = 'R computed with EpiEstim package') # EpiEstim

