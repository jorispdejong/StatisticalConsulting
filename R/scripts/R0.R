# clear global environment
rm(list = ls(all.names = TRUE))

# reproducibility
set.seed(6)

# source
source('scripts/DataPreprocessing.R')

# libraries
suppressPackageStartupMessages(library('R0'))

#############################
### LOAD AND PREPARE DATA ###
#############################
preprocess_data <- F
download_from_web <- F
verbose <- T

# import data
global_covid_data <- importData(preprocess_data = preprocess_data, download_from_web = download_from_web, verbose = verbose)

####################
### PREPARE DATA ###
####################
# covid data of the Netherlands
covid_data <- global_covid_data$Netherlands 
infected_cumulative <- covid_data$confirmed 
infected_daily <- diff(infected_cumulative)

# create reported cases data frame
epid <- infected_daily[(tail(which(infected_daily==0),1)+1):length(infected_daily)]
t <- covid_data$t[(tail(which(infected_daily==0),1)+1):length(infected_daily)]
reported_cases <- data.frame('epid'=epid,'t'=t)

####################################
### GENERATION TIME DISTRIBUTION ###
####################################
# lognormal: 4.7, 2.9 (complete data set with right truncation)
# weibull: 4.8, 2.3 (only most reliable sources with right truncation)
lognormal <- T
GT <- generation.time(type = if(lognormal) 'lognormal' else 'weibull',
                      val = if(lognormal) c(4.7,2.9) else c(4.8,2.3))

##################
### ESTIMATE R ###
##################
estimate_R <- est.R0.TD(epid = reported_cases$epid, t = reported_cases$t, GT = GT)
Rt <- estimate_R$R
t <- as.Date(names(Rt))

####################
### PLOT RESULTS ###
####################
plot(estimate_R)

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


