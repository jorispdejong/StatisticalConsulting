# clear global environment
rm(list = ls(all.names = TRUE))

# reproducibility
set.seed(6)

# source
source('scripts/reproduction number/DataPreprocessing.R')
source('scripts/reproduction number/Functions.R')

# libraries
suppressPackageStartupMessages(library('EpiEstim'))

#############################
### LOAD AND PREPARE DATA ###
#############################
# import data
global_covid_data <- importData(preprocess_data = F, download_from_web = F)

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

####################################
### GENERATION TIME DISTRIBUTION ###
####################################
# lognormal: 4.7, 2.9 (complete data set with right truncation)
# weibull: 4.8, 2.3 (only most reliable sources with right truncation)
lognormal <- T
config <- make_config(list(
  mean_si = if(lognormal) 4.7 else 4.8,
  std_si = if(lognormal) 2.9 else 2.3,
  si_parametric_distr = if(lognormal) 'L' else 'W'))

##################
### ESTIMATE R ###
##################
## Estimate R with assumptions on serial interval
res <- estimate_R(incid, method = "parametric_si", config = config)
Rt <- res$R$`Mean(R)`
Rt_lower <- res$R$`Quantile.0.05(R)`
Rt_upper <- res$R$`Quantile.0.95(R)`
Rt_t <- t[res$R$t_start]

####################
### PLOT RESULTS ###
####################
par(mfrow=c(1,1), mar=c(5.1, 5.1, 4.1, 2.1))
cols <- c('black', addTrans('black',100), 'red', addTrans('red',100))
plot(Rt_t, Rt, type = 'l', col = cols[1],
     xlim = c(t[1], t[length(t)]), ylim=c(0,4),
     xlab = 't', ylab = 'R(t)',
     main = 'Effective Reproduction Number (EpiEstim package)',
     xaxt='n', cex.lab=1.3)
axis(side = 1, at = seq(t[1],t[length(t)],50), 
     labels = format(t[seq(1,length(t),50)], "%d %b %y"))
polygon(c(Rt_t,rev(Rt_t)), c(Rt_lower, rev(Rt_upper)), 
        col = cols[2], border = NA)
lines(Rt_t, Rt, lwd=2, col=cols[1])
abline(h=1, lty=2)
rivm <- getReproductionValueFromRIVM()
rivm_t <- as.Date(rivm$Date)
rivm_Rt <- rivm$Rt_avg
rivm_Rt_lower <- rivm$Rt_low
rivm_Rt_upper <- rivm$Rt_up
lines(rivm_t, rivm_Rt, col=cols[3], lwd=2)
polygon(c(rivm_t,rev(rivm_t)), c(rivm_Rt_lower, rev(rivm_Rt_upper)), 
        col = cols[4], border = NA)
legend('topright',
       legend = c('R(t) (EpiEstim)', '95% confidence interval (EpiEstim)', 'R(t) (RIVM)', '95% confidence interval (RIVM)'), 
       col=cols, lwd=c(3,NA,3,NA), lty=c(1,NA,1,NA),
       pch=c(NA,15,NA,15), cex=1.25)

