# clear global environment
rm(list = ls(all.names = TRUE))

# reproducibility
set.seed(6)

# source
source('scripts/reproduction number/DataPreprocessing.R')
source('scripts/reproduction number/Functions.R')

# libraries
suppressPackageStartupMessages(library('EpiNow2'))

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
epid <- infected_daily[(tail(which(infected_daily==0),1)+1):length(infected_daily)]
t <- covid_data$t[(tail(which(infected_daily==0),1)+1):length(infected_daily)]
reported_cases <- data.frame('date'=t,'confirm'=epid)

load_results_from_file <- T
if(load_results_from_file){
  load('output/reproduction number/EpiNow2/out.RData')
}else{
  # set number of cores to use fitting the model
  # no benefit on runtime if cores > chains which is set to 4 by default
  options(mc.cores = 4)
  
  # generation time from ganyani
  generation_time <- get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
  
  # set delays between infection and case report
  incubation_period <- get_incubation_period(disease = "SARS-CoV-2", source = "lauer")
  
  # reporting delays
  reporting_delay <- list(
    mean = convert_to_logmean(2, 1), mean_sd = 0.1,
    sd = convert_to_logsd(2, 1), sd_sd = 0.1, max = 10
  )
  
  # estimate Rt
  out <- epinow(reported_cases = reported_cases,
                generation_time = generation_time,
                delays = delay_opts(incubation_period, reporting_delay),
                rt = NULL, backcalc = backcalc_opts(),
                obs = obs_opts(scale = list(mean = 0.4, sd = 0.05)),
                horizon = 0,
                target_folder = 'output/reproduction number/EpiNow2/latest',
                logs = file.path('output/reproduction number/EpiNow2/latest', Sys.Date()),
                return_output = TRUE)
  save(out, file='output/reproduction number/EpiNow2/out.RData')
}

####################
### PLOT RESULTS ###
####################
epinow2_plot <- F
if(epinow2_plot){
  plot(out)
}else{
  Rt_t <- out$plots$R$data$date
  Rt <- out$plots$R$data$mean
  Rt_lower <- out$plots$R$data$lower_90
  Rt_upper <- out$plots$R$data$upper_90
  
  par(mfrow=c(1,1), mar=c(5.1, 5.1, 4.1,2.1))
  cols <- c('black', addTrans('black',100), 'red', addTrans('red',100))
  plot(Rt_t, Rt, type = 'l', col = cols[1],
       xlim = c(t[1], t[length(t)]), ylim=c(0,4),
       xlab = 't', ylab = 'R(t)',
       main = 'Effective Reproduction Number (EpiNow2 package)',
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
         legend = c('R(t) (EpiNow2)', '95% confidence interval (EpiNow2)', 'R(t) (RIVM)', '95% confidence interval (RIVM)'), 
         col=cols, lwd=c(3,NA,3,NA), lty=c(1,NA,1,NA),
         pch=c(NA,15,NA,15), cex=1.25)
}
