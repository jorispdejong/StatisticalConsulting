# clear global environment
rm(list = ls(all.names = TRUE))

# reproducibility
set.seed(6)

# source
source('scripts/generation time/Functions.R')

##########################
### EXPERIMENT CONTROL ###
##########################
run_simulation <- F
trunctation <- F
exp_distr <- "weibull" # choose from "lognormal", "gamma", "weibull"
if(trunctation){
 if(exp_distr == "lognormal"){
   fit_file_path <- "output/generation time/fit_lognormal_with_truncation.RData"
 }else if(exp_distr == "gamma"){
   fit_file_path <- "output/generation time/fit_gamma_with_truncation.RData"
 }else if(exp_distr == "weibull"){
   fit_file_path <- "output/generation time/fit_weibull_with_truncation.RData"
 }
}else{
  if(exp_distr == "lognormal"){
    fit_file_path <- "output/generation time/fit_lognormal_no_truncation.RData"
  }else if(exp_distr == "gamma"){
    fit_file_path <- "output/generation time/fit_gamma_no_truncation.RData"
  }else if(exp_distr == "weibull"){
    fit_file_path <- "output/generation time/fit_weibull_no_truncation.RData"
  }
}

##################################
### IMPORT AND PREPROCESS DATA ###
##################################
df <- preprocessData()

#######################
### STAN SIMULATION ###
#######################
if(run_simulation){
  options(mc.cores = parallel::detectCores())
  rstan::rstan_options(auto_write = TRUE)
  
  # data
  if(!trunctation){
    stan_data <- list(N=nrow(df),E_L=df$EL, E_R=df$ER, S_L=df$SL, S_R=df$SR)
  }else{
    stan_data <- list(N=nrow(df),E_L=df$EL, E_R=df$ER, S_L=df$SL, S_R=df$SR, 
                      r=0.14, upper_bound=as.numeric(df$tstar[1]))
  }
  
  if(!trunctation){
    if(exp_distr == "lognormal"){
      init_fun <- function(...) list(e_raw=rep(0.5,nrow(df)), 
                                     s_raw=rep(0.5,nrow(df)), 
                                     logmean_SI = log(mean(df$dist)), 
                                     logsd_SI = log(sd(df$dist)))
    }else if(exp_distr == "gamma"){
      init_fun <- function(...) list(e_raw=rep(0.2,nrow(df)), 
                                     s_raw=rep(0.8,nrow(df)), 
                                     param1 = (mean(df$dist)/sd(df$dist))^2,
                                     param2 = mean(df$dist)/(sd(df$dist)^2))
    }else if(exp_distr == "weibull"){
      init_fun <- function(...) list(e_raw=rep(0.5,nrow(df)), 
                                     s_raw=rep(0.5,nrow(df)), 
                                     logmean_SI = log(mean(df$dist)),
                                     param1 = 1.75)
    }
  }else{
    if(exp_distr == "lognormal"){
      init_fun <- function(...) list(e_raw=rep(0.2,nrow(df)), 
                                     s_raw=rep(0.8,nrow(df)), 
                                     logmean_SI = log(mean(df$dist)),
                                     logsd_SI = log(sd(df$dist)))
    }else if(exp_distr == "gamma"){
      init_fun <- function(...) list(e_raw=rep(0.2,nrow(df)), 
                                     s_raw=rep(0.8,nrow(df)), 
                                     mean_SI = mean(df$dist),
                                     sd_SI = sd(df$dist))
    }else if(exp_distr == "weibull"){
      init_fun <- function(...) list(e_raw=rep(0.5,nrow(df)), 
                                     s_raw=rep(0.5,nrow(df)), 
                                     logmean_SI = log(mean(df$dist)),
                                     param1 = 1.75)
    }
  }
  
  # stan model file path
  stan_model_file_path <- paste0("data/generation time/stan models/",
                                 exp_distr,'_',
                                 ifelse(trunctation, "with_truncation.stan","no_truncation.stan"))
  
  # stan fit
  fit = rstan::stan(file = stan_model_file_path, 
                    data = stan_data, 
                    algorithm = 'HMC',
                    cores = 4,
                    init = init_fun,
                    verbose = T)
}

# show results
fit

# compute WAIC
fit_extract <- rstan::extract(fit)
waic_fit <- loo::waic(fit_extract$log_likelihood)

