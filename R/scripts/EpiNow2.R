# packages
# install.packages(c("data.table", "remotes", "EpiNow2"))		

# clear global environment
rm(list = ls(all.names = TRUE))

# reproducibility
set.seed(6)

# source
source('scripts/DataPreprocessing.R')

# libraries
# suppressPackageStartupMessages(library('data.table'))
suppressPackageStartupMessages(library('EpiNow2'))

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
reported_cases <- data.frame('date'=t,'confirm'=epid)

####################################
### DEFINE SIMULATION PARAMETERS ###
####################################
# set number of cores to use fitting the model
# no benefit on runtime if cores > chains which is set to 4 by default
options(mc.cores = 4)

# literature distributions - please reach out if there are others you think should be supported
generation_time <- get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
incubation_period <- get_incubation_period(disease = "SARS-CoV-2", source = "lauer")

# define reporting delay as lognormal with mean of 4 days and sd of 1 day in absence of
# evidence. If data on onset -> report then can use estimate_delay to estimate the delay
reporting_delay <- list(mean = convert_to_logmean(4, 1),
                        mean_sd = 0.1,
                        sd = convert_to_logsd(4, 1),
                        sd_sd = 0.1,
                        max = 15)

##################
### ESTIMATE R ###
##################
# estimate Rt and nowcast/forecast cases by date of infection
# Example configurations are here: https://epiforecasts.io/EpiNow2/dev/reference/estimate_infections.html
out <- epinow(reported_cases = reported_cases, 
              generation_time = generation_time,
              delays = delay_opts(incubation_period, reporting_delay),
              rt = rt_opts(prior = list(mean = 1.5, sd = 0.5)),
              # here we define the quality of the gaussian process approximation
              # if the fit to data appears poor try increasing basis_prop and
              # potentially the boundary_scale (see ?gp_opts for details)
              # though this will likely increase runtimes.
              gp = gp_opts(basis_prop = 0.2),
              # in some instances stan chains can get stuck when estimating in 
              # these instances consider using the future fitting mode by passing 
              # `future = TRUE, max_execution_time = 60 * 30` to stan_opts and calling 
              # `future::plan("multiprocess")` prior to running epinow this will time out
              # chains after 30 minutes but still return results from completed chains
              stan = stan_opts(),
              horizon = 14,
              target_folder = 'output/reproduction number/EpiNow2/latest',
              logs = file.path('output/reproduction number/EpiNow2/latest', Sys.Date()),
              return_output = TRUE, 
              verbose = TRUE)

####################
### PLOT RESULTS ###
####################
plot(out)
