# clear global environment
rm(list = ls(all.names = TRUE))

suppressPackageStartupMessages(library('EpiEstim'))

# check simplest version of EpiEstim works well 

## load data on pandemic flu in a school in 2009
data("Flu2009")

## estimate the reproduction number (method "non_parametric_si")
## when not specifying t_start and t_end in config, they are set to estimate
## the reproduction number on sliding weekly windows                          
res <- estimate_R(incid = Flu2009$incidence, 
                  method = "non_parametric_si",
                  config = make_config(list(si_distr = Flu2009$si_distr)))
plot(res)

plot(Flu2009$si_distr, type = 'l')

