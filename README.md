# README
## Estimation of the Reproduction Number and Generation TimeDistribution and Serial Interval Distribution
This repository was instigated by a project for the Mathematics Master's course Statistical Consulting.
The covid-19 epidemic requirs major decisions of governments and other institutions. One of the big influences on those decisions is the reproduction number, which is the the average numberof secondary infections caused by a primary case. This so-called R value determines how fast the virus is spreading. In addition, for the estimation of R, the so-called generation time plays an important role. The generation time is the time between the infection of a primary and secondary case. 

In this repository, both the reproduction number and the generation time distribution are estimated by an R implementation. The estimation of the reproduction number of three R packages (R0, EpiEstim, and EpiNow2) is implemented. The estimation of the generation time distribution is implemented according to both the paper by Ganyani et al. and the paper by Nishiuraa et al.

The code can be found in the _R/scripts_ folder. If all the required packages are installed, any script should be able to run without any configuration. The data is taken from the _R/data_ folder and the output is generated and save in the _R/output_ folder. 

The papers that were used as the foundation of the code in this repository can be found in the _References_ folder

For more information, see the complementary report.
