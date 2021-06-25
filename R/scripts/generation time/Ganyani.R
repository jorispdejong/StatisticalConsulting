# clear global environment
rm(list = ls(all.names = TRUE))

# reproducibility
set.seed(6)

# We first read the data from the Tianjin cluster in China.
data <- read.csv("data/generation time/Tianjin cluster cases (updated Feb 29).csv", 
                 header=T)

# We convert the symptom onset dates to the correct format.
data$symptomOnset <- as.Date(data$symptom_onset, "%d/%m/%Y")

# We store the CaseID and difference between onset times with the first onset time in a seperate vector.
Case <- data$CaseID
Time <- data$symptomOnset - min(data$symptomOnset, na.rm=T) 

# We want to determine the possible cases.
# Here the information from the first source column is used.
reportedNegSI <- rep(F, length(data$CaseID))
for (i in data$CaseID){
  all.sources <- as.numeric(unlist(strsplit(as.character(data$Source[i]),","))) 
  if(!all.sources[1]==0){
    all.sources.neg <- c()
    for (k in all.sources){
      if((data$symptomOnset[i]-data$symptomOnset[k])<0){
        all.sources.neg <- c(all.sources.neg,k)  
      }  
    }  
    if(!is.null(all.sources.neg)){
      reportedNegSI[i] <- T 
    }
  }
}

# Here the information from the second source column is used.
vi.list <- list()
for(i in which(!reportedNegSI==T)){
  all.sources <- as.numeric(unlist(strsplit(as.character(data$Source2[i]),",")))   
  if(all.sources[1]==0){
    vi.list[[i]] <- all.sources  
  }else{
    all.sources.pos <- c()
    for (k in all.sources){
      if((data$symptomOnset[i]-data$symptomOnset[k])>=0){
        all.sources.pos <- c(all.sources.pos,k)  
      }  
    }  
    if(!is.null(all.sources.pos)){
      vi.list[[i]] <- all.sources.pos 
    }else{
      vi.list[[i]] <- 0 
    }
  }
}

for(i in which(reportedNegSI==T)){
  vi.list[[i]] <- as.numeric(unlist(strsplit(as.character(data$Source[i]),","))) 
}

NCases  <- nrow(data)
PossibleInfector <- matrix(nrow=NCases,ncol=max(lengths(vi.list)))

# Here we store all of the possible infectors in a matrix.
for(i in 1:NCases){
  PossibleInfector[i,1:lengths(vi.list)[i]] <- vi.list[[i]]
}

# Here we determine which of the cases do and don't play a role in the calculation of the likelihood.
# The cases which were infected by someone outside of the data set in, for example, Wuhan do not contribute as their GT and SI can't be studied.
NPossibleInfector <- rowSums(!is.na(PossibleInfector))
IsNotContributorToLikel <-  c(which(PossibleInfector[,1]==0))
IsContributorToLikel <- c(which(PossibleInfector[,1]!=0))


##################
# We initialize an empty serial interval array.
SI <- c()
for(b in 1:5000){
  # The network is initialized as an empty vector.
  Network <- numeric(NCases)+0
  # We now store a vector with all of the contributors to the likelihood.
  Update <- IsContributorToLikel
  # We sample a random vector from the indices of the possible infectors for each case.
  Draw <- round(runif(length(Update),min=0.5,max=NPossibleInfector[Update]+0.5))
  # We then store these infectors as the true ones in the network.
  for(i in 1:length(IsContributorToLikel)){
    Network[Update[i]] <- PossibleInfector[Update[i],Draw[i]]
  }
  # The serial intervals are determined for the network in the current iteration.
  SI<- c(SI, Time[IsContributorToLikel] - Time[Network[IsContributorToLikel]])
}
# A histogram is made of the SIs generated in the 5000 iterations.
hist(SI, ylab = "Frequency", main = "", yaxt='n')


# likelihood
likelihood <- function(){
  alpha<-c(); Beta<-c() 
  # alpha[1] & Beta[1] are shape & rate of the serial interval distriution  
  alpha[1] <- theta[1]^2/theta[2] # shape = mean^2/var
  Beta[1]  <- theta[1]/theta[2]; 	# rate = mean/var
  # alpha[2] & Beta[2] are shape & rate of the distribution of sum of 2 independent gamma variables
  alpha[2] <- theta[3]^2/theta[4] # shape = mean^2/var
  Beta[2]  <- theta[3]/theta[4]   # rate = mean/var
  # evaluate density
  f_Z <- function(Z){
    monteCarloN <- 300
    delta_i     <- rgamma(monteCarloN, shape = alpha[2], rate = Beta[2])
    delta_j     <- rgamma(monteCarloN, shape = alpha[2], rate = Beta[2])
    Y           <- delta_i-delta_j  
    Z_Y         <- Z - Y
    return(mean(dgamma(Z_Y, shape=alpha[1], rate=Beta[1], log=F)))
  }
  SerialInterval   <- Time[IsContributorToLikel] - Time[Network[IsContributorToLikel]] # serial interval
  return(sum(log(1e-50+sapply(SerialInterval, function(x) f_Z(x)))))
}

# prior
prior <- function(){
  alpha<-c(); Beta<-c()   
  alpha[1] <- theta[1]^2/theta[2] # shape = mean^2/var
  Beta[1]  <- theta[1]/theta[2]; 	# rate = mean/var  
  alpha.prior <-  dunif(alpha[1], 0, 30)
  Beta.prior  <-  dunif(Beta[1], 0, 20)
  return(log(1e-50+alpha.prior)+log(1e-50+Beta.prior))
}

# posterior
posterior <- function(){
  return (likelihood()+prior())
}

#--------------------------------------------------
#----MCMC---------------------
#--------------------------------------------------
# An empty network is initalized
Network <- numeric(NCases)+0
Update <- IsContributorToLikel
Draw <- round(runif(length(Update),min=0.5,max=NPossibleInfector[Update]+0.5))
for(i in 1:length(IsContributorToLikel)){
  # Possible infectors are assigned to each infected case.
  Network[Update[i]] <- PossibleInfector[Update[i],Draw[i]]
}
AcceptedNetwork <- Network

# The parameters for the incubation time are fixed (can be left variable).
m.ll <- 5.2; m.ul <- 5.2 # lower and upper limit for mean 
v.ll <- 2.8^2; v.ul <- 2.8^2 # lower and upper limit for variance

# The mean and variance for both gamma distributions are initialized.
AcceptedTheta=theta <- c(1, 1, m.ll, v.ll) 

P <- posterior()
AcceptedP <- P

# We set the parameters for the MCMC simulation.
NRuns <- 3000000
NUpdate <- length(IsContributorToLikel)
Burnin <- 100000
Thinning <- 200
SaveP <- numeric()
# Create arrays to store the sampled networks and parameters.
SaveNetwork <- matrix(nrow=NCases,ncol=(NRuns-Burnin)/Thinning)
Savetheta <- matrix(nrow=(NRuns-Burnin)/Thinning,ncol=(2+length(theta)))

# Some tuning parameters for the proposed steps
anetwork=asd<-0
tuning <- c(0.5, 0.5)
a <- 0

# We now run the MCMC simulation.
progressbar <- txtProgressBar(min = 0, max = NRuns, style = 3)
for(b in 1:NRuns){
  if(b%%2 != 0){
    # A new network is sampled.
    theta <- AcceptedTheta
    Update <- IsContributorToLikel
    Draw <- round(runif(length(Update),min=0.5,max=NPossibleInfector[Update]+0.5))
    for(i in 1:NUpdate){ 
      Network[Update[i]] <- PossibleInfector[Update[i],Draw[i]]
    }
  }
  
  if(b%%2 == 0){
    # New theta 1 and theta 2 are sampled using a uniform distribution on the interval determined by the tuning
    Network <- AcceptedNetwork
    theta[1] <- runif(1, (AcceptedTheta[1]-tuning[1]), (AcceptedTheta[1]+tuning[1]))
    if(theta[1]<0){theta[1] <- AcceptedTheta[1]}
    
    theta[2] <- runif(1, (AcceptedTheta[2]-tuning[2]), (AcceptedTheta[2]+tuning[2]))
    if(theta[2]<0){ theta[2] <- AcceptedTheta[2]}
    # The fixed incubation variance and mean are stored in theta.
    theta[3] <- runif(1, m.ll, m.ul)
    theta[4] <- runif(1, v.ll, v.ul)
  }
  
  P <- posterior()
  
  # Check whether or not we accept the proposed move using posterior probabilities.
  AcceptYN <- runif(1,min=0,max=1) <= exp(P-AcceptedP)
  if(AcceptYN){
    if(b%%2 != 0){
      anetwork <- anetwork + 1
      AcceptedNetwork <- Network
    }
    if(b%%2 == 0){
      asd <- asd+1
      AcceptedTheta <- theta
    }
    AcceptedP <- P
  }
  # The values are stored if we have passed the burnin fase and if it is a multiple of the 200th iteration.
  if(b%%Thinning == 0 & b>Burnin){
    a <- a + 1
    # Sample a mean difference in incubation time between the infector and infectee
    monteCarloN <- 300
    delta_i     <- rgamma(monteCarloN, shape = AcceptedTheta[1]^2/AcceptedTheta[2], rate = AcceptedTheta[1]/AcceptedTheta[2])
    delta_j     <- rgamma(monteCarloN, shape = AcceptedTheta[1]^2/AcceptedTheta[2], rate = AcceptedTheta[1]/AcceptedTheta[2])
    Y           <- delta_i-delta_j
    # The distribution of the Serial Interval is determined using standard techniques from statistics.
    Savetheta[a,] <- c(AcceptedTheta, AcceptedTheta[1]+mean(Y), (AcceptedTheta[2]+2*AcceptedTheta[4]))
    SaveNetwork[,a] <- AcceptedNetwork
    SaveP[a] <- AcceptedP
  }
  setTxtProgressBar(progressbar, b)
}
close(progressbar)

# Plots of the Markov trajectory of theta_1.
par(mfrow=c(1,2))
plot(Savetheta[,1], type="l", col=4, main="Trajectory of Mean Generation Interval")
plot(Savetheta[,2], type="l", col=4, main="Trajectory of the Variance of the Generation Interval.")

# Histograms of the parameters.
hist(Savetheta[,1], main="Histogram of Mean Generation Interval.")
hist(Savetheta[,2], main = "Histogram of the Variance of the Generation Interval.")


# The mean and quantiles of the mean and standard deviation of the Generation Interval.
mean(Savetheta[,1], na.rm = T); quantile(Savetheta[,1], c(0.025, 0.5, 0.975))#; sd(Savetheta[,1], na.rm = T)
mean(Savetheta[,2]^0.5, na.rm = T); quantile(Savetheta[,2]^0.5, c(0.025, 0.5, 0.975))#; sd(Savetheta[,2], na.rm = T)

# The mean and quantiles of the mean and standard deviation of the Serial Interval.
mean(Savetheta[,5], na.rm = T); quantile(Savetheta[,5], c(0.025, 0.5, 0.975))#; sd(Savetheta[,5], na.rm = T)
mean(Savetheta[,6]^0.5, na.rm = T); quantile(Savetheta[,6]^0.5, c(0.025, 0.5, 0.975))#; sd(Savetheta[,5], na.rm = T)

