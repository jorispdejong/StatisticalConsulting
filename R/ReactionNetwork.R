# clear global environment
rm(list = ls(all.names = TRUE))

# reproducibility
set.seed(6)

# libraries
library(Matrix)
library(MASS)

# source
source("HelperFunctions.R")

#################
### load data ###
#################
IRD <- read.csv("data/dpc-covid19-ita-andamento-nazionale.csv", sep=",", header=T)
IRD$S <- 60.36*1e6 ## set the number of susceptibles equal to the total population size

##########################
### data preprocessing ###
##########################
## ’totale_casi ’ is the cumulative number of infectious individuals over time .
## Therefore we defined the current number of infectious individuals as:
IRD$totale_casi <- IRD$totale_casi - IRD$deceduti - IRD$dimessi_guariti
## Then , we need to update the population size S so as to ensure the total population N being constant over time .
IRD$S <- IRD$S - IRD$totale_casi - IRD$deceduti - IRD$dimessi_guariti
IRD <- IRD[, c("S", "totale_casi", "dimessi_guariti", "deceduti")]
## plot data :
par(mfrow=c(1,1), mar=c(5,5,2,2))
matplot(IRD[,2:4] , type = 'l', xlab = " time ", ylab = "IRD")
legend(x="topleft", legend=c("I","R","D"), col = 1:3 ,lwd=1)

X <- as.matrix(IRD)
colnames(X) <- c("S", "I", "R", "D") ## set short labels to the compartments

dT <- diff(1:nrow(X)) ## in this case we have always a lag =1 time increments
dX <- get.dX(X[,-1]) ## the -1 inside X[ , -1] ensures that we do NOT model the increments of S
V <- get.V()[-1,] ## get the net - effect matrix
M <- get.M(V,head(X, -1),dT) ## get the design matrix M

###############################
## IWLS over the whole period
##
theta_k <- as.numeric(solve(t(M) %*% M) %*% t(M) %*% dX) ## initial OLS estimate
theta_k <- iwls(theta_0 = theta_k, V, M, dX, dT) ## get the IWLS estimate
names(theta_k) <- colnames(V)
R0 <- theta_k["S->I"]/(theta_k["I->R"] + theta_k["I->D"]) ## get the basic reproduction number R0

#########################################################
## IWLS over an increasing surveillance period [1 , Tf]
##
R0s <- rep(NA, nrow(X)-1)
pb <- txtProgressBar(2,nrow(IRD), style = 3)
for(Tf in 2:nrow(IRD)){
  X <- as.matrix(IRD)[1: Tf,] ## select counts from time 1 up to time Tf
  colnames(X) <- c("S","I","R","D")
  
  dX <- get.dX( X[,-1]) ## get the increments
  M <- get.M(V,head(X,-1),dT) ## get the design matrix
  
  theta_k <- as.numeric(solve(t(M) %*% M) %*% t(M) %*% dX) ## initial OLS estimate
  theta_k <- iwls(theta_0 = theta_k,V,M,dX,dT) ## get the IWLS estimate
  names(theta_k) <- colnames(V)
  
  R0s[Tf] <- theta_k["S->I"]/(theta_k["I->R"] + theta_k["I->D"]) ## get the basic reproduction number R0
  
  setTxtProgressBar(pb,Tf)
}

## plot the observed counts for the compartments I, R and D together with the estimated R0 ’s
par(mfrow=c(2,1))
matplot(IRD[,2:4], type = 'l', xlab = "time", ylab = "IRD")
legend("topleft", legend = c("I","R","D"), col = 1:3, lwd=1)
plot(R0s, type = 'l')
abline(h=1, lty=2)

