# clear global environment
rm(list = ls(all.names = TRUE))

# reproducibility
set.seed(6)

# source
source('HelperFunctions.R')
source('Functions.R')

#############################
### LOAD AND PREPARE DATA ###
#############################
# loading
rdata_file_path <- 'data/RData/global_covid_data.RData' # RData file path
# the function 'prepocessData' exports an .RData file with the name 'global_covid_data'
if(!file.exists(rdata_file_path)) preprocessData(rdata_file_path=rdata_file_path, verbose = F)
load(rdata_file_path) # load RData

# import and preprocess data
covid_data <- global_covid_data$Afghanistan
I <- covid_data$confirmed
R <- covid_data$recovered
D <- covid_data$deaths
S <- covid_data$population - I - R - D
IRD_raw <- data.frame('S'=S, 'I'=I, 'R'=R, 'D'=D)
IRD <- IRD_raw[(which(IRD_raw[,2]!=0)[1]):nrow(IRD_raw),]
X <- as.matrix(IRD)
dT <- as.numeric(diff(covid_data$t))
dX <- get.dX(X[,-1])
V <- get.V()[-1,]
M <- get.M(V, head(X,-1), dT)

##################################
### IWLS over the whole period ###
##################################
theta_k <- as.numeric(solve(t(M)%*%M)%*%t(M)%*%dX) ## initial OLS estimate
theta_k <- iwls(theta_0 = theta_k,V,M,dX,dT, verbose=T) ## get the IWLS estimate
names(theta_k) <- colnames(V)
R0 <- theta_k["S->I"]/(theta_k["I->R"] + theta_k["I->D"]) ## get the basic reproduction number R0

##########################################################
### IWLS over an increasing surveillance period [1,Tf] ###
##########################################################
R0s <- rep(NA, nrow(X)-1)
# create progress bar
pb <- txtProgressBar(2, nrow(IRD), style = 3)
for(Tf in 2:nrow(IRD)){
  X <- as.matrix(IRD)[1:Tf,] ## select counts from time 1 up to time Tf
  colnames(X) <- c("S", "I", "R", "D")
  
  dX <- get.dX(X[,-1]) ## get the increments
  M <- get.M(V,head(X,-1),dT) ## get the design matrix
  
  theta_k <- as.numeric(solve(t(M) %*% M) %*% t(M) %*% dX) ## initial OLS estimate
  theta_k <- iwls(theta_0 = theta_k, V, M, dX, dT) ## get the IWLS estimate
  names(theta_k) <- colnames(V)
  
  R0s[Tf-1] <- theta_k["S->I"]/(theta_k["I->R"] + theta_k["I->D"]) ## get the basic reproduction number R0
  
  setTxtProgressBar(pb, Tf) # update progress bar
}

plot(R0s, type = 'l')
plot(all_t[2:length(all_t)], as.numeric(all_C[98,7:ncol(all_C)]-all_C[98,6:(ncol(all_C)-1)]), type='l')


