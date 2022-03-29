##################################################################
# Bayesian repeated measures analysis (one within factor) with ###
# missing data                                                 ###
#                                                              ###
# A design analyses for the project re-buildging trust         ###
# assuming for small effect according Cohen (1988)             ###
##################################################################

library(bain)
library(psych)
library(MASS)
library(mice)
library(tidyverse)
library(hrbrthemes)
library(data.table)

sim_n <- 1000   # number of studies to simulate
true_d <- .3    # size of Cohen's d if mean_i != mean_j

# initialize data frame to store results in
sim_results_total <- tibble(
  true_hyp = character(),
  study_iteration = integer(),
  numerator = character(),
  denominator = character(),
  BF = numeric(),
  N = numeric())


##################################################################
## Loop over N
for(N in c(150)){

##################################################################
## Loop over study
for(study_iteration in 1:sim_n){

##################################################################
## Loop over true effects

for(true_eff in c("nonosp=nonvis=visosp",
                  "nonosp<nonvis<visosp",
                  "nonosp<nonvis=visosp",
                  "nonosp,nonvis,visosp")){                     


# Generate the data ##################################  
data <- data.frame(mvrnorm(n=N, 
                           mu = if(true_eff == "nonosp=nonvis=visosp") 
                                c(0,0,0) else
                                if(true_eff == "nonosp<nonvis<visosp") 
                                c(-true_d,0,true_d) else 
                                if(true_eff == "nonosp,nonvis,visosp")
                                c(true_d, 0, -true_d) else  
                                c(-true_d,0,0),        
                           Sigma = matrix(c( 1, .5, .3,
                                            .5,  1, .5,
                                            .3, .5,  1),                   
                                          3, 3)))
names(data) <- c("nonosp","nonvis","visosp")

# Generate missing values
data$nonosp[(0*floor(N/3)+1):(1*floor(N/3))] <- NA    
data$nonvis[(1*floor(N/3)+1):(2*floor(N/3))] <- NA    
data$visosp[(2*floor(N/3)+1):(3*floor(N/3))] <- NA    

# Impute the data multiply ###########################
 M <- 100 # number of imputed data sets
 out <- mice(data = data, m = M,
             meth=c("norm","norm","norm"), 
             diagnostics = FALSE, 
             printFlag = FALSE)
 
 # Set up the matrices for the estimates ##############
 mulest <- matrix(0,nrow=M,ncol=3) # setup of matrices 
                                   # to store multiple estimates
 covwithin <- matrix(0,nrow=3,ncol=3) # and covariance matrices
 
 
 # Estimate the coefficients for each data frame ######
 for(i in 1:M) {
  within <- lm(cbind(nonosp,nonvis,visosp)~1, # estimate the means 
               data=mice::complete(out,i))    # of the three variables
  mulest[i,]<-coef(within)[1:3]           # store these means in 
                                          # the matrix `mulres`
  covwithin<-covwithin + 1/M * vcov(within)[1:3,1:3]  # compute the  
 }                              # average of the covariance matrices
 
 
 # Compute the average of the estimates ###############
 estimates <- colMeans(mulest)
 names(estimates) <- c("nonosp","nonvis","visosp")
 covbetween <- cov(mulest) # is this the between covariance matrix?
 covariance <- covwithin + (1+1/M)*covbetween # is this the 
                                              # total variance?
 
 
 # Determine the effective and real sample sizes ######
 samp <- nrow(data) # real sample size
 nucom<-samp-length(estimates)
 
 # corresponds to Equation (X) in Hoijtink, Gu, Mulder, & Rosseel (2019)...
 lam <- (1+1/M)*(1/length(estimates))* 
   sum(diag(covbetween %*% ginv(covariance))) # ... (43)
 nuold<-(M-1)/(lam^2) # ... (44)
 nuobs<-(nucom+1)/(nucom+3)*nucom*(1-lam) # ... (46)
 nu<- nuold*nuobs/(nuold+nuobs) # ... (47)
 fracmis <- (nu+1)/(nu+3)*lam + 2/(nu+3) # ... (48)
 neff<-samp-samp*fracmis
 # coerce `covariance` to a list
 covariance<-list(covariance)
 
 
 # Test the hypotheses with bain ######################
 results <- bain(estimates,
              "nonosp=nonvis=visosp;nonosp<nonvis<visosp;nonosp<nonvis=visosp",
              n=neff,Sigma=covariance,    
              group_parameters=3,joint_parameters = 0)
 
 sim_result <- tibble(true_hyp = true_eff,
                     study_iteration = study_iteration,
                     numerator = results$hypotheses,
                     denominator = "Hc",
                     BF = results$fit$BF[1:3],
                     N = nrow(data))%>%
  full_join(tibble(true_hyp = true_eff,
                   study_iteration = study_iteration,
                   numerator = results$hypotheses,
                   `nonosp=nonvis=visosp` = results$BFmatrix[, 1],
                   `nonosp<nonvis<visosp` = results$BFmatrix[, 2],
                   `nonosp<nonvis=visosp` = results$BFmatrix[, 3],
                   N = nrow(data))%>%
              gather(denominator, BF, 
                     `nonosp=nonvis=visosp`, 
                     `nonosp<nonvis<visosp`, 
                     `nonosp<nonvis=visosp`))%>%  
  filter(numerator != denominator)
 
 
 sim_results_total <- full_join(sim_results_total, sim_result)
}
  print(paste(N, study_iteration, sep = "_"))
 }
 }

write_csv(sim_results_total, "sim_results_total_bfda_badgestudy.csv")

