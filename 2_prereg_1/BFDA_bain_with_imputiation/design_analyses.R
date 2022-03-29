###############################################################################
# Bayesian repeated measures analysis (one within factor) with missing data ###
#                                                                           ###
# A design analyses for the project re-buildging trust                      ###
# assuming for small effect according Cohen (1988)                          ###
###############################################################################

library(bain)
library(psych)
library(MASS)
library(mice)
library(tidyverse)
library(hrbrthemes)

sim_n <- 5000    # number of studies to simulate
true_d <- 0.3    # size of Cohen's d if mean_i != mean_j

print(Sys.time())
########################################################################################################################
## Loop over N
for(N in c(150, 250, 350)){

##############################################################################################################
## Loop over study
for(study_iteration in 1:sim_n){

#########################################################################################
## Loop over true effects

for(true_eff in c("nonosp=nonvis=visosp",
                  "nonosp<nonvis<visosp",
                  "nonosp=nonvis<visosp")){


# Generate the data ##################################  
data <- data.frame(mvrnorm(n=N, 
                           mu = if(true_eff == "nonosp=nonvis=visosp") 
                                c(0,0,0) else
                                if(true_eff == "nonosp<nonvis<visosp") 
                                c(-true_d,0,true_d) else c(0, 0, true_d),
                           Sigma = matrix(c( 1, .5, .3,
                                            .5,  1, .5,
                                            .3, .5,  1),
                                          3, 3)))
names(data) <- c("nonosp","nonvis","visosp")

# Generate missing values
data$nonosp[1:22] <- NA    
data$nonvis[23:44] <- NA    
data$visosp[45:66] <- NA    

# Impute the data multiply ###########################
M <- 100 # number of imputed data sets
out <- mice(data = data, m = M,
            meth=c("norm","norm","norm"), diagnostics = FALSE, printFlag = FALSE)


# Set up the matrices for the estimates ##############
mulest <- matrix(0,nrow=M,ncol=3) # setup of matrices to store multiple estimates
covwithin <- matrix(0,nrow=3,ncol=3) # and covariance matrices


# Estimate the coefficients for each data frame ######
for(i in 1:M) {
  within <- lm(cbind(nonosp,nonvis,visosp)~1, data=mice::complete(out,i)) # estimate the means of the three variables
  mulest[i,]<-coef(within)[1:3]                                     # store these means in the matrix `mulres`
  covwithin<-covwithin + 1/M * vcov(within)[1:3,1:3]                # compute the average of the covariance matrices
}


# Compute the average of the estimates ###############
estimates <- colMeans(mulest)
names(estimates) <- c("nonosp","nonvis","visosp")
covbetween <- cov(mulest) # is this the between covariance matrix?
covariance <- covwithin + (1+1/M)*covbetween # is this the total variance?


# Determine the effective and real sample sizes ######
samp <- nrow(data) # real sample size
nucom<-samp-length(estimates)

# corresponds to Equation ...
lam <- (1+1/M)*(1/length(estimates))* sum(diag(covbetween %*% ginv(covariance))) # ... (43)
nuold<-(M-1)/(lam^2) # ... (44)
nuobs<-(nucom+1)/(nucom+3)*nucom*(1-lam) # ... (46)
nu<- nuold*nuobs/(nuold+nuobs) # ... (47)
fracmis <- (nu+1)/(nu+3)*lam + 2/(nu+3) # ... (48)
neff<-samp-samp*fracmis
# coerce `covariance`to a list
covariance<-list(covariance)


# Test the hypotheses with bain ######################
results <- bain(estimates,"nonosp=nonvis=visosp;nonosp<nonvis<visosp;nonosp=nonvis<visosp",n=neff,Sigma=covariance,
                group_parameters=3,joint_parameters = 0)

sim_result <- tibble(true_hyp = true_eff,
                     study_iteration = study_iteration,
                     numerator = results$hypotheses,
                     denominator = "Hu",
                     BF = results$fit$BF[1:3],
                     N = nrow(data))%>%
  full_join(tibble(true_hyp = true_eff,
                   study_iteration = study_iteration,
                   numerator = results$hypotheses,
                   `nonosp=nonvis=visosp` = results$BFmatrix[, 1],
                   `nonosp<nonvis<visosp` = results$BFmatrix[, 2],
                   `nonosp=nonvis<visosp` = results$BFmatrix[, 3],
                   N = nrow(data))%>%
              gather(denominator, BF, `nonosp=nonvis=visosp`, `nonosp<nonvis<visosp`, `nonosp=nonvis<visosp`))%>%
  filter(numerator != denominator)%>%
  filter(!(denominator == "Hu" & (true_hyp != numerator)))


#print(sim_result)

write_csv(sim_result, paste("result_files/", true_eff, study_iteration, N, ".csv", sep = "" ))

}
}
}

print(Sys.time())

## Reading & merging the files #################################################
# listing files
csv_files <- list.files(path       = "result_files/", 
                        pattern    = "*.csv", 
                        full.names = T,
                        recursive = T)

# function to recode all dfs
as_char_fun <- function(x) { 
  x %>%       
    mutate_all(as.character)
}

# apply function for all dfs
list_dataframes <- lapply(csv_files,fread)%>%
  lapply(as_char_fun)


# reduce all dfs to one df
data_imported <- reduce(list_dataframes, full_join)%>%
  as_tibble()%>%
  mutate(BF = as.numeric(BF),
         N = as.numeric(N),
         study_iteration = as.numeric(study_iteration))

## Plotting the Results ########################################################
ggplot(data_imported, 
       aes(numerator, log(BF, base = 10), fill = denominator)) + 
  geom_violin(aes(color = denominator)) + 
  #geom_boxplot(aes(fill = denominator), width = .3, color = "white") + 
  facet_wrap(N~true_hyp, scales = "free") +
  geom_hline(yintercept = 1) + 
  geom_hline(yintercept = -1) + 
  theme_ipsum() + 
  scale_fill_ipsum() +
  scale_color_ipsum() + 
  labs(title = "Design Analysis", 
       subtitle = "Violins with 5, 10, 90 and 95 Percentiles", 
       caption = "Subfigures indicate true hypothesis")
  
 
## Evaluation false positives and false negatives only for "nonosp<nonvis<visosp" and "nonosp=nonvis=visosp" ###############
data_sim_res <- data_imported%>%
  as_tibble()%>%
  #filter(true_hyp != "nonosp=nonvis<visosp" & numerator != "nonosp=nonvis<visosp" & denominator != "nonosp=nonvis<visosp")%>%
  arrange(true_hyp, study_iteration, numerator, N, denominator)%>%
  mutate(sim_result_BF10 = case_when(true_hyp == "nonosp<nonvis<visosp" & numerator == "nonosp<nonvis<visosp" & BF > 10 ~ "right decision",
                                true_hyp == "nonosp<nonvis<visosp" & numerator == "nonosp<nonvis<visosp" & BF < 10 & BF > 1/10 ~ "inconclusive",
                                true_hyp == "nonosp<nonvis<visosp" & numerator == "nonosp<nonvis<visosp" & BF < 1/10 ~ "wrong decision",
                                
                                true_hyp == "nonosp=nonvis=visosp" & numerator == "nonosp=nonvis=visosp" & BF > 10 ~ "right decision",
                                true_hyp == "nonosp=nonvis=visosp" & numerator == "nonosp=nonvis=visosp" & BF < 10 & BF > 1/10 ~ "inconclusive",
                                true_hyp == "nonosp=nonvis=visosp" & numerator == "nonosp=nonvis=visosp" & BF < 1/10 ~ "wrong decision",
                                
                                true_hyp == "nonosp=nonvis<visosp" & numerator == "nonosp=nonvis<visosp" & BF > 10 ~ "right decision",
                                true_hyp == "nonosp=nonvis<visosp" & numerator == "nonosp=nonvis<visosp" & BF < 10 & BF > 1/10 ~ "inconclusive",
                                true_hyp == "nonosp=nonvis<visosp" & numerator == "nonosp=nonvis<visosp" & BF < 1/10 ~ "wrong decision"),
         
        sim_result_BF03 = case_when(true_hyp == "nonosp<nonvis<visosp" & numerator == "nonosp<nonvis<visosp" & BF > 3 ~ "right decision",
                               true_hyp == "nonosp<nonvis<visosp" & numerator == "nonosp<nonvis<visosp" & BF < 3 & BF > 1/3 ~ "inconclusive",
                               true_hyp == "nonosp<nonvis<visosp" & numerator == "nonosp<nonvis<visosp" & BF < 1/3 ~ "wrong decision",
                               
                               true_hyp == "nonosp=nonvis=visosp" & numerator == "nonosp=nonvis=visosp" & BF > 3 ~ "right decision",
                               true_hyp == "nonosp=nonvis=visosp" & numerator == "nonosp=nonvis=visosp" & BF < 3 & BF > 1/3 ~ "inconclusive",
                               true_hyp == "nonosp=nonvis=visosp" & numerator == "nonosp=nonvis=visosp" & BF < 1/3 ~ "wrong decision",
                               
                               true_hyp == "nonosp=nonvis<visosp" & numerator == "nonosp=nonvis<visosp" & BF > 3 ~ "right decision",
                               true_hyp == "nonosp=nonvis<visosp" & numerator == "nonosp=nonvis<visosp" & BF < 3 & BF > 1/3 ~ "inconclusive",
                               true_hyp == "nonosp=nonvis<visosp" & numerator == "nonosp=nonvis<visosp" & BF < 1/3 ~ "wrong decision"
        
                                ))         # case_when() returns NA, 
                                                                                                                                                  # if cases are not matched

ggplot(na.omit(data_sim_res)%>%
         gather(var, sim_result, sim_result_BF10, sim_result_BF03)%>%
         mutate(treshold = substr(var, 12, 15)),
       aes(x = true_hyp, fill = sim_result)) + 
  geom_bar(position = "stack") +
  facet_wrap(treshold ~ N) +
  theme_ipsum() + 
  scale_fill_ipsum() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
  ggtitle("Results of the Bayes Factor Design Analysis", "Subfigures indicate the sample size and the decision treshhold") + 
  labs(x = "True hypothesis", y = "Count of simulated studies", caption = "Assumptions: Cohen's d = .3; correlations of repeated measurements r = .5 and r = .3")
