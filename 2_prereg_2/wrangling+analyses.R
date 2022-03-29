################################################################ #
# Bayesian repeated measures analysis (one within factor) with ###
# missing data                                                 ###
#                                                              ###
# Analyses for the project re-buildging trust                  ###
################################################################ #


## downloading and preparing data ############################## #
library(formr)
formr_connect(email = 'juergen.schneider@uni-tuebingen.de', 
              password = '' ) # find password in video
                  # https://www.youtube.com/watch?v=dQw4w9WgXcQ

# get surveys and
# filter out test runs before Oct 24th and test dummies containing XXX
library(tidyverse)
rbt_0    <- formr_raw_results(survey_name = 'rbt_0') %>%
    dplyr::filter(created > "2019-10-24" & !str_detect(session, "XXX")) %>%
    select(-c(created:expired))   
rbt_1_1  <- formr_raw_results(survey_name = 'rbt_1_1') %>%
    dplyr::filter(created > "2019-10-24" & !str_detect(session, "XXX")) %>%
    select(-c(created:expired))      
rbt_1_2  <- formr_raw_results(survey_name = 'rbt_1_2') %>%
    dplyr::filter(created > "2019-10-24" & !str_detect(session, "XXX")) %>%
    select(-c(created:expired))      
rbt_1_3  <- formr_raw_results(survey_name = 'rbt_1_3') %>%
    dplyr::filter(created > "2019-10-24" & !str_detect(session, "XXX")) %>%
    select(-c(created:expired))      
rbt_1_4  <- formr_raw_results(survey_name = 'rbt_1_4') %>%
    dplyr::filter(created > "2019-10-24" & !str_detect(session, "XXX")) %>%
    select(-c(created:expired))      
rbt_1_5  <- formr_raw_results(survey_name = 'rbt_1_5') %>%
    dplyr::filter(created > "2019-10-24" & !str_detect(session, "XXX")) %>%
    select(-c(created:expired))      
rbt_1_6  <- formr_raw_results(survey_name = 'rbt_1_6') %>%
    dplyr::filter(created > "2019-10-24" & !str_detect(session, "XXX")) %>%
    select(-c(created:expired))      
rbt_1_7  <- formr_raw_results(survey_name = 'rbt_1_7') %>%
    dplyr::filter(created > "2019-10-24" & !str_detect(session, "XXX")) %>%
    select(-c(created:expired))      
rbt_1_8  <- formr_raw_results(survey_name = 'rbt_1_8') %>%
    dplyr::filter(created > "2019-10-24" & !str_detect(session, "XXX")) %>%
    select(-c(created:expired))      
rbt_1_9  <- formr_raw_results(survey_name = 'rbt_1_9') %>%
    dplyr::filter(created > "2019-10-24" & !str_detect(session, "XXX")) %>%
    select(-c(created:expired))      
rbt_1_10 <- formr_raw_results(survey_name = 'rbt_1_10') %>%
    dplyr::filter(created > "2019-10-24" & !str_detect(session, "XXX")) %>%
    select(-c(created:expired))      
rbt_1_11 <- formr_raw_results(survey_name = 'rbt_1_11') %>%
    dplyr::filter(created > "2019-10-24" & !str_detect(session, "XXX")) %>%
    select(-c(created:expired))      
rbt_1_12 <- formr_raw_results(survey_name = 'rbt_1_12') %>%
    dplyr::filter(created > "2019-10-24" & !str_detect(session, "XXX")) %>%
    select(-c(created:expired))      
rbt_2    <- formr_raw_results(survey_name = 'rbt_2') %>%
    dplyr::filter(created > "2019-10-24" & !str_detect(session, "XXX")) %>%
    select(-c(created:expired))      


# pivot them into long format to bind rows
rbt_0_l <- rbt_0 %>%
    mutate(treat_nr = treat) %>%      # rename because it's not the
                                      # treatment type, but the
                                      # rotation of survey 1-12
    dplyr::select(session, treat_nr) %>%
    pivot_longer(cols = -session,
                 names_to = "variable",
                 values_to = "value",
                 values_ptypes = list(value = 'character')) %>%
    mutate(treat = NA_character_)  # introduce treat for later matching

rbt_1_1_l <- pivot_longer(rbt_1_1, 
                          cols = -(session), 
                          names_to = c("treat", "variable"), 
                          # divide col names in two colums
                          names_pattern = "([:alpha:]+)_(t.+_[:digit:])", 
                          values_to = "value",
                          values_ptypes = list(value = 'character'))

rbt_1_2_l <- pivot_longer(rbt_1_2, 
                          cols = -(session), 
                          names_to = c("treat", "variable"), 
                          # divide col names in two colums
                          names_pattern = "([:alpha:]+)_(t.+_[:digit:])", 
                          values_to = "value",
                          values_ptypes = list(value = 'character'))

rbt_1_3_l <- pivot_longer(rbt_1_3, 
                          cols = -(session), 
                          names_to = c("treat", "variable"), 
                          # divide col names in two colums
                          names_pattern = "([:alpha:]+)_(t.+_[:digit:])", 
                          values_to = "value",
                          values_ptypes = list(value = 'character'))

rbt_1_4_l <- pivot_longer(rbt_1_4, 
                          cols = -(session), 
                          names_to = c("treat", "variable"), 
                          # divide col names in two colums
                          names_pattern = "([:alpha:]+)_(t.+_[:digit:])", 
                          values_to = "value",
                          values_ptypes = list(value = 'character'))

rbt_1_5_l <- pivot_longer(rbt_1_5, 
                          cols = -(session), 
                          names_to = c("treat", "variable"), 
                          # divide col names in two colums
                          names_pattern = "([:alpha:]+)_(t.+_[:digit:])", 
                          values_to = "value",
                          values_ptypes = list(value = 'character'))


rbt_1_6_l <- pivot_longer(rbt_1_6, 
                          cols = -(session), 
                          names_to = c("treat", "variable"), 
                          # divide col names in two colums
                          names_pattern = "([:alpha:]+)_(t.+_[:digit:])", 
                          values_to = "value",
                          values_ptypes = list(value = 'character'))

rbt_1_7_l <- pivot_longer(rbt_1_7, 
                          cols = -(session), 
                          names_to = c("treat", "variable"), 
                          # divide col names in two colums
                          names_pattern = "([:alpha:]+)_(t.+_[:digit:])", 
                          values_to = "value",
                          values_ptypes = list(value = 'character'))

rbt_1_8_l <- pivot_longer(rbt_1_8, 
                          cols = -(session), 
                          names_to = c("treat", "variable"), 
                          # divide col names in two colums
                          names_pattern = "([:alpha:]+)_(t.+_[:digit:])", 
                          values_to = "value",
                          values_ptypes = list(value = 'character'))

rbt_1_9_l <- pivot_longer(rbt_1_9, 
                          cols = -(session), 
                          names_to = c("treat", "variable"), 
                          # divide col names in two colums
                          names_pattern = "([:alpha:]+)_(t.+_[:digit:])", 
                          values_to = "value",
                          values_ptypes = list(value = 'character'))

rbt_1_10_l <- pivot_longer(rbt_1_10, 
                           cols = -(session), 
                           names_to = c("treat", "variable"), 
                           # divide col names in two colums
                           names_pattern = "([:alpha:]+)_(t.+_[:digit:])", 
                           values_to = "value",
                           values_ptypes = list(value = 'character'))

rbt_1_11_l <- pivot_longer(rbt_1_11, 
                           cols = -(session), 
                           names_to = c("treat", "variable"), 
                           # divide col names in two colums
                           names_pattern = "([:alpha:]+)_(t.+_[:digit:])", 
                           values_to = "value",
                           values_ptypes = list(value = 'character'))

rbt_1_12_l <- pivot_longer(rbt_1_12, 
                           cols = -(session), 
                           names_to = c("treat", "variable"), 
                           # divide col names in two colums
                           names_pattern = "([:alpha:]+)_(t.+_[:digit:])", 
                           values_to = "value",
                           values_ptypes = list(value = 'character'))

rbt_2_l <- rbt_2 %>%
    mutate(tcg_1 = tsm_1,          # accidentally named var wrong
           tcg_2 = tsm_2,          # rename to tcg == 'treatment
           tcg_3 = tsm_3) %>%      # check global'
    dplyr::select(-c(tsm_1, tsm_2, tsm_3)) %>%
    pivot_longer(cols = -(session), 
                 names_to = c("variable"), 
                 values_to = "value",
                 values_ptypes = list(value = 'character')) %>%
    mutate(treat = NA_character_)  # introduce treat for later matching


# bind rows
rbt <- data.frame(bind_rows(rbt_0_l,
                            rbt_1_1_l, 
                            rbt_1_2_l, 
                            rbt_1_3_l, 
                            rbt_1_4_l, 
                            rbt_1_5_l, 
                            rbt_1_6_l, 
                            rbt_1_7_l, 
                            rbt_1_8_l, 
                            rbt_1_9_l, 
                            rbt_1_10_l, 
                            rbt_1_11_l, 
                            rbt_1_12_l,
                            rbt_2_l))


################################################################ #
# TREATMENT CHECK                                             ####
################################################################ #
library(MASS)
library(bain)
library(psych)

rbt_tch <- rbt %>%
    dplyr::filter(variable == "tch_1" | 
                  variable == "tch_2" | 
                  variable == "tch_3") %>%
    pivot_wider(id_cols = "session",
                names_from = "variable",
                values_from = "value")


### PREPARATION ###
# using lm to get
# - means of conditions (estm), 
# - sample sizes of conditions (sampm) and 
# - residual variances of the sample means (covm)
prep_rbt_tc <- lm(tc ~ treat - 1,   # "The -1 instructs lm to 
                                       # estimate the means in each group."
                      data = rbt)

# get means of conditions
estm_tc <- coef(prep_rbt_tc)
names(estm_tc) <- c("g1","g2","g3")  # renaming to meet bain requirements

# get sample sizes of each condition
sampm <- table(rbt$cond)

# comupte residual variances of the sample means
# collect residual variance from lm object
varm_tc <- (summary(prep_rbt_tc)$sigma)^2

# compute the variance of the sample means in each group
cov1m_tc <- varm_tc/sampm[1]
cov2m_tc <- varm_tc/sampm[2]
cov3m_tc <- varm_tc/sampm[3]

# define cov1m, cov2m, cov3m as 1x1 matrices
cov1m_tc <- matrix(cov1m_tc,1,1)
cov2m_tc <- matrix(cov2m_tc,1,1)
cov3m_tc <- matrix(cov3m_tc,1,1)

# put cov1m, cov2m, cov3m in a list
covm_tc <- list(cov1m_tc,cov2m_tc,cov3m_tc)

### BAYES FACTOR CALCULATION ###
set.seed(0815)
# relevant hypotheses are H1 and H2
# H3 and H4 are further estimated to facilitate interpretation
tc_rbt <- bain(estm_tc,
                   "g1 < g2 < g3; g1 = g2 = g3; g1 < g2 = g3; g1 = g2 < g3;",
                   n = sampm,
                   Sigma = covm_tc,
                   group_parameters = 1,
                   joint_parameters = 0)

# print the results of the analysis with bain
print(tc_rbt)

################################################################ #
# ANALYSES                                                    ####
################################################################ #
