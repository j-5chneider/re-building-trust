library(formr)
library(tidyverse)

formr_connect(email = 'juergen.schneider@uni-tuebingen.de', 
              password = '')                      # BITTE SO NICHT AUF GITHUB :)


################################################################################################# #
### PUBLIC SAMPLE                                                                              ####
################################################################################################# #

# Data download ###################################################################################
rbt_public <- formr_raw_results(survey_name = 'rbt_ext_pub') %>%
    dplyr::filter(!str_detect(session, 'XXX') & !is.na(session) & !is.na(ended)) #filter away dummies & dropouts

# wrangling #######################################################################################
rbt_public_l <- pivot_longer(rbt_public, 
                             cols = (abs1_tsm_1:abs2_tch_5), 
                             names_to = "variable", 
                             values_to = "value") %>%
    mutate(treat = case_when(                    # create treatment variable
        str_detect(variable, "abs1_") ~ toupper(treat1),
        str_detect(variable, "abs2_") ~ toupper(treat2)),
        variable = str_sub(variable, 6, -1)) # delete title page substring

# pivot them into wide format
rbt_public_w <- pivot_wider(rbt_public_l,
                            names_from = variable,
                            values_from = value,
                            id_cols = c(session, treat))

# invert trust items & build scales
inv7fct <- function(x) (8-as.numeric(x))
rbt_public_w <- rbt_public_w %>%
    mutate_at(vars(tru_exp_1:tru_ben_4),
              list(~inv7fct(.))) %>% # recoding 1-->7, 2-->6, ...
    mutate(Treatment = factor(treat, levels = c("GB", "CC", "CB")),
           Experitse = rowMeans(data.frame(tru_exp_1, tru_exp_2, tru_exp_3, 
                                           tru_exp_4, tru_exp_5, tru_exp_6), na.rm = T),
           Integrity = rowMeans(data.frame(tru_int_1, tru_int_2, tru_int_3, tru_int_4), na.rm = T),
           Benevolence = rowMeans(data.frame(tru_ben_1, tru_ben_2, tru_ben_3, tru_ben_4), na.rm = T),
           TSM = rowMeans(data.frame(tsm_1, tsm_2, tsm_3, tsm_4), na.rm = T))


### sample coverage ###############################################################################
# target sample based on census
target <- data.frame(n_needed = c(6, 7, 13, 14, 20, 18, 6, 8, 12, 10, 15, 15, 28, 25, 13, 8, 15, 17),
                     code = c('16_app_f', '16_app_m', '16_L12_f', '16_L12_m', '16_L34_f', '16_L34_m', '35_app_f', '35_app_m', '35_L12_f', '35_L12_m', '35_L34_f', '35_L34_m', '50_app_f', '50_app_m', '50_L12_f', '50_L12_m', '50_L34_f', '50_L34_m'))

# demographics of participants that completed the survey
sample <- formr_raw_results(survey_name = 'rbt_ext_pub_0') %>%
    dplyr::filter(session %in% rbt_public$session) %>%
    dplyr::mutate(code = paste(age, substr(education, 1, 3), sex, sep='_')) %>%
    group_by(code) %>%
    summarize(n_sample = length(code)) %>%
    ungroup()

# table of sample coverage
full_join(sample, target, by="code") %>%
    dplyr::mutate(perc_full = paste((n_sample/n_needed)*100, "%"))



################################################################################################# #
### SCIENTISTS SAMPLE                                                                          ####
################################################################################################# #

# Data download ###################################################################################
rbt_sci_0 <- formr_raw_results(survey_name = 'rbt_ext_sci_pre') %>%
    filter(!is.na(session) & !is.na(ended) & !str_detect(session, "XXX")) %>%
    select(session, treat1, treat2, first_topic)

rbt_sci_1 <- formr_raw_results(survey_name = 'rbt_ext_sci_main') %>%
    filter(!is.na(session) & !is.na(ended) & !str_detect(session, "XXX")) %>%
    select(-c(created, modified, ended, expired))

# match
rbt <- left_join(rbt_sci_1, rbt_sci_0, by="session")

# wrangling #######################################################################################
# pivot them into long format
rbt_sci_l <- pivot_longer(rbt, 
                      cols = (abs1_tsm_1:abs2_tch_5), 
                      names_to = "variable", 
                      values_to = "value") %>%
    mutate(treat = case_when(                    # create treatment variable
        str_detect(variable, "abs1_") ~ toupper(treat1),
        str_detect(variable, "abs2_") ~ toupper(treat2)),
        variable = str_sub(variable, 6, -1)) # delete title page substring

# pivot them into wide format
rbt_sci_w <- pivot_wider(rbt_sci_l,
                     names_from = variable,
                     values_from = value,
                     id_cols = c(session, treat))

# invert trust items & build scales
rbt_sci_w <- rbt_sci_w %>%
    mutate_at(vars(tru_exp_1:tru_ben_4),
              list(~inv7fct(.))) %>% # recoding 1-->7, 2-->6, ...
    mutate(Treatment = factor(treat, levels = c("GB", "CC", "CB")),
           Experitse = rowMeans(data.frame(tru_exp_1, tru_exp_2, tru_exp_3, 
                                           tru_exp_4, tru_exp_5, tru_exp_6), na.rm = T),
           Integrity = rowMeans(data.frame(tru_int_1, tru_int_2, tru_int_3, tru_int_4), na.rm = T),
           Benevolence = rowMeans(data.frame(tru_ben_1, tru_ben_2, tru_ben_3, tru_ben_4), na.rm = T),
           TSM = rowMeans(data.frame(tsm_1, tsm_2, tsm_3, tsm_4), na.rm = T))


# Export to .csv #############################################
write_csv(rbt_public_w, "5_analyses/rbt_public_w.csv")
write_csv(rbt_sci_w, "5_analyses/rbt_sci_w.csv")
