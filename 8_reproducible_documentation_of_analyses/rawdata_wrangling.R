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
    rename(exp_1 = tru_exp_1, 
           exp_2 = tru_exp_2, 
           exp_3 = tru_exp_3, 
           exp_4 = tru_exp_4, 
           exp_5 = tru_exp_5, 
           exp_6 = tru_exp_6,
           int_1 = tru_int_1, 
           int_2 = tru_int_2, 
           int_3 = tru_int_3, 
           int_4 = tru_int_4,
           ben_1 = tru_ben_1, 
           ben_2 = tru_ben_2, 
           ben_3 = tru_ben_3, 
           ben_4 = tru_ben_4) %>% 
    group_by(session) %>% 
    mutate(meas_rep = 1:n()) %>% 
    ungroup() %>% 
    full_join(.,
              formr_raw_results(survey_name = 'rbt_ext_pub_0') %>%
                  filter(session %in% rbt_public$session) %>%
                  select(sex, age, education, session))


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





################################################################################################# #
### SCIENTISTS SAMPLE                                                                          ####
################################################################################################# #

# Data download ###################################################################################
# rbt_sci_0 <- formr_raw_results(survey_name = 'rbt_ext_sci_pre') %>%
#     filter(!is.na(session) & !is.na(ended) & !str_detect(session, "XXX")) %>%
#     select(session, treat1, treat2, first_topic)
# 
# rbt_sci_1 <- formr_raw_results(survey_name = 'rbt_ext_sci_main') %>%
#     filter(!is.na(session) & !is.na(ended) & !str_detect(session, "XXX")) %>%
#     select(-c(created, modified, ended, expired))

rbt_sci_0 <- formr_results(survey_name = 'rbt_ext_sci_pre') %>%
    filter(!is.na(session) & !is.na(ended) & !str_detect(session, "XXX")) %>%
    select(session, treat1, treat2, first_topic)

rbt_sci_1 <- formr_results(survey_name = 'rbt_ext_sci_main') %>%
    filter(!is.na(session) & !is.na(ended) & !str_detect(session, "XXX")) %>%
    select(-c(created, modified, ended, expired))

# match
rbt <- left_join(rbt_sci_1, rbt_sci_0, by="session")

save(rbt, file = "9_data+manual/rbt_sci.RData")
save(rbt_sci_1, file = "9_data+manual/rbt_sci_1.RData")
write_csv(rbt, file = "9_data+manual/rbt_sci.csv")
write_csv(rbt_sci_1, file = "9_data+manual/rbt_sci_1.csv")

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
              list(~inv7fct(.))) %>% # recoding 1-->7, 2-->6, ... %>% 
    rename(exp_1 = tru_exp_1, 
           exp_2 = tru_exp_2, 
           exp_3 = tru_exp_3, 
           exp_4 = tru_exp_4, 
           exp_5 = tru_exp_5, 
           exp_6 = tru_exp_6,
           int_1 = tru_int_1, 
           int_2 = tru_int_2, 
           int_3 = tru_int_3, 
           int_4 = tru_int_4,
           ben_1 = tru_ben_1, 
           ben_2 = tru_ben_2, 
           ben_3 = tru_ben_3, 
           ben_4 = tru_ben_4) %>% 
    group_by(session) %>% 
    mutate(meas_rep = 1:n()) %>% 
    ungroup() %>% 
    full_join(.,rbt_sci_1 %>% 
                  select(session, age, sex, position, position_oth) %>% 
                  distinct())




################################################################################################# #
### Teacher Candidates Sample                                                                  ####
################################################################################################# #

# load data object `x` which contains the data
load("8_reproducible_documentation_of_analyses/rbt.RData") 
# convert to tibble
data_longest <- x%>%
    as_tibble()%>%
    mutate(session = as_factor(session),
           treat = case_when(treat == "nonosp" ~ "GB",
                             treat == "nonvis" ~ "CC",
                             treat == "visosp" ~ "CB"))


# A function to inverse the METI items
inv7fct <- function(x) (8-as.numeric(x))

data_psych_prop_METI <- 
    data_longest%>%
    filter(substr(variable, 1, 3) == "tru")%>% # see codebook
    mutate(item = substr(variable, 5, 9))%>%
    select(session, item, treat, value)%>%
    pivot_wider(names_from = item,
                values_from = value)%>%
    group_by(session)%>%
    mutate(meas_rep = 1:n(),
           treat = as.factor(treat))%>%
    ungroup()%>%
    mutate_at(vars(exp_1:ben_4),
              list(~inv7fct(.))) # recoding 1-->7, 2-->6, ...))

data_psych_prop_tsm <- 
    data_longest%>%
    filter(substr(variable, 1, 3) == "tsm")%>% # see codebook
    mutate(item = substr(variable, 1, 5))%>%
    select(session, item, treat, value)%>%
    pivot_wider(names_from = item,
                values_from = value)%>%
    group_by(session)%>%
    mutate(meas_rep = 1:n(),
           treat = as.factor(treat))%>%
    ungroup()%>%
    mutate_at(vars(starts_with("tsm")),
              list(~as.numeric(as.factor(.))))

data_tch <- data_longest%>%
    filter(variable %in% c("tch_1", "tch_2", "tch_3", "tch_4", "tch_5"))%>%
    # recoding "weiß nicht[don't know]" as lowest specification of ordinal variable
    mutate(value = as.ordered(value))%>%
    spread(variable, value)%>%
    # remove complete empty cases
    filter(is.na(tch_1) == F & is.na(tch_2) == F &
               is.na(tch_3) == F & is.na(tch_4) == F & is.na(tch_5) == F)%>%
    group_by(session)%>%
    mutate(meas_rep = 1:n())%>%
    ungroup()

# Join the data frames
data_psych_prop_joined <- full_join(data_psych_prop_METI, data_psych_prop_tsm)%>%
    mutate(treat = as_factor(treat))


data_demografics <- data_longest %>% 
    filter(variable %in% c("sex", "age", "semester")) %>% 
    select(session, variable, value) %>% 
    spread(variable, value) %>% 
    mutate(session = as.factor(session))


rbt_students_w <- 
    full_join(data_psych_prop_joined, data_demografics) %>% 
    full_join(., data_tch %>% select(-meas_rep)) %>% 
    # remove complete missings
    mutate(na_per_row = rowSums(is.na(.))) %>% 
    filter(na_per_row < 21) %>% 
    select(-na_per_row)


# Export to .csv #############################################
write_csv(rbt_public_w, "8_reproducible_documentation_of_analyses/rbt_public_w.csv")
write_csv(rbt_sci_w, "8_reproducible_documentation_of_analyses/rbt_sci_w.csv")
write_csv(rbt_students_w, "8_reproducible_documentation_of_analyses/rbt_students_w.csv")


# Export for public use #####################################
tmp <- rbt_students_w %>%
    mutate(age1 = case_when(age <= 20 ~ "<=20",          # making age clusters
                            age %in% 21:25 ~ "21-25",
                            age %in% 26:30 ~ "26-30",
                            age > 30 ~ ">30",
                            TRUE ~ NA_character_))

write_csv(tmp, "9_data+codebooks/rbt_study1_undergrad.csv")


# Export for public use #####################################
write_csv(rbt_public, "8_reproducible_documentation_of_analyses/rbt_students_w.csv")

tmp1 <- formr_raw_results(survey_name = 'rbt_ext_pub_0') %>%
                filter(session %in% rbt_public$session) %>%
                select(sex, age, education, session)

write_csv(tmp1, "8_reproducible_documentation_of_analyses/rbt_students_w_demogr.csv")