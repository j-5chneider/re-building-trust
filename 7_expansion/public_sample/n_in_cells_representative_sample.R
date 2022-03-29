library(dplyr)
library(kableExtra)

# put total N here
N <- 250

# Data taken from Census 2011 (England & Wales) "DC5102EW - Highest level of qualification by sex by age"
df <- data.frame(sex = c(rep("male", 35), rep("female", 35)),
                 age = rep(c(rep("16-24", 7), rep("25-34", 7), rep("35-49", 7), rep("50-64", 7), rep("65-x", 7)),2),
                 education = rep(c("No qualifications",
                                   "Level 1 qualifications",
                                   "Level 2 qualifications",
                                   "Apprenticeship",
                                   "Level 3 qualifications",
                                   "Level 4 qualifications and above",
                                   "Other qualifications"), 10),
                 code = c("m_16_n", "m_16_1", "m_16_2", "m_16_a", "m_16_3", "m_16_4", "m_16_o", 
                          "m_25_n", "m_25_1", "m_25_2", "m_25_a", "m_25_3", "m_25_4", "m_25_o", 
                          "m_35_n", "m_35_1", "m_35_2", "m_35_a", "m_35_3", "m_35_4", "m_35_o", 
                          "m_50_n", "m_50_1", "m_50_2", "m_50_a", "m_50_3", "m_50_4", "m_50_o", 
                          "m_65_n", "m_65_1", "m_65_2", "m_65_a", "m_65_3", "m_65_4", "m_65_o", 
                          "w_16_n", "w_16_1", "w_16_2", "w_16_a", "w_16_3", "w_16_4", "w_16_o", 
                          "w_25_n", "w_25_1", "w_25_2", "w_25_a", "w_25_3", "w_25_4", "w_25_o", 
                          "w_35_n", "w_35_1", "w_35_2", "w_35_a", "w_35_3", "w_35_4", "w_35_o", 
                          "w_50_n", "w_50_1", "w_50_2", "w_50_a", "w_50_3", "w_50_4", "w_50_o", 
                          "w_65_n", "w_65_1", "w_65_2", "w_65_a", "w_65_3", "w_65_4", "w_65_o"),
                 persons = c(383215, 635271, 877176, 119974, 822166, 420850, 114013,
                             356374, 486394, 561243, 80242, 561495, 1432301, 278705,
                             780806, 993597, 855662, 261378, 687383, 1968901, 364411,
                             1165667, 543832, 519460, 490271, 517847, 1461749, 319470,
                             1821913, 202503, 253864, 455630, 228747, 866800, 266704,
                             317654, 526961, 891197, 52501, 904442, 485514, 107702,
                             329249, 418507, 579279, 20506, 569306, 1602174, 244749,
                             705734, 1106157, 1118434, 40241, 745435, 2038094, 265543,
                             1388126, 789993, 821364, 58304, 443454, 1352197, 291037,
                             3058589, 344169, 460754, 52730, 137527, 754897, 318246))

sum_pers <- sum(df$persons)

df <- df %>%
    mutate(share = persons/sum_pers,
           n = round(share*N, 0))

# Aggregating Categories for sample n
df_agg <- df %>%
    mutate(age_agg = case_when(
        age == "16-24" | age == "25-34" ~ "16-34",
        age == "35-49"                  ~ "35-49",
        age == "50-64" | age == "65-x"  ~ "50-x"),
        education_agg = case_when(
            education == "Level 1 qualifications" | 
                education == "Level 2 qualifications"           ~ "Level 1 or 2 qualifications",
            education == "Level 3 qualifications" | 
                education == "Level 4 qualifications and above" ~ "Level 3 qualifications or above",
            education == "Apprenticeship" | 
                education == "Other qualifications" |
                education == "No qualifications"                ~ "Apprenticeship, no qual. or other")) %>%
    group_by(age_agg, education_agg, sex) %>%
    summarize(persons = sum(persons))

df_agg2 <- df_agg %>%
    mutate(share = persons/sum_pers,
           n = round(share*N, 0))

kbl(df_agg2) %>%
    kable_styling(full_width = T)
