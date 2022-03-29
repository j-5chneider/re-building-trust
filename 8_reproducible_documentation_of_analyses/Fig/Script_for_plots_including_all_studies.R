###########
# Fig 3.
###########

# Merging the data
library(tidyverse)
library(hrbrthemes)
data_combined_figures <-
    full_join(
    read_csv("8_reproducible_documentation_of_analyses/Fig/data_scales_lables_study_1.csv") %>% select(-sex),
    read_csv("8_reproducible_documentation_of_analyses/Fig/data_scales_lables_study_2.csv") %>% select(-sex),) %>% 
    full_join(read_csv("8_reproducible_documentation_of_analyses/Fig/data_scales_lables_study_3.csv") %>% select(-sex)) 


fig3 <-
    data_combined_figures %>%
    mutate(Treatment = case_when(Treatment == "Greyed out badges" ~ "Grayed out badges",
                                 Treatment == "Control Condition" ~ "Control condition",
                                 TRUE ~ Treatment),
           Study_by_pop = case_when(Study == "Study 1" ~ "Student teachers",
                                    Study == "Study 2" ~ "Social scientists",
                                    T ~ "Public"),
           Study_by_pop = factor(Study_by_pop, levels = c("Student teachers", "Social scientists", "Public"))) %>% 
    ggplot(., aes(x = Treatment, y = Integrity)) +
    geom_violin(adjust = 1.5) +
    stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1)) +
    coord_flip() +
    facet_wrap( ~ Study_by_pop, nrow = 1) +
   # labs(title = "Integrity by experimental condition (Hypothesis 1)",
   #      subtitle = "Violin plots and means ± 1*SD",
   #      caption = "") +
    ylim(1, 7) +
    xlab("") +
    hrbrthemes::theme_ipsum()

ggsave(plot = fig3, 
       filename = "8_reproducible_documentation_of_analyses/Fig/fig3.png", 
       device = NULL,
       width = 35,
       height = 12,
       unit = "cm",
       dpi = 600)

###########
# Fig 4.
###########

fig4 <- 
    data_combined_figures %>%
    mutate(Treatment = case_when(Treatment == "Greyed out badges" ~ "Grayed out badges",
                                 Treatment == "Control Condition" ~ "Control condition",
                                 TRUE ~ Treatment),
           Study_by_pop = case_when(Study == "Study 1" ~ "Student teachers",
                                    Study == "Study 2" ~ "Social scientists",
                                    T ~ "Public"),
           Study_by_pop = factor(Study_by_pop, levels = c("Student teachers", "Social scientists", "Public"))) %>% 
    ggplot(., aes(x = Treatment, y = `Topic specific multiplism`)) + 
    geom_violin(adjust = 1.5, alpha = .5) +
    stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1)) +
    coord_flip() +
    facet_wrap( ~ Study_by_pop, nrow = 1) +
   # labs(title = "Topic specific multiplism by experimental condition (Hypothesis 4)",
   #      subtitle = "Violin plots and means ± 1*SD",
   #      caption = "") +
    xlab("") +
    ylim(1,4) +
    hrbrthemes::theme_ipsum()

ggsave(plot = fig4, 
       filename = "8_reproducible_documentation_of_analyses/Fig/fig4.png", 
       device = NULL,
       width = 35,
       height = 12,
       unit = "cm",
       dpi = 600)
