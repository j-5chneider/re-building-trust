## Recoding the results #############

#  1) Check the BF's for the comparisons of the true hypothesis (A)
#     against the other hypotheses under consideration and it's 
#     complement.
#  2) If the data favors the true hypothesis against all others 
#     under consideration and it's complement with a BF > 3 
#     code »evidence for the true hypothesis«
#  3) If this procedure results in at least one BF with 
#     1/3 < BF < 3 code »inconclusive«
#  4) If this procedure results in at least one BF < 1/3 code 
#     »wrong result« 

library(tidyverse)
library(hrbrthemes)

## Recoding right and (inconclusive or wrong) decisions if 
## `nonosp=nonvis=visosp` is true    
`results_nonosp=nonvis=visosp_true` <- 
  read_csv(("sim_results_total_bfda_badgestudy.csv"))%>%
  filter(true_hyp == "nonosp=nonvis=visosp")%>%
  group_by(N, study_iteration)%>%
  do(data.frame(decision = ifelse(
    filter(., numerator == "nonosp=nonvis=visosp" &
             denominator == "Hc")$BF > 3 &
    filter(., numerator == "nonosp=nonvis=visosp" &
             denominator == "nonosp<nonvis<visosp")$BF > 3 &
    filter(., numerator == "nonosp=nonvis=visosp" &
             denominator == "nonosp<nonvis=visosp")$BF > 3,
    "right", 
    ifelse(
      filter(., numerator == "nonosp=nonvis=visosp" &
               denominator == "Hc")$BF > 1/3 &
      filter(., numerator == "nonosp=nonvis=visosp" &
               denominator == "nonosp<nonvis<visosp")$BF > 1/3 &
      filter(., numerator == "nonosp=nonvis=visosp" &
               denominator == "nonosp<nonvis=visosp")$BF > 1/3,
      "inconclusive", "wrong")),
    true_hyp = .$true_hyp[1]))

## Recoding right and (inconclusive or wrong) decisions if 
## `nonosp<nonvis<visosp` is true
`results_nonosp<nonvis<visosp_true` <-
  read_csv(("sim_results_total_bfda_badgestudy.csv"))%>%
  filter(true_hyp == "nonosp<nonvis<visosp")%>%
  group_by(N, study_iteration)%>%
  do(data.frame(decision = ifelse(
    filter(., numerator == "nonosp<nonvis<visosp" &
             denominator == "Hc")$BF > 3 &
    filter(., numerator == "nonosp<nonvis<visosp" &
             denominator == "nonosp=nonvis=visosp")$BF > 3 &
    filter(., numerator == "nonosp<nonvis<visosp" &
             denominator == "nonosp<nonvis=visosp")$BF > 3,
    "right", 
    ifelse(
      filter(., numerator == "nonosp<nonvis<visosp" &
               denominator == "Hc")$BF > 1/3 &
      filter(., numerator == "nonosp<nonvis<visosp" &
               denominator == "nonosp=nonvis=visosp")$BF > 1/3 &
      filter(., numerator == "nonosp<nonvis<visosp" &
               denominator == "nonosp<nonvis=visosp")$BF > 1/3,
      "inconclusive", "wrong")),
    true_hyp = .$true_hyp[1]))


## Recoding right and (inconclusive or wrong) decisions if 
## `nonosp<nonvis=visosp` is true
`results_nonosp<nonvis=visosp_true` <- 
  read_csv(("sim_results_total_bfda_badgestudy.csv"))%>%
  filter(true_hyp == "nonosp<nonvis=visosp")%>%
  group_by(N, study_iteration)%>%
  do(data.frame(decision = ifelse(
    filter(., numerator == "nonosp<nonvis=visosp" &
             denominator == "Hc")$BF > 3 &
    filter(., numerator == "nonosp<nonvis=visosp" &
             denominator == "nonosp=nonvis=visosp")$BF > 3 &
    filter(., numerator == "nonosp<nonvis=visosp" &
             denominator == "nonosp<nonvis<visosp")$BF > 3,
    "right", 
    ifelse(
      filter(., numerator == "nonosp<nonvis=visosp" &
               denominator == "Hc")$BF > 1/3 &
      filter(., numerator == "nonosp<nonvis=visosp" &
               denominator == "nonosp=nonvis=visosp")$BF > 1/3 &
      filter(., numerator == "nonosp<nonvis=visosp" &
               denominator == "nonosp<nonvis<visosp")$BF > 1/3,
      "inconclusive", "wrong")),
    true_hyp = .$true_hyp[1]))


## Recoding right and (inconclusive or wrong) decisions if 
## `nonosp,nonvis,visosp` is true
`results_nonosp,nonvis,visosp_true` <- 
  read_csv(("sim_results_total_bfda_badgestudy.csv"))%>%
  filter(true_hyp == "nonosp,nonvis,visosp")%>%
  group_by(N, study_iteration)%>%
  do(data.frame(decision = ifelse(
    filter(., numerator == "nonosp=nonvis=visosp" &
             denominator == "Hc")$BF < 1/3 &
    filter(., numerator == "nonosp<nonvis=visosp" &
             denominator == "Hc")$BF < 1/3 &
    filter(., numerator == "nonosp<nonvis<visosp" &
             denominator == "Hc")$BF < 1/3,
    "right", 
    ifelse(
      filter(., numerator == "nonosp=nonvis=visosp" &
               denominator == "Hc")$BF < 3 &
      filter(., numerator == "nonosp<nonvis=visosp" &
               denominator == "Hc")$BF < 3 &
      filter(., numerator == "nonosp<nonvis<visosp" &
               denominator == "Hc")$BF < 3,
      "inconclusive", "wrong")),
    true_hyp = .$true_hyp[1]))


## Joining the results_..._true tables
results_labeled <- full_join(
  `results_nonosp<nonvis<visosp_true`,
  full_join(`results_nonosp<nonvis=visosp_true`, 
            full_join(`results_nonosp=nonvis=visosp_true`, 
                      `results_nonosp,nonvis,visosp_true`)))%>%
  mutate(Decision = factor(decision, 
                           levels = c("wrong", 
                                      "inconclusive", 
                                      "right")))


## Visializing the results
ggplot(results_labeled, aes(true_hyp, fill = Decision)) +
  geom_bar(position = "fill") + 
  geom_text(aes(label=round(..count../1000*100), y= ..count../1000),
            position =position_stack(vjust = 0.5), stat= "count", 
            color = "white", size = 5) +
  coord_flip() +
  facet_wrap(~N, ncol = 1) + 
  labs(title = "Results of the Bayes Factor Design Analysis", 
       caption = "In % (rounded), based on 1,000 simulations") + 
  xlab("True Hypothesis") + 
  scale_fill_viridis_d() +
  theme_ipsum_rc()
