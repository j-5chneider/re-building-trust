library(tidyverse)

# import results from Web of Science:
## all articles from 
## 10 top ranked journals in Social Sciences/Education (based on scimagojr.com on 24th Sept 2020) 
## over the last 5 years
wos1a <- rio::import("7_expansion/topic_search/wos_export1a.xls")
wos1b <- rio::import("7_expansion/topic_search/wos_export1b.xls")
wos2a <- rio::import("7_expansion/topic_search/wos_export2a.xls")
wos2b <- rio::import("7_expansion/topic_search/wos_export2b.xls")
wos3a <- rio::import("7_expansion/topic_search/wos_export3a.xls")
wos3b <- rio::import("7_expansion/topic_search/wos_export3b.xls")
wos4a <- rio::import("7_expansion/topic_search/wos_export4a.xls")
wos4b <- rio::import("7_expansion/topic_search/wos_export4b.xls")
wos5 <- rio::import("7_expansion/topic_search/wos_export5.xls")

wos <- rbind(wos1a, wos1b, wos2a, wos2b, wos3a, wos3b, wos4a, wos4b, wos5)

wos <- wos %>%
  dplyr::mutate(journal = ifelse(str_detect(`Source Title`, "MOBICOM"), 
                                            "MOBICOM", 
                                            ifelse(`Source Title` == "Educational Researcher", 
                                                   "EDUCATIONAL RESEARCHER", 
                                                   `Source Title`)))

write.csv2(wos, file="7_expansion/topic_search/wos_export.csv")


# Sample 99 papers from these journals to avoid unbalanced amount of papers from journals 
## (99 papers is the smallest amount of papers in one journal)
set.seed(815)
wos_samp <- wos %>%
  group_by(journal) %>%
  sample_n(99, replace = F) %>%
  ungroup()

write.csv2(wos_samp, file="7_expansion/topic_search/wos_export_samp.csv")


# get all keywords in one column
keyw_count <- wos_samp %>%
  dplyr::select(journal, `Author Keywords`) %>%
  dplyr::mutate(study_id = rownames(.)) %>%
  tidyr::separate(`Author Keywords`, into = paste("keyw", 1:23, sep = "_"), sep = ";") %>%
  pivot_longer(cols = c(2:24), names_to = "keyw_nr", values_to = "keywords", values_drop_na = T) %>%
  mutate(keywords = tolower(trimws(keywords))) %>%
  dplyr::select(-keyw_nr)

keyw_count <- data.frame(table(keyw_count$keywords))

keyw_count[order(keyw_count$Freq, decreasing = T),]

write.csv2(keyw_count[order(keyw_count$Freq, decreasing = T),], 
           file="7_expansion/topic_search/wos_export_count.csv")



