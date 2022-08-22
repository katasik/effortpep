install.packages("pacman")
require(pacman)
p_load('tidyverse', 'jsonlite')

effort_pep<- read_csv("/Documents/effort pep/qualtricscsv.csv")

effort_pep<- qualtricscsv[-c(1,2), ] %>% type_convert()

labjs_column<- 'labjs-data'

effort_pep %>% 
  # Provide a fallback for missing data
  mutate(
    !!labjs_column := recode(.[[labjs_column]], .missing='[{}]')
  ) %>% 
  # Expand JSON-encoded data per participant
  group_by_all() %>% 
  do(
    fromJSON(.[[labjs_column]], flatten=T)
  ) %>% 
  ungroup() %>%
  # Remove column containing raw JSON
  select(-matches(labjs_column)) -> version1



pep_cleaned_df <- version1 %>%
  select(ResponseId, 73:108) %>% 
  
  # filter out irrelevant components of trial
  filter(str_detect(sender, "catch") |
           str_detect(sender, "probe") |
           str_detect(sender, "error")) %>% 
  
  # separate sender column into block, trial, and component columns
  # will return NA for questionnaire-based trials, this is no issue
  separate(sender_id, into = c("block", "irrelevant value", "trial_num", "component"), sep = "_") %>%  
  
  # mutate block variable
  #mutate(block = ifelse(block == "3", "practice"),
                        #ifelse(block == "5", "main"),
         #mutate(reformat trial number variable
  mutate(trial_num = as.numeric(trial_num) + 1) %>% 
  
  # remove practice trials
  filter(block == 5) %>% 
  
  # remove extraneous probe/catch components
  filter(sender == "probe" & !(is.na(duration)) |
           sender == "catch" & !(is.na(duration))) %>%  
  
  # select only relevant variables
  select(ResponseId, trial_num, sender, duration, word1:word11,
         reversed, probe, response, correct, spelling) %>% 
  
  # rename some variables for clarity
  dplyr::rename(trial_type = sender,
                rt = duration) %>% 
  
  # recode correct column
  mutate(correct = case_when(correct == TRUE ~ 1, 
                             TRUE ~ 0)) 
  
  # remove participants who did not complete all trials
  #group_by(ResponseId)
  #filter(n() == 204) %>%
  #ungroup() %>%
# write_excel_csv(cleaned, "reversed.xlsx")

# cleaned<-pep_cleaned_df %>% 
#   # create belief column to identify whether belief to be expressed was pro- or anti-immigrant
#   mutate(belief = case_when(probe == "TRUE" & reversed == "0" ~ 0,
#                             probe == "TRUE" & reversed == "1" ~ 1,
#                             probe == "FALSE" & reversed == "0" ~ 1,
#                             probe == "FALSE" & reversed == "1" ~ 0,
#                             probe == "??TRUE OR FALSE??" & reversed == "0" ~ 0,
#                             probe == "??TRUE OR FALSE??" & reversed == "1" ~ 1))



reversed<- pep_cleaned_df %>% 
  unite(statement, c(word1: word11)) %>% 
  mutate(effortfun=case_when(statement=="_____WORKING_ON_EASY_TASKS_IS_FUN"~ 0,
                            statement=="_____WORKING_ON_EASY_TASKS_IS_FNU"~ 0,
                            statement=="__I_FIND_IT_DIFFICULT_TO_CONCENTRATE_ON_DIFFICULT_TASKS" ~ 0,
                            statement=="__I_FIND_IT_DIFFICULT_TO_CONCENTRATE_ON_DIFFICULT_TASSK" ~ 0,
                            statement=="__I_FIND_IT_DIFFICULT_TO_CONCENTRATE_ON_EASY_TASSK" ~ 1,
                            statement=="__I_FIND_IT_DIFFICULT_TO_CONCENTRATE_ON_EASY_TASKS" ~ 1,
                            statement=="_______EASY_TASKS_ARE_BORING" ~ 1,
                            statement=="_______EASY_TASKS_ARE_BORIGN" ~ 1,
                            statement=="_______I_HATE_EXERTING_EFFOTR" ~ 0,
                            statement=="_______I_HATE_EXERTING_EFFORT" ~ 0,
                            statement=="_____I_ENJOY_WORKING_ON_EASY_TASKS"~0,
                            statement=="_____I_ENJOY_WORKING_ON_EASY_TASSK"~0,
                            statement=="_____I_ENJOY_EXERTING_EFFORT_VERY_MUHC"~1,
                            statement=="_____I_ENJOY_EXERTING_EFFORT_VERY_MUCH"~1,
                            statement=="__WHILE_EXERTING_EFFORT_I_NOTICE_HOW_ANNOYED_I_GET"~0,
                            statement=="__WHILE_EXERTING_EFFORT_I_NOTICE_HOW_ANNOYED_I_GTE"~0,
                            statement=="_______EXERTING_EFFORT_IS_TIRIGN"~0,
                            statement=="_______EXERTING_EFFORT_IS_TIRING"~0,
                            statement=="_______EXERTING_EFFORT_IS_FUN"~1,
                            statement=="_______EXERTING_EFFORT_IS_FNU"~1,
                            statement=="WHILE_WORKING_ON_EASY_TASKS_I_NOTICE_HOW_ANNOYED_I_GTE"~1,
                            statement=="WHILE_WORKING_ON_EASY_TASKS_I_NOTICE_HOW_ANNOYED_I_GET"~1,
                            statement=="_____I_HATE_WORKING_ON_EASY_TASSK"~ 1,
                            statement=="_____I_HATE_WORKING_ON_EASY_TASKS"~ 1))



correct_all_rate <-
  reversed %>% 
  group_by(ResponseId) %>% 
  summarise(correct_all = mean(correct))

to_clean<-
  left_join(correct_all_rate, reversed, by="ResponseId")

  
cleaned<- to_clean %>%
    # remove participants who got less than 80% correct
    filter(correct_all > .8) %>% 
    # filter only relevant trials
    filter(trial_type == "probe" & correct == 1 & spelling == 1) %>%
    mutate(rt = as.numeric(rt)) %>% 
    # remove global outliers
    filter(rt > 150 & rt < 2500) %>% 
    group_by(ResponseId) %>% 
    filter((mean(rt)-(2*sd(rt))< rt & rt < (mean(rt)+(2*sd(rt))))) %>%
    ungroup()
  

  write_csv(cleaned, "cleaned.csv")

implicit_rt_pep_df <- cleaned %>%
  group_by(ResponseId) %>% 
  filter(response == FALSE & effortfun == 0) %>% 
  summarize(easy_false = mean(rt))

implicit_rt_pep_df1 <- cleaned %>% 
  group_by(ResponseId) %>% 
  filter(response == FALSE & effortfun == 1) %>%
  summarize(effort_false = mean(rt))

implicit_rt_pep_df2 <- cleaned %>% 
  group_by(ResponseId) %>% 
  filter(response == TRUE & effortfun == 0) %>% 
  summarize(easy_true = mean(rt))

implicit_rt_pep_df3 <- cleaned %>% 
  group_by(ResponseId) %>% 
  filter(response == TRUE & effortfun == 1) %>% 
  summarize(effort_true = mean(rt)) 

implicit_rt_pep_final<-
  left_join(implicit_rt_pep_df, implicit_rt_pep_df1, by="ResponseId") %>% 
  left_join(implicit_rt_pep_df2) %>% 
  left_join(implicit_rt_pep_df3)

try<-implicit_rt_pep_final %>% 
  mutate(effort= (easy_false + effort_true)/2) %>% 
  mutate(easy= (effort_false + easy_true)/2) %>%
  mutate(effort_joy = easy-effort)

 final<-
   left_join(try, analysis, by="ResponseId")

mod1<-lm(meandiff~easy + mathsc,
          data=final)
summary(mod1) 


ggplot(final, aes(x = easy, y = meandiff)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")


plot_model(mod1, type = "pred", terms = c("resid_d1", "effort_pep_avg"))
