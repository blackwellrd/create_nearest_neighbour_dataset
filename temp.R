library(tidyverse)

df_imd <- read.csv('D:/Data/GOV.UK/IMD/2019/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv')


df_plot_data <- df_imd %>% 
  select(1 | contains('score')) %>% 
  pivot_longer(cols = contains('score'), names_to = 'metric', values_to = 'score') %>%
  rename_with(.fn = ~c('lsoa11cd', 'metric', 'score'))

ggplot(df_plot_data) %+%
  geom_histogram(aes(x = score)) %+%
  facet_wrap(. ~ metric, scales = 'free')

df_tmp <- read.csv('D:/Data/NHSD/GPREGLSOA/20230701/gp-reg-pat-prac-lsoa-all.csv') %>%
  filter(grepl('WONFORD|LEONARDS', PRACTICE_NAME)) %>%
  select(c('PRACTICE_CODE', 'LSOA_CODE', 'NUMBER_OF_PATIENTS')) %>%
  inner_join(df_plot_data %>% filter(metric == 'Index.of.Multiple.Deprivation..IMD..Score'),
             by = c('LSOA_CODE' = 'lsoa11cd'))

df_tmp %>%
  group_by(PRACTICE_CODE) %>%
  reframe(value = c(quantile(rep(score, NUMBER_OF_PATIENTS), 
                             probs = c(0.25, 0.5, 0.75), 
                             names = TRUE),
                    popn = sum(NUMBER_OF_PATIENTS)))
  
  
hist(rep(df_tmp$score, df_tmp$NUMBER_OF_PATIENTS))
hist(rep(df_tmp$score[df_tmp$PRACTICE_CODE=='L83042'], df_tmp$NUMBER_OF_PATIENTS[df_tmp$PRACTICE_CODE=='L83042']))
hist(rep(df_tmp$score[df_tmp$PRACTICE_CODE=='L83655'], df_tmp$NUMBER_OF_PATIENTS[df_tmp$PRACTICE_CODE=='L83655']))

df_tmp %>% filter(PRACTICE_CODE == 'L83655') %>% arrange(desc(NUMBER_OF_PATIENTS)) %>% glimpse()
df_gp_popn %>% filter(PRACTICE_NAME == '')