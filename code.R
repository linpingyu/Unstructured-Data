library(tidyverse)
library(stringr)
library(tidytext)


full <- readRDS('full_woSalaryInfo.rds')

df1 <- full[stringr::str_detect(full$Title, 'analyst'),]
df2 <- full[stringr::str_detect(full$Title, 'Analyst'),]
df3 <- full[stringr::str_detect(full$Title, 'data'),]
df4 <- full[stringr::str_detect(full$Title, 'Data'),]

all <- bind_rows(df1, df2, df3, df4)
all <- all[!duplicated(all),] %>% filter(!is.na(Id))
all$Title <- str_remove_all(all$Title, '[:punct:]*')
all$FullDescription <- str_remove_all(all$FullDescription,'[:punct:]*')

jobDescription <- all %>% select(Id, Title, FullDescription)
jobDescription %>% 
  unnest_tokens(bigram, FullDescription, token = 'ngrams', n=2) %>% 
  count(bigram, sort = T) %>% 
  ungroup()
