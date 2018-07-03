
## @knitr load.lib

rm(list = ls())
library(wordcloud)
library(tidyverse)
library(dplyr)
library(stringr)
library(tidytext)
library(tm)
library(textstem)
library(tidyr)
library(forcats)
library(igraph)
library(ggraph)
library(widyr)

## @knitr read.data
data <- read_csv("Data/Train_rev1.csv")

train_data_jobs <- data %>% mutate(Title = as.character(tolower(Title))) %>% 
  filter(str_detect(Title, paste(c("analyst", "data", "business intelligence", 
                                   "statisti"),collapse = '|')))

## @knitr corpus

# n_distinct(train_data_jobs$Category)

job_corpus <- VCorpus(VectorSource(train_data_jobs$FullDescription))

# lapply(job_corpus[1:5], as.character)

## @knitr cleaning

#changing to lower case
job_corpus_clean <- tm_map(job_corpus, content_transformer(tolower))

#remove numbers from the text
job_corpus_clean <- tm_map(job_corpus_clean, removeNumbers)

#remove stop words
job_corpus_clean <- tm_map(job_corpus_clean, removeWords, 
                           c(stopwords(), 'will', 'work', 'skill', 'job', 'role', 'required', 'within', 'please', 
                             'opportunity', 'successful', 'requirements', 'working', 'ability', 'looking', 
                             'knowledge', 'skills', 'work', 'good', 'key', 'strong', 'new', 'requirement', 
                             'abilities'))

mystopwords <- data.frame(word = c("will", 'work', 'skill', 'job', 'role', 'required', 'within', 'please', 
                                   'opportunity', 'successful', 'requirements', 'working', 'ability', 'looking', 
                                   'knowledge', 'skills', 'work', 'good', 'key', 'strong', 'new', 'requirement', 
                                   'abilities'))


#remove punctuations
job_corpus_clean <- tm_map(job_corpus_clean, removePunctuation)


#stemming the words to their root words - fucking things
#job_corpus_clean <- tm_map(job_corpus_clean, stemDocument)

# removing extra spaces from the text
job_corpus_clean <- tm_map(job_corpus_clean, stripWhitespace)

# lapply(job_corpus_clean[1:5], as.character)

FullDescription <- data.frame(text=unlist(sapply(job_corpus_clean, `[`, "content")), 
                              stringsAsFactors=F)

train_data_jobs_clean <- train_data_jobs %>% select(-FullDescription) %>% cbind(FullDescription)

ndoc <- length(unique(train_data_jobs_clean$Category))

train_data_jobs_clean <- train_data_jobs_clean %>% 
  mutate(text = as.character(text)) %>% rename(FullDescription = text) %>% 
  mutate(Category = gsub(' Jobs', '', Category),
         Category = as.factor(Category))

train_data_jobs_clean$FullDescription <- sapply(train_data_jobs_clean$FullDescription,
                                                function(row) iconv(row, "latin1", "ASCII", sub=""))

## @knitr word_tf

Word_TF <- train_data_jobs_clean %>% 
  unnest_tokens(word, FullDescription) %>% 
  mutate(word=lemmatize_strings(word)) %>%
  anti_join(stop_words, by = 'word') %>%
  anti_join(mystopwords, by = 'word') %>% 
  group_by(Category) %>%
  count(word,sort=TRUE) %>% 
  ungroup() %>% group_by(Category) %>%
  mutate(TF = n/sum(n))

freq_by_rank <- Word_TF %>% arrange(Category, desc(TF)) %>% 
  group_by(Category) %>% 
  mutate(rank = row_number())

## @knitr frequency.plot

p1 <- freq_by_rank %>% 
  ggplot(aes(rank, TF, color = Category)) + 
  geom_abline(intercept = -0.62, slope = -1.1, color = "gray50", linetype = 2) +
  geom_line(size = 1, alpha = 0.5, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()

## @knitr tf.idf

Word_IDF <- train_data_jobs_clean %>% 
  unnest_tokens(word, FullDescription) %>% 
  mutate(word=lemmatize_strings(word)) %>%
  anti_join(stop_words, by = 'word') %>%
  anti_join(mystopwords, by = 'word') %>%
  group_by(Category) %>%
  count(word,sort=TRUE) %>% ungroup() %>% 
  group_by(word) %>%
  summarise(nDoc=n()) %>%
  mutate(nTotalDoc=ndoc,
         idf = log(nTotalDoc/nDoc)) %>%
  arrange(idf)

Word_Tf_IDF <- merge(Word_TF,Word_IDF,by="word") %>% 
  mutate(TFIDF = TF*idf) %>% 
  arrange(Category,desc(TFIDF)) %>% ungroup()

category_dist <- train_data_jobs_clean %>% group_by(Category) %>%
  summarise(Freq = n()) %>%
  mutate(prop = Freq/sum(Freq)*100) %>%  #Computing proportion of each industry in data
  arrange(desc(prop)) %>% 
  mutate(cum_prop = cumsum(prop))

## @knitr proportion.plot

p2 <- category_dist %>%
  ggplot(aes(x=fct_reorder(Category, prop), y = prop)) + 
  geom_col() + 
  coord_flip() +
  labs(x='Job Category', y = 'Proportion in Data', title = 'Proportion of Categories in Data') +
  theme_bw()

## @knitr word.tf.idf
selected_categories <- category_dist %>% slice(1:10) %>% select(Category) %>% 
  filter(Category != "Other/General Jobs")

# stringr::str_detect(Word_Tf_IDF$word, '^www')

p3 <- Word_Tf_IDF %>% filter(!str_detect(word, '^www')) %>% 
  inner_join(selected_categories, by = "Category") %>% 
  arrange(desc(TFIDF)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(Category) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(word, TFIDF, fill = Category)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~Category, ncol = 3, scales = "free") +
  coord_flip() +
  theme_bw()



## @knitr wordcloud1
set.seed(1234)
tokens <- train_data_jobs_clean %>% 
  select(Category, FullDescription) %>% 
  unnest_tokens(word, FullDescription) %>% 
  anti_join(stop_words, by = 'word') %>% 
  count(word, Category, sort = TRUE) %>% 
  ungroup() %>% 
  group_by(word, Category) %>% 
  summarise(freq = sum(n)) %>% 
  arrange(Category, desc(freq))

tokens %>% 
  filter(Category == "IT") %>% 
  select(word, freq) %>% 
  with(wordcloud(word, freq, max.words=300, random.order=FALSE,
                 rot.per=0.35, colors=brewer.pal(8, 'Dark2')))
# wordcloud2::wordcloud2(color = brewer.pal(8, "Dark2"),minRotation = -pi/6, maxRotation = -pi/6, minSize = 10,
# rotateRatio = 1)
## @knitr wordcloud2
tokens %>% 
  filter(Category == "Accounting & Finance") %>% 
  select(word, freq) %>% 
  with(wordcloud(word, freq, max.words=300, random.order=FALSE,
                 rot.per=0.35, colors=brewer.pal(8, 'Dark2')))
# wordcloud2::wordcloud2(color = brewer.pal(8, "Dark2"),minRotation = pi/6, maxRotation = pi/6, minSize = 10,
#                        rotateRatio = 1)
## @knitr wordcloud3
tokens %>% 
  filter(Category == "Healthcare & Nursing") %>% 
  select(word, freq) %>% 
  with(wordcloud(word, freq, max.words=300, random.order=FALSE,
                 rot.per=0.35, colors=brewer.pal(8, 'Dark2')))
# wordcloud2::wordcloud2(color = brewer.pal(8, "Dark2"))


## @knitr biagramsNetwork

wordFreq <- train_data_jobs_clean %>%
  unnest_tokens(bigram, FullDescription, token = "ngrams", n = 2) %>%
  count(bigram) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_graph <- wordFreq %>%
  filter(n > 200) %>%
  graph_from_data_frame()

arrows <- grid::arrow(type = "closed", length = unit(.1, "inches"), angle = 30)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = arrows) +
  geom_node_point(color = "lightblue", size = 2) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, check_overlap = TRUE) +
  theme_void()


## @knitr correlation
keyWords <- c("analyst","excel","manager","qa", 'sql')

wordCorIT <- train_data_jobs_clean %>%
  filter(Category == 'IT') %>% 
  unnest_tokens(word, FullDescription) %>%
  anti_join(stop_words) %>%
  group_by(word) %>%
  filter(n() >= 200) %>%
  pairwise_cor(word, Id, sort = TRUE) %>%
  ungroup()

plDF.it <- wordCorIT  %>%
  filter(item1 %in% keyWords) %>%
  group_by(item1) %>%
  arrange(desc(correlation)) %>%
  slice(1:20) %>%
  ungroup() %>%
  mutate(xOrder = n():1)

cor.it <- plDF.it %>%
  ggplot(aes(x=xOrder,y=correlation,fill=item1)) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ item1,scales='free',nrow=1) +
  scale_x_continuous(breaks = plDF$xOrder,labels = plDF$item2,expand = c(0,0)) + 
  coord_flip()+ 
  theme_bw()+ 
  theme(legend.position = "none")+
  labs(x='Word',y='Correlations',
       title = 'Top Correlated Words in IT')

wordCorFIN <- train_data_jobs_clean %>%
  filter(Category == 'Accounting & Finance') %>% 
  unnest_tokens(word, FullDescription) %>%
  anti_join(stop_words) %>%
  group_by(word) %>%
  filter(n() >= 200) %>%
  pairwise_cor(word, Id, sort = TRUE) %>%
  ungroup()

plDF.fin <- wordCorFIN  %>%
  filter(item1 %in% c(keyWords, 'sas')) %>%
  group_by(item1) %>%
  arrange(desc(correlation)) %>%
  slice(1:20) %>%
  ungroup() %>%
  mutate(xOrder = n():1)

cor.fin <- plDF.fin %>%
  ggplot(aes(x=xOrder,y=correlation,fill=item1)) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ item1,scales='free',nrow=1) +
  scale_x_continuous(breaks = plDF$xOrder,labels = plDF$item2,expand = c(0,0)) + 
  coord_flip()+ 
  theme_bw()+ 
  theme(legend.position = "none")+
  labs(x='Word',y='Correlations',
       title = 'Top Correlated Words in Accounting & Finance')

wordCorHealth <- train_data_jobs_clean %>%
  filter(Category == 'Healthcare & Nursing') %>% 
  unnest_tokens(word, FullDescription) %>%
  anti_join(stop_words) %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, Id, sort = TRUE) %>%
  ungroup()

plDF.health <- wordCorHealth  %>%
  filter(item1 %in% c(keyWords,'sas','statistics','database')) %>%
  group_by(item1) %>%
  arrange(desc(correlation)) %>%
  slice(1:20) %>%
  ungroup() %>%
  mutate(xOrder = n():1)

cor.health <- plDF.health %>%
  ggplot(aes(x=xOrder,y=correlation,fill=item1)) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ item1,scales='free',nrow=1) +
  scale_x_continuous(breaks = plDF$xOrder,labels = plDF$item2,expand = c(0,0)) + 
  coord_flip()+ 
  theme_bw()+ 
  theme(legend.position = "none")+
  labs(x='Word',y='Correlations',
       title = 'Top Correlated Words in Healthcare & Nursing')

## @knitr topic.modelling

jobs_dtm <- DocumentTermMatrix(job_corpus_clean)

library(topicmodels)

jobs_lda <- LDA(jobs_dtm, k = 9, control = list(seed = 1234))

job_topics <- tidy(jobs_lda, matrix = "beta")


job_top_terms <- job_topics %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

job_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip() +
  theme_bw()


job_documents <- tidy(jobs_lda, matrix = "gamma")

job_top_documents <- job_documents %>% 
  group_by(topic) %>%
  top_n(20, gamma) %>%
  ungroup() %>%
  arrange(topic, -gamma)

job_top_documents %>%
  mutate(term = reorder(document, gamma)) %>%
  ggplot(aes(document, gamma, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip() +
  theme_bw()

