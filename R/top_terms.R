top_terms <- function(topics, texts) {
  topics %>% 
  {plyr::join(extract_clusters(.), ordered_topics(.))} %>% 
  arrange(-score, -prob) %>%
  mutate(Term = tm::stemCompletion(Term,
  clean_text(texts, remove = c('http[^\\b]*\\b'), stem=FALSE) %>% paste(collapse=' ') %>% strsplit(' ') %>% unlist %>% unique
  )) %>%
  distinct(Term, .keep_all = TRUE) %>%
  group_by(predict, score) %>%
  summarize(terms=toString(Term[1:ifelse(length(Term) < 10, length(Term), 10)])) %>%
  as.data.frame %>%
  arrange(-score)
}

extract_clusters <- function(fit, threshold=0) {
  fit %>%
  t %>%
  NMF::predict(., prob=TRUE) %>%
  as.data.frame  %>%
  mutate(Term=rownames(fit)) %>%
  filter(prob > threshold) %>%
  arrange(predict, desc(prob))
}

ordered_topics <- function(topic.model) {
  t(topic.model) %>% 
  extract_clusters %>%
  group_by(predict) %>% 
  summarize(score = sum(prob)) %>%
  arrange(-score) %>%
  as.data.frame
}
