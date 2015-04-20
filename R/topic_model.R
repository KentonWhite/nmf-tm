topic_model <- function(ids, texts, num_topics, remove=c(), keywords_percent = 0.1) {
  corpus <- data.frame(id=ids, text=texts)
  extract_topics(corpus, num_topics, remove, keywords_percent)
}

extract_topics <- function(corpus, num_topics, remove, keywords_percent) {
  corpus %>%
  mutate(text=clean_text(text, remove = c('http[^\\b]*\\b', remove))) %>%
  with(qdap::wfm(text, id)) %>%
  wfm_to_binary %>%
  .[row.names(.) %in% frequent_terms(., keywords_percent*nrow(.)),] %>%
  prune_nodes %>%
  nmf_topic_model(num_topics)
}

clean_text <- function(text, remove=c(), stem=TRUE) {
  if(Sys.info()['sysname'] == 'Darwin') {
    text <- iconv(text, to='UTF-8-MAC', sub='byte')
  } else {
    text <- iconv(text, sub='')
  }
  text %>%
  tolower %>%
  gsub('@[^[:space:]]*', '', .) %>%
  gsub('^[^:\\b]:', '', .) %>%
  stringr::str_trim(.) %>%
  iconv(to="ASCII//TRANSLIT") %>%
  gsub(' ̀', '', .) %>%
  tm::removePunctuation(.) %>%
  tm::removeNumbers(.) %>%
  tm::removeWords(., words=c(tm::stopwords('english'), tm::stopwords('french'))) %>%
  {ifelse(rep(stem, length(text)),
  qdap::stemmer(., warn=FALSE, capitalize=FALSE, language=c('english', 'french')),
  .)} %>%
  gsub("\\b[a-zA-Z0-9]{1,2}\\b", "", .) %>%
  tm::removeWords(., words=remove) %>%
  tm::stripWhitespace(.)
}

wfm_to_binary <- function(wfm) {
  wfm[wfm > 1] <- 1
  return(wfm)  
}

frequent_terms <- function(wfm, n=100) {
  freq.terms <- wfm %>%
  rowSums %>%
  sort(decreasing=TRUE)
  
  frequency <- freq.terms[n]
  frequency <- max(frequency, 2)
  freq.terms[freq.terms >= frequency] %>%
  names
}

prune_nodes <-function(wfm) {
  colTotals <- apply(wfm, 2, sum)
  wfm[, colTotals > 0]
}

nmf_topic_model <- function(wfm, num_topics) {  
  mat <- wfm %>%
  reshape2::melt(.) %>%
  filter(value != 0) %>%
  select(1,2) %>%
  unique %>%
  table %>%
  as.data.frame.matrix
  return(NMF::nmf(mat, num_topics, method='lee', seed='nndsvd'))
}


