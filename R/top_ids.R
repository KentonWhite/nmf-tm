top_ids <- function(fit, threshold=0) {
  fit %>%
  NMF::predict(., prob=TRUE) %>%
  as.data.frame  %>%
  mutate(id=colnames(fit)) %>%
  filter(prob > threshold) %>%
  arrange(predict, desc(prob))
}
