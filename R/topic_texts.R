topic_texts <- function(topics, topic_id, ids, texts, threshold=0) {
  topic_ids <- topics %>%
  top_ids(threshold=threshold) %>% 
  filter(predict == topic_id)

  data.frame(
    id=ids[ids %in% topic_ids$id], 
    text=texts[ids %in% topic_ids$id]
  ) %>%
  plyr::join(., topic_ids)
}