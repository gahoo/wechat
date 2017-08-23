prepare_sentiment <- function(filename){
  sentiment<-read.delim(filename, na.strings = '')
  sentiment %>%
    filter(!is.na(secondary.sentiment)) %>%
    select(-sentiment, -magnitude, -ploarity) %>%
    rename(sentiment=secondary.sentiment,
           magnitude=secondary.magnitude,
           ploarity=secondary.ploarity) ->
    secondary_sentiment
  
  sentiment %>%
    select(-starts_with('secondary')) %>%
    rbind(secondary_sentiment)
}

words2sentiment_df <- function(words){
  sentiment %>%
    filter(word %in% words) %>%
    group_by(sentiment) %>%
    summarise(value = sum(magnitude) / length(words) ) %>%
    left_join(sentiment_anno)
}

plot_sentiment_bar <- function(chat_seg_words){
  plyr::ldply(chat_seg_words, words2sentiment_df, .id = 'who') %>%
    ggplot(aes(x=feeling, y=value, fill=feeling)) +
    geom_bar(stat='identity') +
    coord_flip() +
    facet_grid(who~.,scales = 'free_y', space = 'free_y') +
    theme(text = element_text(family = 'SimHei'))
}

get_timeline_sentiment <- function(chat, type='mix'){
  cutter <- worker(type = type, stop_word = 'stop.txt')
  chat %>%
    mutate(msg = as.character(msg) ) %>%
    plyr::ddply(.variables = c('who', 'date'), .fun = function(x){
      words <- segment(x$msg, cutter)
      sentiment %>%
        filter(word %in% words) %>%
        group_by(sentiment) %>%
        summarise(value = sum(magnitude) )
    }) %>%
    left_join(sentiment_anno)
}
