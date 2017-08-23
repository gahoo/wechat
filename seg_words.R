library(jiebaR)

who_said <- function(chat){
  plyr::dlply(chat, .variables = 'who', .fun = function(x) as.character(x$msg))
}

mix_words <- function(chat_by_who){
  msg_length <- sapply(chat_by_who, length)
  unlist(sapply(1:length(chat_by_who), function(i){
    tail(chat_by_who[[i]], n=floor(msg_length[i] * 0.5))
  }))
}

seg_words <- function(words, stop, type='mix'){
  cutter <- worker(type = type, stop_word = 'stop.txt')
  fragment_words <- segment(paste0(words, collapse = '\n'), cutter)
  stop_idx <- fragment_words %in% stop
  fragment_words <- fragment_words[!stop_idx]
  words_list <- as.list(fragment_words) #将向量转化为列表
  strsplit(as.character(words_list), split=" ")
}

count_word_freq <- function(words, filter.freq=3, filter.nchar=1){
  word_freq <- sort(table(words), decreasing = TRUE)
  filter_idx <- word_freq < filter.freq | nchar(names(word_freq)) < filter.nchar
  message(sum(!filter_idx))
  word_freq[!filter_idx]
}
