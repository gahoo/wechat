library(wordcloud2)
library(htmlwidgets)

plot_wordcloud2 <- function(word_freq, output, filter.freq = 3){
  word_freq <- word_freq[word_freq >= filter.freq]
  if(length(word_freq) < 3) return()
  word_freq %>%
    wordcloud2 ->
    wc2
  saveWidget(wc2, file = output, selfcontained = F)
  wc2
}

plot_letterCloud <- function(word_freq, output, word, filter.freq = 3){
  word_freq <- word_freq[word_freq >= filter.freq]
  if(length(word_freq) < 3) return()
  word_freq %>%
    as.data.frame %>%
    rename(word = words) %>%
    letterCloud(word = word) ->
    lwc
  saveWidget(lwc, file = output, selfcontained = F)
  lwc
}