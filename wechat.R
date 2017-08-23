library(stringr)
library(lubridate)
library(dplyr)
library(magrittr)
source('plots.R')
source('seg_words.R')
source('wordcloud.R')
source('lda_vis.R')
source('sentiment.R')

format_chat <- function(chat_txt){
  date<-"\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}"
  who <- "\\|(.*?):"
  type <- "^\\d+"
  msg <- "\\d{2}:\\d{2}:\\d{2}:(.*)"
  
  df <- data.frame(
    type = str_match(chat_txt, type),
    date = as_datetime(str_match(chat_txt, date), tz="Asia/Hong_Kong"),
    who = str_match(chat_txt, who)[,2],
    msg = str_match(chat_txt, msg)[,2]
  ) %>%
    mutate(who = gsub('me', 'Gahoolee', who)) %>%
    filter(!grepl('^\\.|^\\/|^~', msg))
  df[!is.na(df$date),]
}

load_chat <- function(txt_file){
  chat_txt<-scan(txt_file, what='char', sep='\n')
  chat_txt[grepl("^1\\|", chat_txt)] %>%
    format_chat
}

analyse_conversation <- function(filename, filter.freq=2, filter.nchar=1, K=5, G=3000,
                                 draw.plots=T, draw.wordcloud=T, draw.lda=T, draw.sentiment=T){
  message(filename)
  conversation <- basename(tools::file_path_sans_ext(filename))
  chat <- load_chat(filename)
  dir.create(file.path('results', conversation), showWarnings = F, recursive = T)
  ## plot
  if(draw.plots){
    plot_all(chat, file.path('results', conversation, 'plots.pdf'))
  }

  ## segment words
  chat_by_who <- who_said(chat)
  chat_by_who$MIX <- mix_words(chat_by_who)
  
  chat_seg_words <- lapply(chat_by_who, function(x){
    seg_words(x, stop, 'mix')
  })
  
  word_freqs <- lapply(chat_seg_words, function(x){
    x %>%
      unlist %>%
      count_word_freq(filter.freq = filter.freq, filter.nchar = filter.nchar)
  })
  
  ## wordcloud
  if(draw.wordcloud){
    wcs <- lapply(names(word_freqs), function(who){
      widget_html_path <- file.path('results', conversation, who, 'wordcloud')
      dir.create(widget_html_path, showWarnings = F, recursive = T)
      
      plot_wordcloud2(
        word_freqs[[who]],
        output = file.path(normalizePath(widget_html_path), 'wordcloud2.html'),
        filter.freq = filter.freq)
      
      plot_letterCloud(
        word_freqs[[who]],
        output = file.path(normalizePath(widget_html_path), 'letter_cloud.html'),
        word = who,
        filter.freq = filter.freq)
    })
  }
  
  ## LDA
  if(draw.lda){
    params <- lapply(names(word_freqs), function(who){
      message(who)
      plot_lda(chat_seg_words[[who]], word_freqs[[who]],
               output = file.path('results', conversation, who, 'lda'),
               K=K, G=G)
    })
  }

  ## sentiment
  if(draw.sentiment){
    chat_sentiment <<- get_timeline_sentiment(chat, 'mix')
    sentiment_pdf_file <- file.path('results', conversation, 'sentiment.pdf')
    plot_sentiment_all(chat_sentiment, conversation, sentiment_pdf_file)
  }
  
}

Sys.setlocale(locale="zh_CN.UTF-8")
sentiment <- prepare_sentiment('sentiment/sentiment.txt')
sentiment_anno<-read.delim('sentiment/sentiment_anno.txt', na.strings = '')
stop <- scan('stop.txt', what='char', sep = '\n')
analyse_conversation('dump/elfore.txt', filter.freq = 3, filter.nchar = 1,
                     draw.wordcloud = T, draw.sentiment = T, draw.lda = T,
                     draw.plots = T)
