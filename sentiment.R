library(dygraphs)

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

get_timeline_sentiment <- function(chat, type='mix'){
  get_sentiment<- function(x){
    words <- segment(x$msg, cutter)
    sentiment %>%
      filter(word %in% words) %>%
      group_by(sentiment) %>%
      summarise(value = sum(magnitude) )
  }
  
  cutter <- worker(type = type, stop_word = 'stop.txt')
  chat %>%
    mutate(msg = as.character(msg) ) %>%
    plyr::ddply(.variables = c('who', 'date'), .fun = get_sentiment) %>%
    mutate(NP = substr(as.character(sentiment), 1, 1)) %>%
    left_join(sentiment_anno)
}

date_filter <- function(df, minDate, maxDate){
  if(is.null(minDate) && is.null(maxDate)){
    df
  }else if(!is.null(minDate) && !is.null(maxDate)){
    filter(df, date >= minDate, date <= maxDate)
  }else if(!is.null(minDate) && is.null(maxDate)){
    filter(df, date >= minDate)
  }else if(is.null(minDate) && !is.null(maxDate)){
    filter(df, date <= maxDate)
  }
}

plot_sentiment_bar <- function(chat_sentiment, by='blurry.feeling', minDate=NULL, maxDate=NULL, normalize=NULL){
  if(is.null(normalize)){
    total_name <- unique(chat_sentiment$who)
    total <- rep(1, length(total_name))
    names(total) <- total_name
  }else if(normalize == 'count'){
    total <- table(chat_sentiment$who)
  }else if(normalize == 'magnitude'){
    total <- tapply(chat_sentiment$value, chat_sentiment$who, sum)
  }

  chat_sentiment %>%
    date_filter(minDate, maxDate) %>%
    group_by_('who', by) %>%
    summarise(value = sum(value) ) %>%
    mutate(value=value/total[who]) %>%
    ggplot(aes(y=value)) +
    aes_string(x=by, fill=by) +
    geom_bar(stat='identity') +
    coord_flip() +
    facet_grid(who~.,scales = 'free_y', space = 'free_y') +
    theme(text = element_text(family = 'SimHei'))
}

plot_sentiment_point <- function(chat_sentiment, by='blurry.feeling', minDate=NULL, maxDate=NULL){
  chat_sentiment %>%
    date_filter(minDate, maxDate) %>%
    ggplot(aes(x=date, fill=feeling, alpha=value)) +
    aes_string(y=by) +
    geom_point(aes(size=sqrt(value), color=feeling)) +
    facet_grid(who~., scales = 'free', space = 'free') +
    theme(text = element_text(family = 'SimHei'))
}

plot_sentiment_line <- function(chat_sentiment, by='blurry.feeling', minDate=NULL, maxDate=NULL){
  chat_sentiment %>%
    date_filter(minDate, maxDate) %>%
    ggplot(aes(x=date, y=value)) +
    aes_string(color=by) +
    geom_smooth(se=F) +
    facet_grid(who~.) +
    theme(text = element_text(family = 'SimHei'))
}

prepare_who_sentiment_timeseries <- function(chat_sentiment, by='blurry.feeling'){
  plyr::dlply(
    chat_sentiment, .variables = 'who',
    .fun = function(x){
      x <- x %>%
        select_('date', by, 'value') %>%
        group_by_('date', by) %>%
        summarise(value = sum(value)) %>%
        tidyr::spread(by, 'value', fill=0)
      row.names(x)<-x$date
      x$date<-NULL
      x
    })
}


plot_sentiment_timeseries <- function(data, output, ...){
  sentiment_ts <- dygraph(data, ...) %>%
    dyOptions(fillGraph = TRUE, fillAlpha = 0.2) %>%
    dyRangeSelector()
  saveWidget(sentiment_ts, file = output, selfcontained = F)
  sentiment_ts
}

plot_who_sentiment_timeseries <- function(chat_sentiment, conversation, by='blurry.feeling'){
  who_chat_sentiment <- prepare_who_sentiment_timeseries(chat_sentiment, by=by)
  
  lapply(names(who_chat_sentiment), function(who){
    widget_html_path <- file.path('results', conversation, who, 'sentiment')
    dir.create(widget_html_path, showWarnings = F, recursive = T)
    
    plot_sentiment_timeseries(
      who_chat_sentiment[[who]],
      output = file.path(normalizePath(widget_html_path), paste0(by, '.sentiment.html')),
      group = conversation)
  })
}

plot_sentiment_all <- function(chat_sentiment, conversation, pdfname){
  n_who <- length(unique(chat_sentiment$who))
  cairo_pdf(pdfname, family = 'SimHei', onefile = T, height = 4 * n_who, width = 12)
  minDate = as_datetime('2017-02-28 23:30', tz="Asia/Hong_Kong")
  maxDate = as_datetime('2017-07-29 0:00', tz="Asia/Hong_Kong")
  
  for(by in c('NP', 'blurry.feeling', 'feeling')){
    p <- plot_sentiment_bar(chat_sentiment, by=by) + ggtitle('All Bar', subtitle = by)
    print(p)
    
    p <- plot_sentiment_bar(chat_sentiment, by=by, normalize = 'count') +
      ggtitle('All Bar normalized by counts', subtitle = by)
    print(p)
    
    p <- plot_sentiment_bar(chat_sentiment, by=by, normalize = 'magnitude') +
      ggtitle('All Bar normalized by magnitude', subtitle = by)
    print(p)
    
    # p <- plot_sentiment_bar(chat_sentiment, by=by,
    #                           minDate=minDate, maxDate = maxDate) +
    #   ggtitle(paste0('From ', minDate, ' To ', maxDate), subtitle = by)
    # print(p)
    
    p <- plot_sentiment_point(chat_sentiment, by=by) + ggtitle('All Point', subtitle = by)
    print(p)
    # p <- plot_sentiment_point(chat_sentiment, by=by,
    #                           minDate=minDate, maxDate = maxDate) +
    #   ggtitle(paste0('From ', minDate, ' To ', maxDate), subtitle = by)
    # print(p)
    
    p <- plot_sentiment_line(chat_sentiment, by=by) + ggtitle('All Lines', subtitle = by)
    print(p)
    # p <- plot_sentiment_line(chat_sentiment, by=by,
    #                          minDate=minDate, maxDate = maxDate) +
    #   ggtitle(paste0('From ', minDate, ' To ', maxDate), subtitle = by)
    # print(p)
    
    plot_who_sentiment_timeseries(chat_sentiment, conversation, by)
  }
  dev.off()
}
