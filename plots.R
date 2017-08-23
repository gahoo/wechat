library(ggplot2)

plot_year_day <- function(chat){
  chat %>%
    ggplot(aes(x = day(date), y= ..count.., fill=who)) +
    geom_bar(position = 'dodge') +
    facet_grid(year(date) ~ month(date)) +
    theme(text = element_text(family = 'SimHei'))
}

plot_hour_bar <- function(chat, position='stack'){
  chat %>%
    ggplot(aes(x = hour(date), y= ..count.., fill=who)) +
    geom_bar(position = position) +
    theme(text = element_text(family = 'SimHei'))
}

plot_day_hour_who_heat <- function(chat){
  chat %>%
    mutate(day = day(date),
           hour = hour(date)) %>%
    group_by(day, hour, who) %>%
    summarise(n = n()) %>%
    ggplot(aes(x = day, y = hour, fill=n)) +
    geom_tile() +
    scale_fill_continuous(low='yellow', high='red') +
    theme(text = element_text(family = 'SimHei')) +
    facet_grid(who~.)
}

plot_time_who_heat <- function(chat, bin=60){
  chat %>%
    mutate(time = (hour(date) * 60 + minute(date)) %/% bin) %>%
    group_by(time, who) %>%
    summarise(n = n()) %>%
    ggplot(aes(x = who, y = time, fill=n)) +
    geom_tile() +
    scale_fill_continuous(low='yellow', high='red') +
    coord_flip() +
    theme(text = element_text(family = 'SimHei')) +
    ggtitle('Who speaks most heatmap', paste(bin, 'minute as bin'))
}

plot_time_wday_who_heat <- function(chat, bin=10){
  chat %>%
    mutate(time = (hour(date) * 60 + minute(date)) %/% 10,
           wday = wday(date)) %>%
    group_by(time, wday, who) %>%
    summarise(n = n()) %>%
    ggplot(aes(x = wday, y = time, fill=n)) +
    geom_tile() +
    scale_fill_continuous(low='yellow', high='red') +
    coord_flip() +
    theme(text = element_text(family = 'SimHei')) +
    facet_grid(who~.) +
    ggtitle('Who speaks most heatmap in weekday by person', paste(bin, 'minute as bin'))
}


plot_all <- function(chat, pdfname){
  cairo_pdf(pdfname, family = 'SimHei', onefile = T, height = 8, width = 12)
  plot_year_day(chat) %>% print
  p <- plot_hour_bar(chat, 'fill') + ggtitle('Who speaks most (Percent)', subtitle = 'by hour')
  print(p)
  p <- plot_hour_bar(chat)
  p <- p + ggtitle('Who speaks most', subtitle = 'by hour')
  print(p)
  pp <- p + facet_wrap(~month(date), nrow=3) + ggtitle('Who speaks most', subtitle = 'by month')
  print(pp)
  pp <- p + facet_wrap(~day(date), nrow=5) + ggtitle('Who speaks most', subtitle = 'by day')
  print(pp)
  plot_time_who_heat(chat) %>% print
  plot_time_who_heat(chat, bin=15) %>% print
  plot_time_who_heat(chat, bin=5) %>% print
  plot_time_wday_who_heat(chat, bin=60) %>% print
  plot_time_wday_who_heat(chat, bin=10) %>% print
  dev.off()
}