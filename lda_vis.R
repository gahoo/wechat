library(lda)
library(LDAvis)

#https://computational-communication.com/%E5%8F%AF%E8%A7%86%E5%8C%96/ldavis-intro/
plot_lda <- function(chat_seg_words, word_freq, output='./vis', K=5, G=5000, alpha=0.1, eta=0.02){
  prepare_doc <- function(){
    get.terms <- function(x) {
      index <- match(x, vocab)  # 获取词的ID
      index <- index[!is.na(index)]  #去掉没有查到的，也就是去掉了的词
      rbind(as.integer(index - 1), as.integer(rep(1, length(index))))   #生成上图结构
    }
    lapply(chat_seg_words, get.terms)
  }
  
  fit_lda <- function(documents){
    set.seed(357)
    fit <- lda.collapsed.gibbs.sampler(
      documents = documents, K = K, vocab = vocab,
      num.iterations = G, alpha = alpha, eta = eta,
      initial = NULL, burnin = 0, compute.log.likelihood = TRUE)
    theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))  #文档—主题分布矩阵
    phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))  #主题-词语分布矩阵
    term.frequency <- as.integer(word_freq)   #词频
    doc.length <- sapply(documents, function(x) sum(x[2, ])) #每篇文章的长度，即有多少个词
    list(phi=phi, theta=theta, doc.length = doc.length, vocab = vocab,
         term.frequency = term.frequency)
  }
  
  vis_lda <- function(params){
    json <- do.call(createJSON, params)
    #json为作图需要数据，下面用servis生产html文件，通过out.dir设置保存位置
    serVis(json, out.dir = output, open.browser = FALSE)
  }
  
  if(length(word_freq) < 10) return()
  vocab <- names(word_freq)
  documents <- prepare_doc()
  params <- fit_lda(documents)
  vis_lda(params)
  params
}


