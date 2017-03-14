# before loading RWeka library.
library(rJava)
.jinit(parameters="-Xmx128g")
options(mc.cores=1)

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(tm))
suppressPackageStartupMessages(library(RWeka))
suppressPackageStartupMessages(library(wordcloud))
#source("http://bioconductor.org/biocLite.R")
#biocLite("Rgraphviz")
suppressPackageStartupMessages(library(Rgraphviz))
suppressPackageStartupMessages(library(quanteda))
suppressPackageStartupMessages(library(knitr))
source("multiplot.R")


# Set the default configuration settings
CAP.config <- c()
CAP.config$url <- 'https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip'
CAP.config$data_path <- "./data"
CAP.config$file_zipped <- 'dataset.zip'
CAP.files <- NA

#' Download e unzip data files
#' @param config Configuration settings
#' @example CAP.download() 
#'
CAP.download <-function(config = CAP.config){
  file = paste0(Config$data_path, '/' , config$file_zipped)
  if(!file.exists(file)){
    if(!dir.exists(config$data_path)){
      dir.create(config$data_path)
    }
    download.file(config$url, file)
  }
  
  if(!dir.exists(paste0(config$data_path,'/final'))){
    unzip(file, exdir = config$data_path)
  } 
  paste0('Dataset downloaded and unzipped in folder: ', Config$data_path, '/final')
}


CAP.fileLinesCount <- function(file){
  system(paste0("wc -l < ", file), intern = TRUE)
}

CAP.fileWordsCount <- function(file){
  system(paste0("wc -w < ", file), intern = TRUE)
}

CAP.fileCharsCount <- function(file){
  system(paste0("wc -m < ", file), intern = TRUE)
}

#' example, draft for a function
CAP.load <- function(){
  twitter <- readLines("data/final/en_US/en_US.twitter.txt", skipNul = T)
  blogs <- readLines("data/final/en_US/en_US.blogs.txt", skipNul = T)
  news <- readLines("data/final/en_US/en_US.news.txt", skipNul = T)
}

CAP.clean <- function(text){
  
}

CAP.ngramFreq <- function(text, n=1){
  ngrams <- tokenize(char_tolower(text), removePunct = TRUE, 
                     removeNumbers = TRUE, removeTwitter=TRUE, 
                     removeSymbols=TRUE, removeURL=TRUE, 
                     removeSeparators=TRUE, ngrams = n)
  my_dfm <- dfm(ngrams, remove=stopwords("english"))
  freq <- colSums(my_dfm)
  gram <- data.frame(ngram=names(freq), freq = freq)
  gram %>% arrange(desc(freq))
  
}


CAP.plotFrequency <- function(filename, lines=10000, top=20){
  sampled_file <- readLines(filename, skipNul = T, n=lines)
  # sampled_file <- iconv(sampled_file, 'UTF-8', 'ASCII', "byte")
  ngram.1 <- CAP.ngramFreq(sampled_file, 1)
  t1 <- ggplot(ngram.1[1:top,], aes(x=reorder(ngram, freq), y=freq)) + 
    geom_bar(stat='identity', col="gray", fill="green", width=0.5) +
    coord_flip() + 
    ggtitle("1-gram") +
    xlab("n-gram") + 
    ylab("") + 
    geom_text(aes(label=freq), hjust=-0.1, size=3)
  
  ngram.2 <- CAP.ngramFreq(sampled_file, 2)
  t2 <- ggplot(ngram.2[1:top,], aes(x=reorder(ngram, freq), y=freq)) + 
    geom_bar(stat='identity', col="gray", fill="blue", width=0.5) +
    coord_flip() + 
    ggtitle("2-gram") +
    xlab("") + 
    ylab("frequency") + 
    geom_text(aes(label=freq), hjust=-0.1, size=3)
  
  
  ngram.3 <- CAP.ngramFreq(sampled_file, 3)
  t3 <- ggplot(ngram.3[1:top,], aes(x=reorder(ngram, freq), y=freq)) + 
    geom_bar(stat='identity', col="gray", fill="purple", width=0.5) +
    coord_flip() + 
    ggtitle("3-gram") +
    xlab("") + 
    ylab("") + 
    geom_text(aes(label=freq), hjust=-0.1, size=3)
  
  multiplot(t1, t2, t3, cols=3)
}

CAP.wordcloud <- function(text, max.words=200){
  corpus <- Corpus(VectorSource(text))
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords, stopwords("english")) 
  corpus <- tm_map(corpus, stemDocument, language = "english")
  corpus <- tm_map(corpus, PlainTextDocument)
  wordcloud(corpus, max.words = max.words, random.order=FALSE, rot.per=0.1, scale=c(2.5,.3), 
            use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
  
}

CAP.createDfm <- function(text, n=1){
  ngrams <- tokenize(char_tolower(text), removePunct = TRUE, 
                   removeNumbers = TRUE, removeTwitter=TRUE, 
                   removeSymbols=TRUE, removeURL=TRUE, 
                   removeSeparators=TRUE, ngrams = n)
  dfm(ngrams, remove=stopwords("english"))
}

CAP.getTerms <- function(text){
  tokenize(char_tolower(text), removePunct = TRUE, 
                     removeNumbers = TRUE, removeTwitter=TRUE, 
                     removeSymbols=TRUE, removeURL=TRUE, 
                     removeSeparators=TRUE, ngrams = 1)
}


#'
#'to-do: falta analizar o contexto
CAP.predictProb <- function(text, prob, gram.model){
  # gram.3 <- CAP.ngramFreq(pred, 3)
  gram.2 <- CAP.ngramFreq(text, 2)
  
  # table(grepl(paste(gram.2$ngram, collapse = "|"), gram.model$ngram))
  
  filtered <- gram.model[grepl(paste(gram.2$ngram, collapse = "|"), gram.model$ngram),]
  filtered$last <- str_match(filtered$ngram, '.*_(.*)$')[,2]
  my_grep <- paste(prob, collapse = "|")
  
  filtered <- filtered %>%
    filter(grepl(my_grep, filtered$ngram)) %>%
    group_by(last) %>%
    summarise(last_freq=sum(freq)) %>%
    arrange(desc(last_freq))
  
  # filtered[grepl(my_grep, filtered$last),]
  filtered[filtered$last %in% prob,]
}