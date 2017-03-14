suppressPackageStartupMessages(library(quanteda))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))

CAP.train <- function(text, n) {
  ngrams <- tokenize(char_tolower(text), removePunct = TRUE, 
                     removeNumbers = TRUE, removeTwitter=TRUE, 
                     removeSymbols=TRUE, removeURL=TRUE, 
                     removeSeparators=TRUE, ngrams = n)
  dfm(ngrams, remove=stopwords("english")) # só funciona com n = 1
}

CAP.dfm <- function(text) {
  my_corpus <- corpus(char_tolower(text))
  dfm(my_corpus, remove=stopwords("english"), removePunct = TRUE, 
      removeNumbers = TRUE, removeTwitter=TRUE, 
      removeSymbols=TRUE, removeURL=TRUE, 
      removeSeparators=TRUE)
}

CAP.predictNext <- function(test_tokens, train_tokens){
  test_tokens <- selectFeatures(test_tokens, features(train_tokens))
}
  

twitter_sample <- twitterSample <- readLines("./data/final/en_US/en_US.twitter.txt", skipNul = T, n=10 ^ 5)
pred <- "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"
prob <- c("beer", "pretzels", "soda", "cheese")

# visualizando as tops features
train_tokens <- CAP.train(twitter_sample, 1)
test_tokens <- CAP.train(pred, 1)
topfeatures(train_tokens)
topfeatures(test_tokens)

# ---------
# testes para escolha das features

# não faz sentido
dfm2b <- dfm_select(test_tokens, featnames(train_tokens))

# escolhe as features presentes no traino pelos nome do test, faz mais sentido 
dfm2b <- dfm_select(train_tokens, featnames(test_tokens))

topfeatures(dfm2b, 50)
# -----------------


# escolhe utilizando regexp ()
train_tokens <- CAP.train(twitter_sample, 2:3)
test_tokens <- CAP.train(pred, 1:2)

train_tokens <- CAP.train(twitter_sample, 3)
object.size(train_tokens) / 1024 ^ 2 
test_tokens <- CAP.train(pred, 2)
names <- paste0(featnames(test_tokens), '_\\w')
dfm2b <- dfm_select(train_tokens, names, "keep", valuetype = "regex")

#remove_tokens <- CAP.train(pred, 2)
# dfm2b <- dfm_select(dfm2b, featnames(remove_tokens), "remove")


dfm2b
topfeatures(dfm2b, 50)
featnames(dfm2b)
grep('case', featnames(dfm2b)) # penultima palavra
grep('beer', featnames(dfm2b)) # resposta
as.data.frame(dfm2b[, grep('case_beer', featnames(dfm2b))])



# ----------
train_tokens <- CAP.dfm(twitter_sample)
test_tokens <- CAP.dfm(pred)
topfeatures(train_tokens)
topfeatures(test_tokens)

dfm2b <- dfm_select(train_tokens, featnames(test_tokens))

df <- as.data.frame(dfm2b)

df$score <- rowSums(df)
# colSums(as.data.frame(dfm2b))

nrow(df[df$score>2, ])
row.names(df[df$score>0, ])

dfm2b <- dfm_select(train_tokens, featnames(test_tokens))

my_fcm <- fcm(dfm2b)
my_fcm
my_fcm <- fcm(train_tokens)

df <- as.data.frame(my_fcm[, featnames(test_tokens)])

head(df)

df <- rowSums(df)
df <- sort(df, decreasing = TRUE)

grep('beer', df)
#troxa!