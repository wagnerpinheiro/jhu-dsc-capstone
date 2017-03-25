library(quanteda)
library(stringi)
library(stringr)
library(dplyr)
library(data.table)

unigm_df <- readRDS("unigram.rds")
bigm_df <- readRDS("bigram.rds")
trigm_df <- readRDS("trigram.rds")
quadgm_df <- readRDS("quadgram.rds")
profanity <- readLines("bad_words.txt", encoding = "UTF-8", warn = FALSE, skipNul = TRUE)

#Cleaning the text files
Clean_String <- function(string){
  
  #To standardize all apostrophes
  temp <- gsub("\u2019", "'", string)
  
  #Remove URLs in strings
  temp <- str_replace_all(temp, " ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "")
  
  #Remove numbers, punctuations except apostrophe, non-graphical characters
  temp <- str_replace_all(temp, "[^[a-zA-Z]['-]\\s]", "")
  
  temp <- tolower(temp)
  temp <- str_replace_all(temp, "[\\s]+", " ")
  return(temp)
}

bigram_predict <- function(wi_1){
  df <- bigm_df[bigm_df$word1 == wi_1,]
  if (nrow(df) == 0) return(unigm_df$Content[1:5])
  
  else {
    df_obs <- mutate(df, prob = round((df$Frequency-2)/sum(df$Frequency),4))
    alpha_bigram <- 1 - sum(df_obs$prob)
    bigram_obs_word <- df$word2[]
    bigram_unobs_df <- unigm_df[!unigm_df$Content %in% bigram_obs_word,]
    bigram_unobs_df <- mutate(bigram_unobs_df, 
                              prob = round(alpha_bigram * bigram_unobs_df$Frequency 
                                           / sum(bigram_unobs_df$Frequency),4))
    df_unobs <- cbind(wi_1, bigram_unobs_df)
    colnames(df_unobs) <- c("word1", "word2", "Frequency", "prob")
    df_unobs <- mutate(df_unobs, Content = paste(df_unobs$word1, df_unobs$word2, sep = "_"))
    df_unobs <- df_unobs[1:5,]
    df_unobs <- df_unobs[, c(1,2,5,3,4)]
    df_bigm <- rbind(df_obs, df_unobs)
    df_bigm <- df_bigm[order(-df_bigm$prob),]
    df_bigm <- df_bigm[complete.cases(df_bigm),]
    
    if (nrow(df_bigm) > 5) return(df_bigm$word2[1:5])
    else return(df_bigm$word2[1:nrow(df_bigm)])
  } 
}

trigram_predict <- function(wi_2, wi_1){
  df <- trigm_df[trigm_df$word1 == wi_2,]
  df <- df[df$word2 == wi_1,]
  if (nrow(df) == 0) return(bigram_predict(wi_1))
  
  else {
    df_obs <- mutate(df, prob = round((df$Frequency-1)/sum(df$Frequency),4))
    alpha_trigram <- 1 - sum(df_obs$prob)
    trigram_obs_word <- df$word3[]
    bigm_obs_df <- bigm_df[bigm_df$word1 == wi_1,]
    trigram_unobs_df <- bigm_obs_df[!bigm_obs_df$word2 %in% trigram_obs_word,]
    trigram_unobs_df <- mutate(trigram_unobs_df, 
                               prob = round(alpha_trigram * trigram_unobs_df$Frequency 
                                            / sum(trigram_unobs_df$Frequency),4))
    trigram_unobs_df$Content <- NULL
    
    if(nrow(trigram_unobs_df) == 0) {
      if (nrow(df_obs) > 5) return(df_obs$word3[1:5])
      else return(df_obs$word3[1:nrow(df_obs)])
      }
    
    else{
      df_unobs <- cbind(wi_2, trigram_unobs_df)
      colnames(df_unobs) <- c("word1", "word2","word3", "Frequency", "prob")
      df_unobs <- mutate(df_unobs, Content = paste(df_unobs$word1, df_unobs$word2, df_unobs$word3, sep = "_"))
      df_unobs <- df_unobs[1:5,]
      df_unobs <- df_unobs[, c(1,2,3,6,4,5)]
      df_trigm <- rbind(df_obs, df_unobs)
      df_trigm <- df_trigm[order(-df_trigm$prob),]
      df_trigm <- df_trigm[complete.cases(df_trigm),]
      
      if (nrow(df_trigm) > 5) return(df_trigm$word3[1:5])
      else return(df_trigm$word3[1:nrow(df_trigm)])
      
    }
  }
}

quadgram_predict <- function(wi_3, wi_2, wi_1){
  df <- quadgm_df[quadgm_df$word1 == wi_3,]
  df <- df[df$word2 == wi_2,]
  df <- df[df$word3 == wi_1,]
  if (nrow(df) == 0) return(trigram_predict(wi_2,wi_1))
  
  else {
    df_obs <- mutate(df, prob = round((df$Frequency-1)/sum(df$Frequency),4))
    alpha_quadgram <- 1 - sum(df_obs$prob)
    quadgram_obs_word <- df$word4[]
    trigm_obs_df <- trigm_df[trigm_df$word1 == wi_2,]
    trigm_obs_df <- trigm_obs_df[trigm_obs_df$word2 == wi_1,]
    quadgram_unobs_df <- trigm_obs_df[!trigm_obs_df$word3 %in% quadgram_obs_word,]
    quadgram_unobs_df <- mutate(quadgram_unobs_df, 
                                prob = round(alpha_quadgram * quadgram_unobs_df$Frequency 
                                             / sum(quadgram_unobs_df$Frequency),4))
    quadgram_unobs_df$Content <- NULL
    
    if(nrow(quadgram_unobs_df) == 0) {
      if (nrow(df_obs) > 5) return(df_obs$word4[1:5])
      else return(df_obs$word4[1:nrow(df_obs)]) 
    }
    
    else{
      df_unobs <- cbind(wi_3, quadgram_unobs_df)
      colnames(df_unobs) <- c("word1", "word2", "word3", "word4", "Frequency", "prob")
      df_unobs <- mutate(df_unobs, Content = paste(df_unobs$word1, df_unobs$word2, df_unobs$word3, df_unobs$word4, sep = "_"))
      df_unobs <- df_unobs[1:5,]
      df_unobs <- df_unobs[, c(1,2,3,4,7,5,6)]
      df_quadgm <- rbind(df_obs, df_unobs)
      df_quadgm <- df_quadgm[order(-df_quadgm$prob),]
      df_quadgm <- df_quadgm[complete.cases(df_quadgm),]
      
      if (nrow(df_quadgm) > 5) return(df_quadgm$word4[1:5])
      else return(df_quadgm$word4[1:nrow(df_quadgm)])
      
    }
  }
}

predictnextword <- function(string){
  string <- as.character(string)
  string <- Clean_String(string)
  string <- strsplit(string, " ")[[1]]
  string <- ifelse(string %in% profanity, "###", string)
  nwords <- length(string)
  
  if (nwords == 1){
    wi_1 <- string
    return(bigram_predict(wi_1))
  }
  
  if (nwords == 2){
    wi_2 <- string[1]
    wi_1 <- string[2]
    return(trigram_predict(wi_2, wi_1))
  }
  
  if (nwords > 2){
    wi_3 <- string[nwords-2]
    wi_2 <- string[nwords-1]
    wi_1 <- string[nwords]
    return(quadgram_predict(wi_3, wi_2, wi_1))
  }
  
}