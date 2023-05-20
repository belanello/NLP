
library(tokenizers)
library(stringr)
library(dplyr)
library(lexicon)
library(data.table)
library(shinyWidgets)
library(RSQLite)
library(forcats)
library(ggplot2)

getMb <- function(x) paste(round(x/1e+6,2),'MB')

# Function to get indices for subset 
getSubIdx <- function(obj, frac){
  set.seed(1563)
  sub <- floor(length(obj)*frac)
  sample(length(obj),sub)
}

# Define the function to clean the data
# Take list of documents and return data.table



prep <- function(data){
  # Break up documents into sentences
  sentences <- tokenize_sentences(data)
  
  # Break up sentences into words 
  words <- tokenize_words(data.frame(sentences = unlist(sentences))[,1],
                          lowercase = FALSE,
                          strip_numeric = TRUE, simplify = TRUE)
  
  # Subset only the sentences (remove the sentences of length 1)
  len_words <- sapply(words, length)
  idx_subset <- which(len_words > 1)
  words <- words[idx_subset]
  
  # Make a data.table of words for cleaning 
  lengths <- sapply(words, length)
  ids <- rep(1:length(words), times=lengths)
  dt <- data.table(id = ids, word = unlist(words))
  
  # Make different apostrophe same (’ and ')
  # Replace non-alphabets with <UNK>
  # Replace profane words with <UNK>
  
  profs <- unique(c(profanity_alvarez, profanity_arr_bad, profanity_banned))
  profs_cap <- str_to_title(profs)
  profs_upper <- str_to_upper(profs)
  
  dt[, word := str_replace_all(word, "’", "'")]
  dt[str_detect(word, "[^A-Za-z\\.']"), word := "UNK"]
  dt[str_detect(word, ".+\\.com$"), word := "UNK"]
  dt[word %chin% profs, word := "UNK"]
  dt[word %chin% profs_cap, word := "UNK"]
  dt[word %chin% profs_upper, word := "UNK"]
  dt[1:.N]
}
# Define the function to replace the out of vocabulary words with "UNK"
# Take a list of documents or data.table and return data.table


prep2 <- function(data, vocab){
  
  if (!is.data.table(data)){
    data <- prep(data)
    data[!(word %chin% vocab), word := "UNK"]
  }else{
    data[!(word %chin% vocab), word := "UNK"]
  }
  
  data[1:.N]
}

# Define a function to make a data.table of words into a sentence vector 
# Take data.table and return a vector of sentences
makeSentence <- function(dt){
  dt[, sentence := str_c(word, collapse = " "), by = id][,word := NULL]
  dt <- unique(dt)
  as.vector(unlist(dt[,.(sentence)]))
}

# Define the function to make n-grams
# Take a list of sentences and return a vector of ngrams
makeNgram <- function(data, n){
  
  data <- str_c(strrep("BOS ",n-1), data, strrep(" EOS", n-1), sep = "")
  ngram <- tokenize_ngrams(data, n = n, lowercase = FALSE, simplify = TRUE)
  unlist(ngram)
  
}

# Define the function to get the counts of ngrams
# Take a vector of ngrams and return data.table
getNgramCounts <- function(data){
  count <- data.table(ngram = data)
  count[, n := .N, by = ngram]
  count <- unique(count)
  count[order(n, decreasing = TRUE)]
}

# Define a function to generate probability table
# Take data.table of the ngram counts/n and return data.table with probability
getProbTable <- function(data, n){
  if(n == 1){
    data[, `:=` (history = NA,
                 word = ngram,
                 dN = sum(n),
                 prob = n/sum(n))]
    setcolorder(data, c("history", "word", "dN", "n", "prob","ngram"))
    data[1:.N]
  }else{
    pattern = str_glue("^(\\S+", strrep("\\s\\S+", n-2), ")\\s(\\S+)$")
    data[, `:=`(history = str_extract(ngram, 
                                      pattern = pattern, 
                                      group = 1),
                word = str_extract(ngram, 
                                   pattern = pattern, 
                                   group = 2))]
    data[, dN := sum(n), by = history][, prob := n/dN]
    setcolorder(data, c("history", "word", "dN", "n", "prob","ngram"))
    data[1:.N]
  }
}

# Define the functions which gives the probability of each word in the 
# sentence 

# Take a single sentence and return a probability vector
getUniProbs <- function(s){
  
  n <- length(s)
  probs <- rep(0, n)
  for (i in 1:n){
    probs[i] <- uni_table[word == s[i], prob]
  }
  probs
}

getBiProbs <- function(s){
  
  n <- length(s)
  probs <- rep(0, n)
  p <- bi_table[history == "BOS" & word == s[1], prob]
  if(length(p) > 0) probs[1] <- p
  
  for (i in 2:n){
    p <- bi_table[history == s[i-1] & word == s[i], prob]
    if (length(p) > 0) probs[i] <- p
    
  }
  probs
}





getTriProbs <- function(s){
  
  n <- length(s)
  probs <- rep(0, n)
  p <- tri_table[history == "BOS BOS" & word == s[1], prob]
  if(length(p) > 0) probs[1] <- p
  
  p2 <- tri_table[history == paste("BOS", s[1]) & word == s[2], prob]
  if(length(p2) > 0) probs[2] <- p2
  
  for (i in 3:n){
    p <- tri_table[history == paste(s[i-2], s[i-1]) & word == s[i], prob]
    if (length(p) > 0) probs[i] <- p
    
  }
  probs
}

getFourProbs <- function(s){
  
  n <- length(s)
  probs <- rep(0, n)
  p <- four_table[history == "BOS BOS BOS" & word == s[1], prob]
  if(length(p) > 0) probs[1] <- p
  
  p2 <- four_table[history == paste("BOS BOS", s[1]) & word == s[2], prob]
  if(length(p2) > 0) probs[2] <- p2
  
  p3 <- four_table[history == paste("BOS", s[1], s[2]) & word == s[3], prob]
  if(length(p3) > 0) probs[3] <- p3
  
  
  for (i in 4:n){
    p <- four_table[history == paste(s[i-3], s[i-2], s[i-1]) & word == s[i], prob]
    if (length(p) > 0) probs[i] <- p
    
  }
  probs
}

# Define a function to calculate probabilities for each N-gram
# Take a list of sentences
# Return a data.table of id, and four columns of each N-gram probabilities
getProbsDT <- function(sentences){
  # add " EOS" to all the sentences
  sentences <- str_c(sentences, " EOS")
  # Convert a list of sentenes to a list of words per sentence
  sent_words <- sapply(sentences, str_split, 
                       pattern = " ", simplify = TRUE, USE.NAMES = FALSE)
  # The number of elements in a list of sentences 
  len <- length(sentences)
  # The number of elements in a list of words per sentence
  sent_length <- sapply(sent_words, length, USE.NAMES = FALSE)
  # Make a empty data.table
  dt <- data.table(id = rep(1:len, times = sent_length),
                   unigram = rep(0, sum(sent_length)),
                   bigram = rep(0, sum(sent_length)),
                   trigram = rep(0, sum(sent_length)),
                   fourgram = rep(0, sum(sent_length)))
  
  # Subset data.table by sentence id
  # Calculate probabilities and assign them in data.table
  for (i in 1:len){
    dt[id == i, unigram := getUniProbs(sent_words[[i]])] 
    dt[id == i, bigram := getBiProbs(sent_words[[i]])] 
    dt[id == i, trigram := getTriProbs(sent_words[[i]])] 
    dt[id == i, fourgram := getFourProbs(sent_words[[i]])] 
    
  }
  dt[1:.N]
}

library(combinat)

# Generate sets of lambda values systematically
# distributing values of 1 in 0.1 interval into 4 bins in all possible way 
# excluding lambda for unigram = 0

# Find dividing positions
comb <- t(combn(13, 3, simplyfy = TRUE))
pos_n <- choose(13, 3)
m <- matrix(rep(0.1, pos_n*13), nrow = pos_n)

# Assign 0 at dividing positions
for (i in 1:pos_n){
  m[i, comb[i, ]] <- 0
}

# (I refered below function at:
# https://stackoverflow.com/questions/16357962/r-split-numeric-vector-at-position)
splitAt <- function(x, pos){
  list(x[1:(pos[1]-1)], x[pos[1]:(pos[2]-1)],
       x[pos[2]:(pos[3]-1)], x[pos[3]:length(x)])
}

ls <- list()
for (i in 1:pos_n){
  ls <- append(ls, splitAt(m[i,], comb[i,]))
}

results <- sapply(ls, sum)

results_m <- matrix(results, nrow = pos_n, byrow = TRUE)

lambda_dt <- data.table(results_m)
lambda_dt <- lambda_dt[!V1 == 0]

getLogProbs <- function(dt, lambda){
  # Multiply each probability by lambda
  dt[,`:=`(uni_wtd = unigram*lambda[1],
           bi_wtd = bigram*lambda[2],
           tri_wtd = trigram*lambda[3],
           four_wtd = fourgram*lambda[4],
           prob = rep(0, .N))]
  
  # Sum weighted probabilities for each word
  dt[, prob := uni_wtd + bi_wtd + tri_wtd + four_wtd]
  # Get log probabilities for each sentence
  dt[,  sum(log(prob)),  by = id]
  
}

# Take data.table of probabilities + data.table of lambdas
# and return mean log probabilities on different set of lambda
getMeanLogProbs <- function(dt, lambdas){
  
  n_sent <- dt[.N, id]
  n_lambda <- nrow(lambdas)
  logProbs <- matrix(rep(0, n_sent*n_lambda), nrow = n_sent)
  for (i in 1:n_lambda){
    results <- getLogProbs(dt, as.numeric(lambdas[i]))
    logProbs[, i] <- results$V1
  }
  
  colMeans(logProbs)
}

# Take data.table of all the probabilities of the sentences and lambda
# return peplexities of each sentence

getPPL <- function(dt, lambda){
  # Multiply each probability by lambda
  dt[,`:=`(uni_wtd = unigram*lambda[1],
           bi_wtd = bigram*lambda[2],
           tri_wtd = trigram*lambda[3],
           four_wtd = fourgram*lambda[4],
           prob = rep(0, .N))]
  
  # Sum weighted probabilities for each word
  dt[, prob := uni_wtd + bi_wtd + tri_wtd + four_wtd]
  # Get perplexities for each sentence
  dt[,  (exp(sum(log(prob))))^-(1/(.N)),  by = id]
  
}


# ==============================================================================

# conn <- dbConnect(RSQLite::SQLite(), "nlp2.db")
# dbListTables(conn)
# dbWriteTable(conn, "table_all", uni_table)
# dbWriteTable(conn, "table_all", bi_table, append = TRUE)
# dbWriteTable(conn, "table_all", tri_table, append = TRUE)
# dbWriteTable(conn, "table_all", four_table, append = TRUE)
# dbDisconnect(conn)


# Take a single sentence and return a probability vector
# Get predictions of the first word
getP1 <- function(){
  pattern1 <- "BOS BOS BOS"
  pattern2 <- "BOS BOS"
  pattern3 <- "BOS"
  
  conn <- dbConnect(RSQLite::SQLite(), "nlp2.db")
  words <- dbGetQuery(conn, "SELECT word FROM table_all 
                    WHERE history IN (?, ?, ?) OR history IS NULL
                    GROUP BY word
                    ORDER BY SUM(prob_wtd) DESC
                    LIMIT 5", params = c(pattern1,pattern2,pattern3))
  dbDisconnect(conn)
  as.character(unlist(words))
}



# Get predictions by history of one word
getP2 <- function(w){
  pattern1 <- str_glue("BOS BOS ", w)
  pattern2 <- str_glue("BOS ", w)
  pattern3 <- w

  conn <- dbConnect(RSQLite::SQLite(), "nlp2.db")
  words <- dbGetQuery(conn, "SELECT word FROM table_all 
                    WHERE history IN (?, ?, ?) OR history IS NULL
                    GROUP BY word
                    ORDER BY SUM(prob_wtd) DESC
                    LIMIT 5", params = c(pattern1,pattern2,pattern3))
  dbDisconnect(conn)
  as.character(unlist(words))
}



# Get predictions by history of 2 words
getP3 <- function(w1, w2){
  pattern1 <- str_glue("BOS ", w1, " ", w2)
  pattern2 <- str_glue(w1, " ", w2)
  pattern3 <- w2

  conn <- dbConnect(RSQLite::SQLite(), "nlp2.db")
  words <- dbGetQuery(conn, "SELECT word FROM table_all 
                    WHERE history IN (?, ?, ?) OR history IS NULL
                    GROUP BY word
                    ORDER BY SUM(prob_wtd) DESC
                    LIMIT 5", params = c(pattern1,pattern2,pattern3))
  dbDisconnect(conn)
  as.character(unlist(words))
}




# Get predictions by history of 3 words
getPred <- function(w1, w2, w3){
  pattern1 <- str_glue(w1, " ", w2, " ", w3)
  pattern2 <- str_glue(w2, " ", w3)
  pattern3 <- w3

  conn <- dbConnect(RSQLite::SQLite(), "nlp2.db")
  words <- dbGetQuery(conn, "SELECT word FROM table_all 
                    WHERE history IN (?, ?, ?) OR history IS NULL
                    GROUP BY word
                    ORDER BY SUM(prob_wtd) DESC
                    LIMIT 5", params = c(pattern1,pattern2,pattern3))
  dbDisconnect(conn)
  as.character(unlist(words))
}


# Define a function to give predictions for test set
# Take sentence and a vector of predicted words for the first word of the sentence(p1)
# Return the data.table of top5 words that the model predicted and "YES/NO"
# if the word is in predicted words


getTop5 <- function(sentence, pred_n = 5){
  # Add EOS to the sentence
  sentence <- str_glue(sentence, " EOS")
  # Break the sentence into words
  s <- unlist(str_split(sentence, " "))
  n <- length(s)
  
  # Make empty table
  pred <- matrix(rep("", n*pred_n), nrow = n)
  pred[1,] <- getP1()
  pred[2,] <- getP2(s[1])
  pred[3,] <- getP3(s[1], s[2])
  
  if (n > 3){
    for(i in 3:(n-1)){
      pred[i+1,] <- getPred(s[i-2], s[i-1], s[i])
    }
  }
  data.table(word = s, pred)
}




