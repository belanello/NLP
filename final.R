# Download file

url <- 'https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip'
if (!file.exists('Coursera-SwiftKey.zip')){
  download.file(url, 'Coursera-SwiftKey.zip')
}

# Read data
con <- unz('Coursera-SwiftKey.zip','final/en_US/en_US.blogs.txt')
blogs <- readLines(con,skipNul = TRUE)
close(con)

con <- unz('Coursera-SwiftKey.zip','final/en_US/en_US.news.txt')
news <- readLines(con,skipNul = TRUE)
close(con)

con <- unz('Coursera-SwiftKey.zip','final/en_US/en_US.twitter.txt')
twitter <- readLines(con,skipNul = TRUE)
close(con)

# ==============================================================================
# Get test and dev set
all <- c(twitter, blogs, news)
sub_idx <- getSubIdx(all, 0.5)
sub <- all[sub_idx]

# This is a new training set
train2 <- all[-sub_idx]

# training set 
train_idx <- getSubIdx(sub, 0.7)
train <- sub[train_idx]
test_dev <- sub[-train_idx]

# test, devset
test_idx <- getSubIdx(test_dev, 0.66)
test <- test_dev[test_idx]
dev <- test_dev[-test_idx]

train_len <- length(train)
train_new_len <- length(train2)
test_len <- length(test)
dev_len <- length(dev)

# check the length
c(train_len, train_new_len, test_len, dev_len)
sum(c(train_len, train_new_len, test_len, dev_len))
N_training_doc <- sum(c(train_len, train_new_len))
length(all)

# remove files
rm(blogs)
rm(twitter)
rm(news)
rm(all)

# ==============================================================================
# clean train and train2
train_dt <- prep(train)

# write in database
conn <- dbConnect(RSQLite::SQLite(), "nlp.db")
dbWriteTable(conn, "word_dt", train_dt, overwrite = TRUE)
dbListTables(conn)
dbDisconnect(conn)

rm(train_dt)

train2_dt <- prep(train2)
conn <- dbConnect(RSQLite::SQLite(), "nlp.db")
dbWriteTable(conn, "word_dt", train2_dt, append = TRUE)
dbListTables(conn)
dbGetQuery(conn, "SELECT COUNT(word) FROM word_dt")
dbDisconnect(conn)

rm(train2_dt)

# ==============================================================================
# Count words and change words to UNK if appears less than 100 times
conn <- dbConnect(RSQLite::SQLite(), "nlp.db")
wc <- dbGetQuery(conn, "SELECT word, COUNT(word) AS n FROM word_dt 
                        GROUP BY word
                        ORDER BY n DESC")
dbDisconnect(conn)
wc <- data.table(wc)

N <- sum(wc$n)
n_100 <- wc[n >= 100, sum(n)]

vocab <- wc[n >= 100, .(word)]
coverage <- n_100/N

# update words in word_dt to UNK if not in vocab list
conn <- dbConnect(RSQLite::SQLite(), "nlp.db")
dbWriteTable(conn, "vocab", vocab, overwrite = TRUE)
dbListTables(conn)

dbExecute(conn, "UPDATE word_dt 
                  SET word = 'UNK' 
                  WHERE word NOT IN (SELECT word FROM vocab)")

dbDisconnect(conn)

# ==============================================================================
# make sentence table
conn <- dbConnect(RSQLite::SQLite(), "nlp.db")

res <- dbGetQuery(conn, "SELECT GROUP_CONCAT(word) AS sent_words FROM word_dt
                  GROUP BY id")

sent <- data.table(res)
sent[, sent := str_replace_all(sent_words, ",", " ")][, sent_words := NULL]

rm(res)
sent_len <- length(unlist(sent))
# ==============================================================================
# get counts of each N-gram
# update vocab adding EOS(end of sentence)

dbListTables(conn)
unigram <- makeNgram(str_c(sent$sent, " EOS"), 1)
uni_counts <- getNgramCounts(unigram)
dbWriteTable(conn, "uni_counts", uni_counts, overwrite = TRUE)

rm(uni_counts)
dbWriteTable(conn, "vocab", data.table(word = unique(unigram)), overwrite = TRUE)

dbGetQuery(conn, "SELECT count(word) FROM vocab")
rm(unigram)

bigram <- makeNgram(sent$sent, 2)
bi_counts <- getNgramCounts(bigram)
dbWriteTable(conn, "bi_counts", bi_counts, overwrite = TRUE)
rm(bigram)
rm(bi_counts)

trigram <- makeNgram(sent$sent, 3)
tri_counts <- getNgramCounts(trigram)
dbWriteTable(conn, "tri_counts", tri_counts, overwrite = TRUE)
rm(trigram)
rm(tri_counts)


fourgram <- makeNgram(sent$sent, 4)
four_counts <- getNgramCounts(fourgram)
dbWriteTable(conn, "four_counts", four_counts, overwrite = TRUE)
rm(fourgram)
rm(four_counts)
rm(sent)

# ==============================================================================
# Make probability tables and store in db
uni_counts <- dbGetQuery(conn, "SELECT * FROM uni_counts")
uni_counts <- data.table(uni_counts)
uni_table <- getProbTable(uni_counts, 1)
dbWriteTable(conn, "uni_table", uni_table, overwrite = TRUE)
rm(uni_counts)
rm(uni_table)


bi_counts <- dbGetQuery(conn, "SELECT * FROM bi_counts WHERE n >= 5")
bi_counts <- data.table(bi_counts)
bi_counts
bi_table <- getProbTable(bi_counts, 2)
dbWriteTable(conn, "bi_table", bi_table, overwrite = TRUE)
rm(bi_counts)
rm(bi_table)

tri_counts <- dbGetQuery(conn, "SELECT * FROM tri_counts WHERE n >= 5")
tri_counts <- data.table(tri_counts)
tri_counts
tri_table <- getProbTable(tri_counts, 3)
dbWriteTable(conn, "tri_table", tri_table, overwrite = TRUE)
rm(tri_counts)
rm(tri_table)


four_counts <- dbGetQuery(conn, "SELECT * FROM four_counts WHERE n >= 5")
four_counts <- data.table(four_counts)
four_counts
four_table <- getProbTable(four_counts, 4)
dbWriteTable(conn, "four_table", four_table, overwrite = TRUE)
rm(four_counts)
rm(four_table)

dbDisconnect(conn)
# ==============================================================================
# Find optimal lambdas
conn <- dbConnect(RSQLite::SQLite(), "nlp.db")
vocab <- dbGetQuery(conn, "SELECT * FROM vocab")

dev_dt <- prep2(dev, vocab$word)
dev_sent <- makeSentence(dev_dt)

dbListTables(conn)
uni_table <- as.data.table(dbGetQuery(conn, "SELECT * FROM uni_table"))
bi_table <- as.data.table(dbGetQuery(conn, "SELECT * FROM bi_table"))
tri_table <- as.data.table(dbGetQuery(conn, "SELECT * FROM tri_table"))
four_table <- as.data.table(dbGetQuery(conn, "SELECT * FROM four_table"))
dbDisconnect(conn)

keys <- c("history", "word")
setkeyv(uni_table, keys)
setkeyv(bi_table, keys)
setkeyv(tri_table, keys)
setkeyv(four_table, keys)

prob_dt <- getProbsDT(dev_sent[1:10000])
mean_log_prob <- getMeanLogProbs(prob_dt, lambda_dt)
lambda_dt[order(mean_log_prob, decreasing = TRUE)][1:50]

# choose lambdas(highest prob among all N-gram used)
lambda_sub <- lambda_dt[order(mean_log_prob, decreasing = TRUE)][3]

# test which lambdas gives highest accuracy on test set
test_dt <- prep2(test, vocab$word)
test_sent <- makeSentence(test_dt)

# choose lambdas by accuracy
lambdas <- lambda_sub
lambdas <- as.numeric(lambdas)

uni_table[, prob_wtd := prob*lambdas[1]]
bi_table[, prob_wtd := prob*lambdas[2]]
tri_table[, prob_wtd := prob*lambdas[3]]
four_table[, prob_wtd := prob*lambdas[4]]

conn <- dbConnect(RSQLite::SQLite(), "nlp.db")
dbWriteTable(conn, "table_all", uni_table, overwrite = TRUE)
dbWriteTable(conn, "table_all", bi_table, append = TRUE)
dbWriteTable(conn, "table_all", tri_table, append = TRUE)
dbWriteTable(conn, "table_all", four_table, append = TRUE)
dbDisconnect(conn)

# ==============================================================================
# test interpolation model (words with highest probabilities)
yes1 <- 0
no1 <- 0
yes3 <- 0
no3 <- 0
yes5 <- 0
no5 <- 0

for (i in 1:1000){
  print(i)
  sentence <- str_glue("BOS BOS BOS ", test_sent[i], " EOS")
  words <- str_split(sentence, " ", simplify = TRUE)
  n <- length(words)

  for (j in 3:(n-1)){
    print(j)
    params <- c(str_c(words[c(j-2, j-1, j)], collapse = " "), 
                str_c(words[c(j-1, j)], collapse = " "), 
                words[j])
    conn <- dbConnect(RSQLite::SQLite(), "nlp.db")
    options <- dbGetQuery(conn, "SELECT word FROM table_all
                        WHERE history IN (?, ?, ?) OR history IS NULL
                        GROUP BY word
                        ORDER BY SUM(prob_wtd) DESC
                        LIMIT 5", params = params)
    dbDisconnect(conn)
    print(words[j+1])
    print(options$word)
    
    if(words[j+1] == options$word[1]){
      yes1 <- yes1 + 1
    } else {
      no1 <- no1 + 1
    }
    
    
    if(words[j+1] %in% options$word[1:3]){
      yes3 <- yes3 + 1
    } else {
      no3 <- no3 + 1
    }
    if(words[j+1] %in% options$word[1:5]){
      yes5 <- yes5 + 1
    } else {
      no5 <- no5 + 1
    }
  }

}

acc_1 <- yes1/(yes1 + no1)
acc_3 <- yes3/(yes3 + no3)
acc_5 <- yes5/(yes5 + no5)

# ==============================================================================
# # test interpolation model (words sampled according to thier probabilities)
# 
# yes3_sample <- 0
# no3_sample <- 0
# yes5_sample <- 0
# no5_sample <- 0
# 
# for (i in 1:100){
#   print(i)
#   sentence <- str_glue("BOS BOS BOS ", test_sent[i], " EOS")
#   words <- str_split(sentence, " ", simplify = TRUE)
#   n <- length(words)
#   
#   for (j in 3:(n-1)){
#     print(j)
#     params <- c(str_c(words[c(j-2, j-1, j)], collapse = " "), 
#                 str_c(words[c(j-1, j)], collapse = " "), 
#                 words[j])
#     conn <- dbConnect(RSQLite::SQLite(), "nlp.db")
#     options <- dbGetQuery(conn, "SELECT word, SUM(prob_wtd) FROM table_all
#                         WHERE history IN (?, ?, ?) OR history IS NULL
#                         GROUP BY word
#                         ORDER BY SUM(prob_wtd) DESC", params = params)
#     options <- sample(options[,1], size = 5, prob = options[,2])
#     dbDisconnect(conn)
#     print(words[j+1])
#     print(options)
#     
#     if(words[j+1] %in% options[1:3]){
#       yes3_sample <- yes3_sample + 1
#     } else {
#       no3_sample <- no3_sample + 1
#     }
#     if(words[j+1] %in% options[1:5]){
#       yes5_sample <- yes5_sample + 1
#     } else {
#       no5_sample <- no5_sample + 1
#     }
#   }
#   
# }
# yes3_sample/(yes3_sample + no3_sample)
# yes5_sample/(yes5_sample + no5_sample)

# ==============================================================================
# test Stupid back-off model
yes1_sb <- 0
no1_sb <- 0
yes3_sb <- 0
no3_sb <- 0
yes5_sb <- 0
no5_sb <- 0

for (i in 1:1000){
  print(i)
  sentence <- str_glue("BOS BOS BOS ", test_sent[i], " EOS")
  words <- str_split(sentence, " ", simplify = TRUE)
  n <- length(words)
  
  for (j in 3:(n-1)){
    print(j)
    params <- c(str_c(words[c(j-2, j-1, j)], collapse = " "), 
                str_c(words[c(j-1, j)], collapse = " "), 
                words[j])
    conn <- dbConnect(RSQLite::SQLite(), "nlp.db")
    options_sb <- dbGetQuery(conn, "SELECT word FROM table_all
                        WHERE history = ?
                        ORDER BY prob_wtd DESC
                        LIMIT 5", params = params[1])
    options_sb <- options_sb$word
    if (length(options_sb) < 5){
      print("checking trigram")
      options_3 <- dbGetQuery(conn, "SELECT word FROM table_all
                        WHERE history = ?
                        ORDER BY prob_wtd DESC
                        LIMIT 5", params = params[2])
      options_sb <- unique(c(options_sb, options_3$word))
    }
    
    if(length(options_sb) < 5){
      print("checking bigram")
      options_2 <- dbGetQuery(conn, "SELECT word FROM table_all
                        WHERE history = ?
                        ORDER BY prob_wtd DESC
                        LIMIT 5", params = params[3])
      options_sb <- unique(c(options_sb, options_2$word))
    }
    
    if(length(options_sb) < 5){
      print("checking unigram")
      options_1 <- dbGetQuery(conn, "SELECT word FROM table_all
                        WHERE history IS NULL
                        ORDER BY prob_wtd DESC
                        LIMIT 5")
      options_sb <- unique(c(options_sb, options_1$word))
    }
    options_sb <- options_sb[1:5]
    dbDisconnect(conn)
    print(words[j+1])
    print(options_sb)
    
    if(words[j+1] == options$word[1]){
      yes1_sb <- yes1_sb + 1
    } else {
      no1_sb <- no1_sb + 1
    }
    
    if(words[j+1] %in% options_sb[1:3]){
      yes3_sb <- yes3_sb + 1
    } else {
      no3_sb <- no3_sb + 1
    }
    if(words[j+1] %in% options_sb[1:5]){
      yes5_sb <- yes5_sb + 1
    } else {
      no5_sb <- no5_sb + 1
    }
  }
  
}

acc_1_sb <- yes1_sb/(yes1_sb + no1_sb)
acc_3_sb <- yes3_sb/(yes3_sb + no3_sb)
acc_5_sb <- yes5_sb/(yes5_sb + no5_sb)

# ==============================================================================
# make a dataframe of accuracy
acc_df <- data.frame(N.words = c(14271, 14271),
           within_1 = paste(round(c(acc_1, acc_1_sb)*100, 2), "%"),
           within_3 = paste(round(c(acc_3, acc_3_sb)*100, 2), "%"),
           within_5 = paste(round(c(acc_5, acc_5_sb)*100, 2), "%"))
row.names(acc_df) <- c("Interporation", "Stupid_backoff")
saveRDS(acc_df, file = "acc_df.rds")
# ==============================================================================
# theres no need to predict first word in App
# so remove the rows that contains "BOS" at the end of the history

ng1 <- uni_table[, .(history, word, prob_wtd)]
ng2 <- bi_table[str_detect(history, "BOS", negate = TRUE),
                .(history, word, prob_wtd)]
ng3 <- tri_table[str_detect(history, ".+BOS$", negate = TRUE),
                 .(history, word, prob_wtd)]
ng4 <- four_table[str_detect(history, ".+BOS$", negate = TRUE),
                 .(history, word, prob_wtd)]

# put all tables together
all <- rbind(ng1, ng2, ng3, ng4)

object.size(all)/1e6


# change history and word column to ids

vocab$id <- 1:nrow(vocab)
vocab <- data.table(vocab)
all <- vocab[all, on = .(word)]
all[,word := NULL]
all

history <- data.table(all[, unique(history)])
history$hist_id <- 1:nrow(history)
colnames(history) <- c("history", "id")

all <- history[all, on = .(history)]
all[, history := NULL]
colnames(all) <- c("hist_id", "word_id", "prob_wtd")
object.size(all)/1e6
object.size(history)/1e6
object.size(vocab)/1e6

# ==============================================================================
# make database for App

conn <- dbConnect(RSQLite::SQLite(), "NextWordPrediction/tables.db")

dbListTables(conn)
dbWriteTable(conn, 'prob_table', all, overwrite = TRUE)
dbWriteTable(conn, 'vocab', vocab, overwrite = TRUE)
dbWriteTable(conn, 'history', history, overwrite = TRUE)
dbDisconnect(conn)

conn <- dbConnect(RSQLite::SQLite(), "NextWordPrediction/tables.db")
dbExecute(conn, "ALTER TABLE prob_table RENAME TO old_prob_table")
dbListTables(conn)
dbExecute(conn, "ALTER TABLE vocab RENAME TO old_vocab")
dbListTables(conn)
dbExecute(conn, "ALTER TABLE history RENAME TO old_history")
dbListTables(conn)

dbExecute(conn, "CREATE TABLE prob_table (
          hist_id INTEGER NOT NULL,
          word_id INTEGER NOT NULL,
          prob_wtd REAL NOT NULL,
          FOREIGN KEY (hist_id) REFERENCES history (id),
          FOREIGN KEY (word_id) REFERENCES vocab (id))")

dbExecute(conn, "INSERT INTO prob_table SELECT * FROM old_prob_table")
dbGetQuery(conn, "SELECT * FROM prob_table LIMIT 10")

dbExecute(conn, "CREATE TABLE history (
          history TEXT,
          id INTEGER NOT NULL PRIMARY KEY)")
dbExecute(conn, "INSERT INTO history SELECT * FROM old_history")
dbGetQuery(conn, "SELECT * FROM history LIMIT 10")
dbListTables(conn)

dbExecute(conn, "CREATE TABLE vocab (
          word TEXT,
          id INTEGER NOT NULL PRIMARY KEY)")
dbExecute(conn, "INSERT INTO vocab SELECT * FROM old_vocab")


dbExecute(conn, "DROP TABLE old_prob_table")
dbExecute(conn, "DROP TABLE old_history")
dbExecute(conn, "DROP TABLE old_vocab")
dbDisconnect(conn)

# ==============================================================================

