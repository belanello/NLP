library(RSQLite)

getNextWords <- function(params){
  
  conn <- dbConnect(RSQLite::SQLite(), "tables.db")
  words <- dbGetQuery(conn, "SELECT word, SUM(prob_wtd) 
                  FROM prob_table 
                  INNER JOIN history
                  ON hist_id = history.id
                  INNER JOIN vocab ON word_id = vocab.id
                  WHERE history IN (?, ?, ?)
                  OR hist_id = 1
                  GROUP BY word_id
                  ORDER BY SUM(prob_wtd) DESC LIMIT 5", params = params)
  
  dbDisconnect(conn)
  words <- as.character(words[,1])
  words <- words[words != "UNK" & words != "EOS"]
  words[1:3]
}


getNextWords2 <- function(params){
  
  conn <- dbConnect(RSQLite::SQLite(), "tables.db")
  words <- dbGetQuery(conn, "SELECT word 
                  FROM prob_table 
                  INNER JOIN history
                  ON hist_id = history.id
                  INNER JOIN vocab ON word_id = vocab.id
                  WHERE history IN (?, ?, ?)
                  OR hist_id = 1
                  ORDER BY history.id DESC, prob_wtd DESC
                  LIMIT 20", params = params)
  dbDisconnect(conn)
  words <- unique(words$word)[1:5]
  words <- words[words != "UNK" & words != "EOS"]
  words[1:3]
}


checkUNK <- function(words){
  conn <- dbConnect(RSQLite::SQLite(), "tables.db")
  vocab <- dbGetQuery(conn, "SELECT * FROM vocab" )
  dbDisconnect(conn)
  ifelse((words %in% vocab$word) | (words == "BOS"), words, "UNK")
  
}