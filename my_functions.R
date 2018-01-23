library(dplyr)
library(tidytext)
library(tm)
library(ggplot2)
library(widyr)
library(igraph)
library(ggraph)



nokia <- readLines('https://github.com/sudhir-voleti/sample-data-sets/raw/master/text%20analysis%20data/amazon%20nokia%20lumia%20reviews.txt')
text <- nokia

# Function 1 - Text cleaner
clean_text_tokenizer <- function(text){ 
  if (!require(tidytext)) {install.packages("tidytext")}
  if (!require(dplyr)) {install.packages("dplyr")}
  if (!require(tm)) {install.packages("tm")}
  temp  =  gsub("(<.*>)|([^[:alnum:]])", " ", text)         
  temp  =  iconv(temp, "latin1", "ASCII", sub="") 
  temp  =  tolower(temp)                          
  temp  =  gsub("\\d", "", temp)                           
  temp =  stripWhitespace(temp)
  temp  =  gsub("^\\s+|\\s+$", "", temp)          
  temp <- data_frame(document = 1:120, text = temp)
  temp <- temp %>% unnest_tokens(word, text)
  my_stop_words <- c("phone","samsung", "phones", "ii") # my stop words defined
  custom_stop_words <- bind_rows(data_frame(word = my_stop_words, 
                                          lexicon = c("custom")), stop_words) 
  custom_stop_words  = unique(custom_stop_words) # de duplicating
  
  temp <- temp %>%
    anti_join(custom_stop_words)
  return(temp) } # Cleaning function ends


# Function 2 A - for creating DTM matrix 
 dtm_creator <- function(cleaned_text){
   if (!require(tidytext)) {install.packages("tidytext")}
   if (!require(dplyr)) {install.packages("dplyr")}
   if (!require(tm)) {install.packages("tm")}
   temp <- cleaned_text %>% count(document, word, sort = TRUE) %>%
   ungroup()
 # creating DTM 
 dtm_temp <- temp %>%
  cast_sparse(document, word, n)
 return(dtm_temp) } # DTM function ends 
 
 
# Function 2 B - for TF-IDF Matrix
 tf_idf_creator <- function(cleaned_text){
   if (!require(tidytext)) {install.packages("tidytext")}
   if (!require(dplyr)) {install.packages("dplyr")}
   if (!require(tm)) {install.packages("tm")}
   temp <- cleaned_text  %>% count(document, word, sort = TRUE) %>%
     ungroup()
   
   total_temp <- temp %>%  group_by(document) %>% 
   summarize(total = sum(n))  
   temp <- left_join(temp, total_temp)
 
  # creating TF-IDF matrix 
   temp <- temp %>%
   bind_tf_idf(word, document, n)
   tf_tdf_matrix <- temp %>%
     cast_sparse(document, word, tf_idf)
  
   return(tf_tdf_matrix) } # TF-IDF creator function ends


# Function 3A word cloud creator
word_cloud_creator = function(dtm_temp){    
  if (!require(wordcloud)) {install.packages("wordcloud")}
  dtm = as.matrix(dtm_temp)  
  dtm_colsum = apply(dtm, 2, sum)  
  min_word = min(50, length(dtm_colsum))   
  words = colnames(dtm)[1:min_word] 
  freq = 10 * dtm_colsum/mean(dtm_colsum)  
  wordcloud(words,  
            freq,           
            scale = c(8, 0.3),  
            colors=1:10)       
  }  # word cloud creator ends


# Function 3B -COG creator 
cog_creator <- function(dtm_matrix) { # function starts
  if (!require(igraph)) {install.packages("igraph")}
  if (!require(ggraph)) {install.packages("ggraph")}
  docTermMatrix <- as.matrix(dtm_matrix) # document-term-matrix
  termDocMatrix <- t(docTermMatrix) # transposing document-term-matrix
  termDocMatrix[termDocMatrix>=1] <- 1 
  termMatrix <- termDocMatrix %*% t(termDocMatrix) # term-term adjacency matrix
  diag(termMatrix) = 0  # removing the diagonal elements
  termMatrix = termMatrix[1:50, 1:50] # looking at just first 50 rows and column
  graph <- graph.adjacency(termMatrix, weighted=T, mode = "undirected") # build a graph from the above matrix
  graph <- simplify(graph) # removing loops
  V(graph)$label <- colnames(termMatrix) # setting labels of vertices
  V(graph)$degree <- degree(graph) # setting degrees of vertices
  graph = delete.vertices(graph, V(graph)[ degree(graph) == 0 ]) # Deleting lone wolfs 
  set.seed(101) # set seed to make the layout reproducible
  plot(g, layout=layout.kamada.kawai) # Plotting graph
} # Cog_cretaor ends 


# function 3c - barplot_creator
barplot_creator <- function(dtm_matrix){
  freq <- colSums(as.matrix(dtm_matrix))
  barplot(sort(freq))
          } # function barplot_cretaor ends

# Function 3 - Function that creates word-cloud, COG, and Bar-plot 
visualizer <- function(dtm_matrix){
  word_cloud_creator(dtm_matrix)
  cog_creator(dtm_matrix)
  barplot_creator(dtm_matrix)
  }
  


