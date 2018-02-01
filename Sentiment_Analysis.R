text <- readLines("https://raw.githubusercontent.com/akhandelwal1982/DAM/master/nokia.txt")

emotion_func <- function(text){
  if (!require(tidytext)) {install.packages("tidytext")}
  if (!require(dplyr)) {install.packages("dplyr")}
  if (!require(tm)) {install.packages("tm")}
  if (!require(syuzhet)) {install.packages("syuzhet")}
emotions <- text %>% Clean_Text() %>% get_nrc_sentiment()%>% 
            mutate(review_number = row_number(), polar = positive - negative)
return(emotions)}











