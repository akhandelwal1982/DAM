Clean_Text <- function(x){   # x = a text doc
  if (!require(tidytext)) {install.packages("tidytext")}
  if (!require(dplyr)) {install.packages("dplyr")}
  if (!require(tm)) {install.packages("tm")}
  
  x  =  gsub("<.*?>", " ", x)               # regex for removing HTML tags
  x  =  iconv(x, "latin1", "ASCII", sub="") # Keep only ASCII characters
  x  =  gsub("[^[:alnum:]]", " ", x)        # keep only alpha numeric 
  x  =  tolower(x)                          # convert to lower case characters
  x  =  removeNumbers(x)                    # removing numbers
  x  =  stripWhitespace(x)                  # removing white space
  x  =  gsub("^\\s+|\\s+$", "", x)          # remove leading and trailing white space
  
  # removing standard english stopwords like 'the', 'an' etc
  stopwords = tm::stopwords('english')      # tm's inbuilt stopword list; 'tokenizer' too has a stopwords() func, hence 'tm::'
  
  x  =  removeWords(x,stopwords)            # removing stopwords created above
  x  =  stripWhitespace(x)                  # removing white space
 
  
  return(x) }

emotion_func <- function(text){
  if (!require(tidytext)) {install.packages("tidytext")}
  if (!require(dplyr)) {install.packages("dplyr")}
  if (!require(tm)) {install.packages("tm")}
  if (!require(syuzhet)) {install.packages("syuzhet")}
  if (!require(ggplot2)) {install.packages("ggplot")}
emotions <- text %>% Clean_Text() %>% get_nrc_sentiment()%>% 
            mutate(review_number = row_number(), polar = positive - negative)
return(emotions)}









