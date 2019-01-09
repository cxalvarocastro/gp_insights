library(tm)
library(wordcloud)
library(textclean)
library(svDialogs)
library(SnowballC)

# Create clean, n-gram TDMs
createCleanTDM_ngram <- function(textColumn, ngram = 1, stopWordsLanguage = c("en"), excludeWords = c(""), textStemming = FALSE) {
  
  # Text stemming only works with unigram for now, so if ngram > 1 and textStemming = TRUE, throw error message
  if(textStemming == TRUE & ngram > 1) {
    stop("As of now, text stemming doesn't work with anything other than unigrams")
  }
  
  # drop NAs and empty rows from text column
  textColumn <- na.omit(textColumn)
  # textColumn <- textColumn[which(nchar(trimws(textColumn)) > 0)]
  
  # convert dataframe column into a vector
  textVector <- unlist(textColumn)
  
  # if long text file, make sure user commits to long wait
  if(length(textVector) > 5000) {
    continue <- dlgInput("This is a lot of text (> 5,000 rows), and might take a while (roughly 500 rows per minute, with stemming). Want to continue? Type Yes or No: ", Sys.info()["user"])$res
    if(continue == "Yes") {
      print("Starting analysis")
    }
    else stop("Processing stopped")
  }
  
  # convert text vector to a corpus
  textCorpus <- VCorpus(VectorSource(textVector))
  print("Text vector converted to corpus")
  
  # clean the corpus with various functions: convert to lowercase, replace contractions/numbers/ordinals/symbols, remove punctuation, remove common stopwords + custom stopwords
  textCorpus <- tm_map(textCorpus, content_transformer(tolower))
  textCorpus <- tm_map(textCorpus, content_transformer(replace_contraction))
  textCorpus <- tm_map(textCorpus, content_transformer(replace_number))
  textCorpus <- tm_map(textCorpus, content_transformer(replace_ordinal))
  textCorpus <- tm_map(textCorpus, content_transformer(replace_symbol))
  textCorpus <- tm_map(textCorpus, removePunctuation)
  # corpus <- tm_map(corpus, removeNumbers)
  textCorpus <- tm_map(textCorpus, removeWords, 
                       c(stopwords(stopWordsLanguage), excludeWords))
  
  # text stemming is optional: if TRUE, stem the words, they'll be re-completed in a later step
  if(textStemming) {
    textCorpusCopy <- textCorpus
    textCorpus <- tm_map(textCorpus, stemDocument)
    print("Text stemmed")
  }
  
  # finishing text cleaning, removing whitespace
  textCorpus <- tm_map(textCorpus, stripWhitespace)
  print("Corpus cleaned")
  
  # Create tokenizer function using ngram input
  tokenizer <- function(x) {
    unlist(lapply(ngrams(words(x), ngram), paste, collapse = " "), use.names = FALSE)
  }
  
  # create a Term Document Matrix, applying tokenizer function from above step
  textTDM <- TermDocumentMatrix(textCorpus, control = list(tokenize = tokenizer))
  print(paste("nGrams (", ngram, ") created"), sep="")
  print("Corpus converted to TDM")
  
  # if text stemming = TRUE, complete stems that were created in earlier step
  if(textStemming) {
    textTDM$dimnames$Terms <- as.character(stemCompletion(textTDM$dimnames$Terms, textCorpusCopy, type = "prevalent"))
    print("Text stems completed")
  }
  
  # return TDM
  return(textTDM)
}


# Printing wordclouds function
wordcloudWithTable <- function(tdm, colorPalette = "Greens", cloudWords = 30, printWords = 6) {
  
  # print the wordcloud, using cloudWords input for # of words in the cloud and colorpalette from brewer.pal function
  wordcloud(names(rowSums(as.matrix(tdm))), rowSums(as.matrix(tdm)), max.words = cloudWords, rot.per = 0, random.order = FALSE, colors = brewer.pal(7, colorPalette)[-(1:2)])
  
  # convert the tdm to a matrix, find the row sums (which is the frequency count of each word), print the table into the console using printWords for # of words printed
  m <- as.matrix(tdm)
  numCols <- ncol(m)
  v <- sort(rowSums(m), decreasing = TRUE)
  d <- data.frame(word = names(v), freq = v)
  head(d, printWords)
  
}




# Create TDM for comparison cloud
createCleanTDM_comparison <- function(textColumn1, textColumn2, columnNames = c("column1","column2"), stopWordsLanguage = c("en"), excludeWords = c("")) {
  
  # drop NAs and empty rows from text columns
  textColumn1 <- na.omit(textColumn1)
  #textColumn1 <- textColumn1[which(nchar(trimws(textColumn1)) > 0)]
  
  textColumn2 <- na.omit(textColumn2)
  #textColumn2 <- textColumn2[which(nchar(trimws(textColumn2)) > 0)]
  
  # convert dataframe columns into vectors
  textVector1 <- unlist(textColumn1)
  textVector2 <- unlist(textColumn2)
  
  # if long text file, make sure user commits to long wait
  if(length(textVector1) > 1000 | length(textVector2) > 1000) {
    continue <- dlgInput("This is a lot of text (> 1,000 rows), and might take a while (roughly 50 rows per minute). Want to continue? Type Yes or No: ", Sys.info()["user"])$res
    if(continue == "Yes") {
      print("Starting analysis")
    }
    else stop("Processing stopped")
  }
  
  # condense vectors together into one
  all_textVector1 <- paste(textVector1, collapse = "")
  all_textVector2 <- paste(textVector2, collapse = "")
  all_text <- c(all_textVector1, all_textVector2)
  
  # convert to corpus
  all_corpus <- VCorpus(VectorSource(all_text))
  print("Text vector converted to corpus")
  
  # create function to clean the corpus: convert to lowercase, replace contractions/numbers/ordinals/symbols, remove punctuation, remove extra whitespace, remove common stopwords + custom stopwords
  cleanCorpus <- function(corpus){
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, content_transformer(replace_contraction))
    corpus <- tm_map(corpus, content_transformer(replace_number))
    corpus <- tm_map(corpus, content_transformer(replace_ordinal))
    corpus <- tm_map(corpus, content_transformer(replace_symbol))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removeWords, 
                     c(stopwords(stopWordsLanguage), excludeWords))
    return(corpus)
  }
  
  # run the corpus cleaning function on the corpus
  all_clean <- cleanCorpus(all_corpus)
  print("Corpus cleaned")
  
  # convert the corpus into a Term Document Matrix
  all_tdm <- TermDocumentMatrix(all_clean)
  print("Corpus converted to TDM")
  
  # add column names to the Term Document Matrix, default is "column 1" and "column 2"
  colnames(all_tdm) <- columnNames
  
  # return the TDM
  return(all_tdm)
}



# Printing comparison clouds function
comparisonCloudWithTable <- function(tdm, cloudWords = 30, printWords = 6) {
  
  # Print the comparison cloud, cloudWords is the number of words in the cloud (default is 30)
  comparison.cloud(as.matrix(tdm), max.words = cloudWords, rot.per= 0, random.order = 0, colors = c("DarkRed", "DarkGreen"))
  
  # convert the Term Document Matrix to a data frame, calculate the difference in frequency between the two columns, sort the data frame by Difference, print to console
  v <- as.data.frame(as.matrix(tdm))
  v$Difference <- abs(v[,1] - v[,2])
  v <- v[order(-v$Difference),]
  head(v, printWords)
  
}