# Text Mining in R - Bag of Words
# Text mining is the process of distilling actionable insights from text.
install.packages("qdap")
library(qdap)
# Installin Data
text <- "Text mining usually involves the process of structuring the input text.
         The overarching goal is, essentially, to turn text into data for analysis, 
         via application of natural language processing (NLP) and analytical methods."

# Find the 10 most frequent terms: term_count
term_count <- freq_terms(text, 10)

# Plot term_count
plot(term_count)

# Import text data
tweets <- read.csv("coffee.csv", stringsAsFactors = FALSE)

# View the structure of tweets
str(tweets)

# Print out the number of rows in tweets
nrow(tweets)

# Isolate text from tweets: coffee_tweets
coffee_tweets <- tweets$text

# Load tm
library(tm)

# There are two kinds of the corpus data type, the permanent corpus, PCorpus, 
# and the volatile corpus, VCorpus. In essence, the difference between the two
# has to do with how the collection of documents is stored in your computer. 
# We will use the volatile corpus, which is held in the computer's RAM rather 
# than saved to disk, just to be more memory efficient.

# Make a vector source: coffee_source
coffee_source <- VectorSource(coffee_tweets)

# The VCorpus object is a nested list, or list of lists. At each index of the VCorpus
# object, there is a PlainTextDocument object, which is essentially a list that 
# contains the actual text data (content), as well as some corresponding metadata (meta).
# It can help to visualize a VCorpus object to conceptualize the whole thing.

# Make a volatile corpus: coffee_corpus
coffee_corpus <- VCorpus(coffee_source)

# Print out coffee_corpus
coffee_corpus

# Print data on the 15th tweet in coffee_corpus
coffee_corpus[[15]]

# Print the content of the 15th tweet in coffee_corpus
coffee_corpus[[15]] [1]

# Print example_text to the console
example_text

# Create a DataframeSource on columns 2 and 3: df_source
df_source <- DataframeSource(example_text[, 2:3])

# Convert df_source to a corpus: df_corpus
df_corpus <- VCorpus(df_source)

# Examine df_corpus
df_corpus

# Create a VectorSource on column 3: vec_source
vec_source <- VectorSource(example_text[, 3])

# Convert vec_source to a corpus: vec_corpus
vec_corpus <- VCorpus(vec_source)

# Examine vec_corpus
vec_corpus

# Create Object
text1 <- "<b>She</b> woke up at       6 A.M. It\'s so early!  She was only 10% 
          awake and began drinking coffee in front of her computer."

# All lowercase
tolower(text1)

# Remove punctuation
removePunctuation(text1)

# Remove numbers
removeNumbers(text1)

# Remove whitespace
stripWhitespace(text1)

# qdap functions
# Remove text within brackets
bracketX(text1)

# Replace numbers with words
replace_number(text1)

# Replace abbreviations
replace_abbreviation(text1)

# Replace contractions
replace_contraction(text1)

# Replace symbols with words
replace_symbol(text1)

# List standard English stop words
stopwords("en")

# Print text without standard stop words
removeWords(text1, stopwords("en"))

# Add "coffee" and "bean" to the list: new_stops
new_stops <- c("coffee", "bean", stopwords("en"))

# Remove stop words from text
removeWords(text1, new_stops)

# Create complicate
complicate <- c("complicated", "complication", "complicatedly")

# Perform word stemming: stem_doc
stem_doc <- stemDocument(complicate)

# Create the completion dictionary: comp_dict
comp_dict <- "complicate"

# Perform stem completion: complete_text 
complete_text <- stemCompletion(stem_doc, comp_dict)

# Print complete_text
complete_text

# Remove punctuation: rm_punc
rm_punc <- removePunctuation(text_data)

# Create character vector: n_char_vec
n_char_vec <- unlist(strsplit(rm_punc, split = ' '))

# Perform word stemming: stem_doc
stem_doc <- stemDocument(n_char_vec)

# Print stem_doc
stem_doc

# Re-complete stemmed document: complete_doc
complete_doc <- stemCompletion(stem_doc, comp_dict)

# Print complete_doc
complete_doc

# Alter the function code to match the instructions
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "coffee", "mug"))
  return(corpus)
}

# Apply your customized function to the tweet_corp: clean_corp
clean_corp <- clean_corpus(tweet_corp)

# Print out a cleaned up tweet
clean_corp[[227]][1]

# Print out the same tweet in original form
tweets$text[227]

# Create the dtm from the corpus: coffee_dtm
coffee_dtm <- DocumentTermMatrix(clean_corp)

# Print out coffee_dtm data
coffee_dtm

# Convert coffee_dtm to a matrix: coffee_m
coffee_m <- as.matrix(coffee_dtm)

# Print the dimensions of coffee_m
dim(coffee_m)

# Review a portion of the matrix
coffee_m[148:150, 2587:2590]

# Create a TDM from clean_corp: coffee_tdm
coffee_tdm <- TermDocumentMatrix(clean_corp)

# Print coffee_tdm data
coffee_tdm

# Convert coffee_tdm to a matrix: coffee_m
coffee_m <- as.matrix(coffee_tdm)

# Print the dimensions of the matrix
dim(coffee_m)

# Review a portion of the matrix
coffee_m[2587:2590, 148:150]
