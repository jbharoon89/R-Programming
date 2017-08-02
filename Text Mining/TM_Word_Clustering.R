### Word Clustering
## Continuation of Text Mining Tutorial!!
## Feel free to ask me about the datasets used.

rain <- read.csv("rains.csv", stringsAsFactors = FALSE)

# Create dist_rain
dist_rain <- dist(rain[, 2])

# View the distance matrix
dist_rain

# Create hc
hc <- hclust(dist_rain)

# Plot hc
plot(hc, labels = rain$city)

# Print the dimensions of tweets_tdm
dim(tweets_tdm)

# Create tdm1
tdm1 <- removeSparseTerms(tweets_tdm, sparse = 0.95)

# Create tdm2
tdm2 <- removeSparseTerms(tweets_tdm, sparse = 0.975)

# Print tdm1
tdm1

# Print tdm2
tdm2

# Create tweets_tdm2
tweets_tdm2 <- removeSparseTerms(tweets_tdm, sparse = 0.975)

# Create tdm_m
tdm_m <- as.matrix(tweets_tdm2)

# Create tdm_df
tdm_df <- as.data.frame(tdm_m)

# Create tweets_dist
tweets_dist <- dist(tdm_df)

# Create hc
hc <- hclust(tweets_dist)

# Plot the dendrogram
plot(hc)

# Installing DENDEXTEND PACKAGE
install.packages("dendextend")

library(dendextend)

# Create hc
hc <- hclust(tweets_dist)

# Create hcd
hcd <- as.dendrogram(hc)

# Print the labels in hcd
labels(hcd)

# Change the branch color to red for "marvin" and "gaye"
hcd <- branches_attr_by_labels(hcd, c("marvin", "gaye"), color = "red")

# Plot hcd
plot(hcd, main = "Better Dendrogram")

# Add cluster rectangles 
rect.dendrogram(hcd, k = 2, border = "grey50")

## Using Word Associations

# Create associations
associations <- findAssocs(tweets_tdm, "venti", 0.2)

# View the venti associations
associations

# Create associations_df
associations_df <- list_vect2df(associations)[, 2:3]

# Plot the associations_df values (don't change this)
ggplot(associations_df, aes(y = associations_df[, 1])) + 
  geom_point(aes(x = associations_df[, 2]), 
             data = associations_df, size = 3) + 
  theme_gdocs()

### Tokenizer - Unigrams, Bigrams, Multigrams

install.packages("RWeka")

library(RWeka)

# Make tokenizer function 
tokenizer <- function(x)
  NGramTokenizer(x, Weka_control(min = 2, max = 2))

# Create unigram_dtm
unigram_dtm <- DocumentTermMatrix(text_corp)

# Create bigram_dtm
bigram_dtm <- DocumentTermMatrix(text_corp,
                                 control = list(tokenize = tokenizer))

# Examine unigram_dtm
unigram_dtm

# Examine bigram_dtm
bigram_dtm

# Create bigram_dtm_m
bigram_dtm_m <- as.matrix(bigram_dtm)

# Create freq
freq <- colSums(bigram_dtm_m)

# Create bi_words
bi_words <- names(freq)

# Examine part of bi_words
bi_words[2577:2587]

# Plot a wordcloud
wordcloud(bi_words, freq, max.words = 15)

# Create tf_tdm
tf_tdm <- TermDocumentMatrix(text_corp)

# Create tfidf_tdm
tfidf_tdm <- TermDocumentMatrix(text_corp, control = list(weighting = weightTfIdf))

# Create tf_tdm_m
tf_tdm_m <- as.matrix(tf_tdm)

# Create tfidf_tdm_m 
tfidf_tdm_m <- as.matrix(tfidf_tdm)

# Examine part of tf_tdm_m
tf_tdm_m[508:509, 5:10]

# Examine part of tfidf_tdm_m
tfidf_tdm_m[508:509, 5:10]

# Add author to custom reading list
custom_reader <- readTabular(
  mapping = list(content = "text", 
                 id = "num", 
                 author = "screenName", 
                 date = "created")
)

# Make corpus with custom reading
text_corpus <- VCorpus(
  DataframeSource(tweets), 
  readerControl = list(reader = custom_reader)
)

# Clean corpus
text_corpus <- clean_corpus(text_corpus)

# Print data
text_corpus[[1]][1]

# Print metadata
text_corpus[[1]][2]