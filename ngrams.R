
# Libraries loaded

library(dplyr)
library(tm)
library(ggplot2)
library(RWeka)
library(stringi)
library(knitr)
library(slam)

# Data file and connections established

con <- file('en_US.blogs.txt', 'r')
blogsdata <- readLines(con, skipNul = TRUE)
close(con)

con <- file('en_US.news.txt', 'r')
newsdata <- readLines(con, skipNul = TRUE)
close(con)

con <- file('en_US.twitter.txt', 'r')
twitterdata <- readLines(con, skipNul = TRUE)
close(con)


# Random sampling of datasets

set.seed(999)
samplerate <- 0.03
sampledata <- c(sample(blogsdata, length(blogsdata) * samplerate),
                sample(newsdata, length(newsdata) * samplerate),
                sample(twitterdata, length(twitterdata) * samplerate))



# Step through the cleaning processes

myCorpus <- VCorpus(VectorSource(sampledata))
identifier <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
myCorpus <- tm_map(myCorpus, identifier, "(f|ht)tp(s?)://(.*)[.][a-z]+")
myCorpus <- tm_map(myCorpus, identifier, "@[^\\s]+")

# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation)

# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)

# remobve whitespace
myCorpus <- tm_map(myCorpus, stripWhitespace)

# convert to lower
myCorpus <- tm_map(myCorpus, tolower)

# convert to plain text
myCorpus <- tm_map(myCorpus, PlainTextDocument)

# remove english stopwords
singlecorpus <- tm_map(myCorpus, removeWords, stopwords('english'))



# N-gram upto 5-gram

getFreq <- function(tdm) {
        freq <- sort(rowSums(as.matrix(rollup(tdm, 2, FUN = sum)), na.rm = T), decreasing = TRUE)
        return(data.frame(word = names(freq), freq = freq))
}

gram2 <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
gram3 <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
gram4 <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
gram5 <- function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5))


# aggregate frequencies from myCorpus

f1 <- getFreq(removeSparseTerms(TermDocumentMatrix(singlecorpus), 0.999))
save(f1, file="f1data.rds")

f2 <- getFreq(TermDocumentMatrix(myCorpus, control = list(tokenize = gram2, bounds = list(global = c(5, Inf)))))
save(f2, file="f2data.rds")

f3 <- getFreq(TermDocumentMatrix(myCorpus, control = list(tokenize = gram3, bounds = list(global = c(3, Inf)))))
save(f3, file="f3data.rds")

f4 <- getFreq(TermDocumentMatrix(myCorpus, control = list(tokenize = gram4, bounds = list(global = c(2, Inf)))))
save(f4, file="f4data.rds")

f5 <- getFreq(TermDocumentMatrix(myCorpus, control = list(tokenize = gram5, bounds = list(global = c(2, Inf)))))
save(f5, file="f5data.rds")


allf <- list("f1" = f1, "f2" = f2, "f3" = f3, "f4" = f4, "f5" = f5)
save(allf, file="allfdata.rds")


