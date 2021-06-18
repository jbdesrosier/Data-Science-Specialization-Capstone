#====
# Load data
#====
blogsFileName <- 'C:/Users/desrjus/Documents/R Files/Coursera/Capstone/Data-Science-Specialization-Capstone/final/en_US/en_US.blogs.txt'
con <- file(blogsFileName, open = "r")
blogs <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
close(con)


newsFileName <- 'C:/Users/desrjus/Documents/R Files/Coursera/Capstone/Data-Science-Specialization-Capstone/final/en_US/en_US.news.txt'
con <- file(newsFileName, open = "r")
news <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
close(con)


twitterFileName <- 'C:/Users/desrjus/Documents/R Files/Coursera/Capstone/Data-Science-Specialization-Capstone/final/en_US/en_US.twitter.txt'
con <- file(twitterFileName, open = "r")
twitter <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
close(con)


#====
# Histogram of wpl
#====
library(stringi)
library(kableExtra)



# file size
fileSizeMB <- round(file.info(c(blogsFileName,
                                newsFileName,
                                twitterFileName))$size / 1024 ^ 2)

# num lines per file
numLines <- sapply(list(blogs, news, twitter), length)

# num characters per file
numChars <- sapply(list(nchar(blogs), nchar(news), nchar(twitter)), sum)

# num words per file
numWords <- sapply(list(blogs, news, twitter), stri_stats_latex)[4,]

# words per line
wpl <- lapply(list(blogs, news, twitter), function(x) stri_count_words(x))

# words per line summary
wplSummary = sapply(list(blogs, news, twitter),
                    function(x) summary(stri_count_words(x))[c('Min.', 'Mean', 'Max.')])
rownames(wplSummary) = c('WPL.Min', 'WPL.Mean', 'WPL.Max')

summary <- data.frame(
  File = c("en_US.blogs.txt", "en_US.news.txt", "en_US.twitter.txt"),
  FileSize = paste(fileSizeMB, " MB"),
  Lines = numLines,
  Characters = numChars,
  Words = numWords,
  t(rbind(round(wplSummary)))
)

kable(summary,
      row.names = FALSE,
      align = c("l", rep("r", 7)),
      caption = "") %>% kable_styling(position = "left")


#======
# Sample and clean teh data
#========
# set seed for reproducability
set.seed(660067)

# assign sample size
sampleSize = 0.01

# sample all three data sets
sampleBlogs <- sample(blogs, length(blogs) * sampleSize, replace = FALSE)
sampleNews <- sample(news, length(news) * sampleSize, replace = FALSE)
sampleTwitter <- sample(twitter, length(twitter) * sampleSize, replace = FALSE)

# remove all non-English characters from the sampled data
sampleBlogs <- iconv(sampleBlogs, "latin1", "ASCII", sub = "")
sampleNews <- iconv(sampleNews, "latin1", "ASCII", sub = "")
sampleTwitter <- iconv(sampleTwitter, "latin1", "ASCII", sub = "")

# combine all three data sets into a single data set and write to disk
sampleData <- c(sampleBlogs, sampleNews, sampleTwitter)
sampleDataFileName <- 'C:/Users/desrjus/Documents/R Files/Coursera/Capstone/Data-Science-Specialization-Capstone/final/en_US/en_US.sample.txt'
con <- file(sampleDataFileName, open = "w")
writeLines(sampleData, con)
close(con)

# get number of lines and words from the sample data set
sampleDataLines <- length(sampleData);
sampleDataWords <- sum(stri_count_words(sampleData))

# remove variables no longer needed to free up memory
rm(blogs, news, twitter, sampleBlogs, sampleNews, sampleTwitter)

#====
# Build Corpus
#====
library(tm)

buildCorpus <- function (dataSet) {
  docs <- VCorpus(VectorSource(dataSet))
  toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
  
  # remove URL, Twitter handles and email patterns
  docs <- tm_map(docs, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
  docs <- tm_map(docs, toSpace, "@[^\\s]+")
  docs <- tm_map(docs, toSpace, "\\b[A-Z a-z 0-9._ - ]*[@](.*?)[.]{1,3} \\b")
  docs <- tm_map(docs, tolower)
  docs <- tm_map(docs, removeWords, stopwords("english"))
  docs <- tm_map(docs, removePunctuation)
  docs <- tm_map(docs, removeNumbers)
  docs <- tm_map(docs, stripWhitespace)
  docs <- tm_map(docs, PlainTextDocument)
  return(docs)
}

# build the corpus and write to disk (RDS)
corpus <- buildCorpus(sampleData)
saveRDS(corpus, file = 'C:/Users/desrjus/Documents/R Files/Coursera/Capstone/Data-Science-Specialization-Capstone/final/en_US/en_US.corpus.rds')

# convert corpus to a dataframe and write lines/words to disk (text)
corpusText <- data.frame(text = unlist(sapply(corpus, '[', "content")), stringsAsFactors = FALSE)
con <- file('C:/Users/desrjus/Documents/R Files/Coursera/Capstone/Data-Science-Specialization-Capstone/final/en_US/en_US.corpus.txt', open = "w")
writeLines(corpusText$text, con)
close(con)

kable(head(corpusText$text, 10),
      row.names = FALSE,
      col.names = NULL,
      align = c("l"),
      caption = "First 10 Documents") %>% kable_styling(position = "left")

# remove variables no longer needed to free up memory
rm(sampleData)

#====
# Tokenization and N-gram generation
#====
library('RWeka')

unigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
bigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
trigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

#====
# Unigrams
#====
library(ggplot2)
# create term document matrix for the corpus
unigramMatrix <- TermDocumentMatrix(corpus, control = list(tokenize = unigramTokenizer))

# eliminate sparse terms for each n-gram and get frequencies of most common n-grams
unigramMatrixFreq <- sort(rowSums(as.matrix(removeSparseTerms(unigramMatrix, 0.99))), decreasing = TRUE)
unigramMatrixFreq <- data.frame(word = names(unigramMatrixFreq), freq = unigramMatrixFreq)

# generate plot
g <- ggplot(unigramMatrixFreq[1:20,], aes(x = reorder(word, -freq), y = freq))
g <- g + geom_bar(stat = "identity", fill = I("grey50"))
g <- g + geom_text(aes(label = freq ), vjust = -0.20, size = 3)
g <- g + xlab("")
g <- g + ylab("Frequency")
g <- g + theme(plot.title = element_text(size = 14, hjust = 0.5, vjust = 0.5),
               axis.text.x = element_text(hjust = 1.0, angle = 45),
               axis.text.y = element_text(hjust = 0.5, vjust = 0.5))
g <- g + ggtitle("20 Most Common Unigrams")
print(g)


