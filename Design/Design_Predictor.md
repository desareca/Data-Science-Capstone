---
title: "Design Predictor"
author: "crsd"
date: "15 de febrero de 2019"
output: 
      html_document:
            code_folding: hide
            toc: true
            toc_depth: 3
            number_sections: false
            theme: united
            highlight: tango
            css: air.css
            keep_md: true
---

## Introduction

The main objective of this final project is to build a shiny application that is capable of predicting the next word. This report shows the complete process of how the algorithm for predicting words was designed.




## Split datasets.

After making an exploratory analysis of the data, **train**, **test** and **validation** sets are created. For these sets are considered 60%, 20% and 20% respectively.

It is important to mention that the documents must be stored in the folder *./SwiftKey/* within the working directory.




```
          corpus     train      test     validation
size      "598.1 Mb" "359.4 Mb" "120 Mb" "120.2 Mb"
sentences "3336695"  "2002357"  "667120" "667218"  
```



## Cleaning and generation of ngrams.

To generate the ngrams, a random sampling will be carried out that considers **300 samples** of **1% of the size** of the training corpus with replacement.

This with the objective of having a number of relevant samples and avoiding the processing cost that implies having a set of this size. We must consider that this reduction in processing capacity is replaced with processing time, so this adjustment takes a long time.

Each sample is transformed according to:

- Remove punctuation and junk
- Replace Unicode apostrophe with ASCII apostrophe
- Remove multiple repeating consecutive words
- Remove multiple repeating consecutive pairs of words
- Remove profane words
- Replace abbreviation
- Replace contraction
- Remove numbers
- Transform to tolower
- Remove punctuation
- Remove strip extra whitespace

Once the transformation is done, each sample is saved in a specific folder *./TrainGram/*, to later group all into individual ngrams.





```
      gram-1      gram-2    gram-3    gram-4   
mean  "295.06 Kb" "1.74 Mb" "3.31 Mb" "4.48 Mb"
files "300"       "300"     "300"     "300"    
```

To group the ngrams, frequencies **greater than 1** will be considered and more than **65% will appear in each document** for each sample. These samples will be combined using the *dplyr* package.



With this, you obtain ngrams according to the following:


```
      1-gram  2-gram    3-gram    4-gram   
Terms "55917" "359331"  "337547"  "127936" 
size  "4 Mb"  "27.4 Mb" "26.8 Mb" "11.5 Mb"
```

Then histograms for each ngram, considering the 20 most frequent in each case.

![](Design_Predictor_files/figure-html/plotngram-1.png)<!-- -->![](Design_Predictor_files/figure-html/plotngram-2.png)<!-- -->![](Design_Predictor_files/figure-html/plotngram-3.png)<!-- -->![](Design_Predictor_files/figure-html/plotngram-4.png)<!-- -->

## Determination of parameters.

To design the predictor we will use an interpolation algorithm, where the probability of each ngram is multiplied by a parameter $\lambda$, according to the following expression:

$p(t_{i}|t_{i-3}t_{i-2}t_{i-1}) = \lambda_{4}f(t_{i}|t_{i-3}t_{i-2}t_{i-1})+\lambda_{3}f(t_{i}|t_{i-2}t_{i-1})+\lambda_{2}f(t_{i}|t_{i-1})+\lambda_{1}f(t_{i})$

To determine the parameters, review the following document (Algorithm 4.8):

https://core.ac.uk/download/pdf/61910629.pdf



The result of applying the algorithm is shown below:


```
          4-gram 3-gram 2-gram 1-gram
count      58091  37636  25845   6364
parameter  0.454  0.294  0.202   0.05
```

## Tests

For the tests, **2 models** are considered, one with unadjusted parameters, that is, each parameter is worth 0.25 (equal weight) and another with the parameters adjusted according to the previous section.

These models will be tested with **50 samples of 1% of the set** of tests with replacement.

It is considered that for the tests windows with the most probable words will be taken. Windows of 1, 5, 10 and 100 words will be considered.

In addition, 2 metrics, **absolute error** and **preplexity** are considered.

The error to determine how much a word appears in a given window and the perplexity to estimate how much probability appears in said window.

In both cases a small value indicates good performance.



Perplexity results:


```
              window 1  window 5 window 10 window 100
Lambda equal  414828.6 101258.31  52002.69   13430.13
Lambda adjust 267016.2  68425.68  35778.11   11158.36
```

Error results:


```
              window 1 window 5 window 10 window 100
Lambda equal  3499.074 3006.972  2747.678   2135.518
Lambda adjust 3503.780 2996.924  2721.738   2136.748
```

In this case, although the error is similar between the 2 models, the perplexity has better performance with the adjusted parameters.

This is relevant, since this indicates that in the same window, the model with the adjusted parameters delivers the word corresta with a higher probability, a situation that is preferable.


## Next steps.

One of the biggest problems of predicting words is the context, since certain words are used more or less frequently in different contexts.

An improvement to the model would be to adjust the parameters to a given context. This can be done using the test set or other set and some regression algorithm.

---

## References

This report is complemented with:

https://rpubs.com/desareca/Milestone-Report <br/>
https://rpubs.com/desareca/Next-Word-Presentation <br/>

The theoretical references, both for the ngrams and for the interpolation algorithm are found in:

https://en.wikipedia.org/wiki/N-gram <br/> 
https://core.ac.uk/download/pdf/61910629.pdf <br/>
http://www.cs.cornell.edu/courses/cs4740/2014sp/lectures/smoothing+backoff.pdf

---

## Code

*Load the libraries*

```r
suppressMessages(library(tm))
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
```

*Create data sets train, test and validation*

```r
dir = "./SwiftKey/"
language = "en"
seed = 50
prob = c(0.6, 0.2, 0.2)
corpus <- VCorpus(DirSource(dir), readerControl = list(language = language))
CorpusTrain <- corpus
CorpusTest <- corpus
CorpusVal <- corpus
for (i in 1:length(corpus)) {
      set.seed(seed)
      ind <- sample(3, length(corpus[[i]]$content), replace = TRUE, prob = prob)
      CorpusTrain[[i]]$content <- corpus[[i]]$content[ind==1]
      CorpusTest[[i]]$content <- corpus[[i]]$content[ind==2]
      CorpusVal[[i]]$content <- corpus[[i]]$content[ind==3]
}
saveRDS(corpus, "Corpus.rds")
saveRDS(CorpusTrain, "CorpusTrain.rds")
saveRDS(CorpusTest, "CorpusTest.rds")
saveRDS(CorpusVal, "CorpusVal.rds")
remove(prob, corpus, CorpusTrain, CorpusTest, CorpusVal, i, seed, ind)
```

*Show data sets train, test and validation*

```r
corpus <- readRDS("Corpus.rds")
CorpusTrain <- readRDS("CorpusTrain.rds")
CorpusTest <- readRDS("CorpusTest.rds")
CorpusVal <- readRDS("CorpusVal.rds")
lengthCorpus <- 0
lengthCorpusTrain <- 0
lengthCorpusTest <- 0
lengthCorpusVal <- 0
for (i in 1:3) {
      lengthCorpus <- lengthCorpus + length(corpus[[i]]$content)
      lengthCorpusTrain <- lengthCorpusTrain + length(CorpusTrain[[i]]$content)
      lengthCorpusTest <- lengthCorpusTest + length(CorpusTest[[i]]$content)
      lengthCorpusVal <- lengthCorpusVal + length(CorpusVal[[i]]$content)
}
lengthData <- cbind(lengthCorpus, lengthCorpusTrain, lengthCorpusTest,
                   lengthCorpusVal)
sizeData <- cbind(format(object.size(corpus), units = "Mb"),
                  format(object.size(CorpusTrain), units = "Mb"),
                  format(object.size(CorpusTest), units = "Mb"),
                  format(object.size(CorpusVal), units = "Mb"))
Data <- rbind(sizeData, lengthData)
remove(corpus, CorpusTrain, CorpusTest, CorpusVal, lengthCorpus, 
       lengthCorpusTrain, lengthCorpusTest, lengthCorpusVal,
       lengthData, sizeData)
colnames(Data) <- c("corpus", "train", "test", "validation")
rownames(Data) <- c("size", "sentences")
Data
```

*Cleaning and transformation data set training. Create samples for ngram*

```r
CorpusTrain <- readRDS("CorpusTrain.rds")
# funciones para tokenizar
token1gram <- function(x) {
      unlist(lapply(NLP::ngrams(words(x), 1), paste, collapse = " "),
             use.names = FALSE)}
token2gram <- function(x) {
      unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = " "),
             use.names = FALSE)}
token3gram <- function(x) {
      unlist(lapply(NLP::ngrams(words(x), 3), paste, collapse = " "),
             use.names = FALSE)}
token4gram <- function(x) {
      unlist(lapply(NLP::ngrams(words(x), 4), paste, collapse = " "),
             use.names = FALSE)}

# limpieza y transformaciones del texto
transfCorpus <- function(corpus, profaneWords = NULL, stopWords = FALSE,
                         abv = TRUE, cont = TRUE){
      f <- content_transformer(function(x, patt1, patt2) gsub(patt1, patt2, x))
      # Download profane words from Robert J Gabriel's github
      # "https://github.com/RobertJGabriel/Google-profanity-words/blob/master/list.txt"
      if(!is.null(profaneWords)){
            con <- file("profaneList.txt", "rb")
            profaneWords <- readLines(con, encoding = "UTF-8", skipNul = T, warn = F)
            close(con)
      }
            # Replace abbreviation
      if(abv){
            corpus[[1]]$content <- qdap::replace_abbreviation(corpus[[1]]$content)
            corpus[[2]]$content <- qdap::replace_abbreviation(corpus[[2]]$content)
            corpus[[3]]$content <- qdap::replace_abbreviation(corpus[[3]]$content)
      }
      # Replace contraction
      if(cont){
            corpus[[1]]$content <- qdap::replace_contraction(corpus[[1]]$content)
            corpus[[2]]$content <- qdap::replace_contraction(corpus[[2]]$content)
            corpus[[3]]$content <- qdap::replace_contraction(corpus[[3]]$content)
      }
      # Remove punctuation and junk
      corpus <- tm_map(corpus, f, "[[:punct:]]", "")
      # Replace Unicode apostrophe with ASCII apostrophe
      corpus <- tm_map(corpus, f, "['']", "'")
      # Remove multiple repeating consecutive words
      corpus <- tm_map(corpus, f, "\\b(\\w+)(?:\\s+\\1\\b)+", "\\1")
      # Remove multiple repeating consecutive pairs of words
      corpus <- tm_map(corpus, f, "\\b(\\w+\\s\\w+)(\\s\\1)+", "\\1")
      # Remove numbers
      corpus <- tm_map(corpus, removeNumbers)
      # Transform to tolower
      corpus <- tm_map(corpus, content_transformer(tolower))
      # Remove punctuation
      corpus <- tm_map(corpus, removePunctuation)
      # Remove profane words
      if(!is.null(profaneWords)){
            corpus <- tm_map(corpus, removeWords, profaneWords)
      }
      if(stopWords){
            corpus <- tm_map(corpus, removeWords, stopwords(kind = "en"))
      }
      # Remove strip extra whitespace
      corpus <- tm_map(corpus, stripWhitespace)
      corpus
}

dir1 = "./trainGram/"
if(!dir.exists(dir1)){
      dir.create(dir1)
}

n <- 300 # numero de muestras aleatorias
for (k in 1:n) {
      set.seed(10*k) #genera semilla, distinta para cada muestra.
      Train <- CorpusTrain # carga corpus en variable para modificarla
      for (i in 1:length(CorpusTrain)) { # toma muestra de 1% de manera aleatoria y con reemplazo
            ind <- sample(2, length(CorpusTrain[[i]]$content), replace = TRUE,
                          prob = c(0.01, 0.99))
            Train[[i]]$content <- CorpusTrain[[i]]$content[ind==1]
      }
      
      Train <- transfCorpus(Train, profaneWords = "profaneList.txt") # transformaciones
      # genera n-grams
      gram1 <- TermDocumentMatrix(Train, control = list(tokenize = token1gram))
      gram2 <- TermDocumentMatrix(Train, control = list(tokenize = token2gram))
      gram3 <- TermDocumentMatrix(Train, control = list(tokenize = token3gram))
      gram4 <- TermDocumentMatrix(Train, control = list(tokenize = token4gram))
      # guarda cada variable por separado
      saveRDS(gram1, paste0(dir1, "gram1-", k, ".rds"))
      saveRDS(gram2, paste0(dir1, "gram2-", k, ".rds"))
      saveRDS(gram3, paste0(dir1, "gram3-", k, ".rds"))
      saveRDS(gram4, paste0(dir1, "gram4-", k, ".rds"))
}
remove(n, k, Train, CorpusTrain, ind, dir1, token1gram,
       token2gram, token3gram, token4gram, token5gram, token6gram,
       transfCorpus, i, gram1, gram2, gram3, gram4, pb)
```

*Shows the characteristics of the samples created*

```r
a <- dir("./trainGram/")
id1 <- grepl("^gram1", a)
id2 <- grepl("^gram2", a)
id3 <- grepl("^gram3", a)
id4 <- grepl("^gram4", a)
g1 <- a[id1]
g2 <- a[id2]
g3 <- a[id3]
g4 <- a[id4]
size1 <- 0
size2 <- 0
size3 <- 0
size4 <- 0
for (i in 1:300) {
      size1 <- size1 + file.info(paste0("./trainGram/",g1[i]))$size/1024
      size2 <- size2 + file.info(paste0("./trainGram/",g2[i]))$size/1024/1024
      size3 <- size3 + file.info(paste0("./trainGram/",g3[i]))$size/1024/1024
      size4 <- size4 + file.info(paste0("./trainGram/",g4[i]))$size/1024/1024
}
lengthData <- cbind(sum(id1), sum(id2), sum(id3), sum(id4))
sizeData <- cbind(paste(round(size1/sum(id1), digits = 2), "Kb"),
                  paste(round(size2/sum(id2), digits = 2), "Mb"),
                  paste(round(size3/sum(id3), digits = 2), "Mb"),
                  paste(round(size4/sum(id4), digits = 2), "Mb"))
Data <- rbind(sizeData, lengthData)
remove(a, id1, id2, id3, id4, g1, g2, g3, g4, size1, size2, size3, size4, 
       sizeData, lengthData)
colnames(Data) <- c("gram-1", "gram-2", "gram-3", "gram-4")
rownames(Data) <- c("mean", "files")
Data
```

*Join the samples to generate the final ngrams*

```r
source("join.R")
dir1 = "./trainGram/"

joinGram <- function(n, dir1 = "./trainGram/", name = "gram1-", Sparse = FALSE, 
                     factor = 0.8, level.inf = 0){
      gram <- readRDS(paste0(dir1, name, 1, ".rds"))
      if(Sparse){
            gram <- removeSparseTerms(gram, factor)
      }
      gram <- as.matrix(gram)
      gram <- as.matrix(rowSums(gram), ncol = 1)
      colnames(gram) <- "Freq"
      gram <- cbind(gram, word1 = rownames(gram))
      gram <- as.data.frame(gram, row.names = rownames(gram))
      gram$Freq <- as.numeric(as.character(gram$Freq))
      gram <- gram[gram$Freq > level.inf,]
      if(n>1){
            for (i in 2:n) {
                  gram1 <- readRDS(paste0(dir1, name, i, ".rds"))
                  if(Sparse){
                        gram1 <- removeSparseTerms(gram1, factor)
                  }
                  gram1 <- as.matrix(gram1)
                  gram1 <- as.matrix(rowSums(gram1), ncol = 1)
                  colnames(gram1) <- "Freq"
                  gram1 <- cbind(gram1, word1 = rownames(gram1))
                  gram1 <- as.data.frame(gram1, row.names = rownames(gram1))
                  gram1$Freq <- as.numeric(as.character(gram1$Freq))
                  gram1 <- gram1[gram1$Freq > level.inf,]
                  
                  gram <- summarize(group_by(rbind(gram, gram1), word1), Freq = sum(Freq))
            }
      }
      return(data.frame(gram[gram$Freq>level.inf,], stringsAsFactors = FALSE))
}

n = 300
factor = 0.65
level.inf = 1

# Gram con stopwords
gram1 <- joinGram(n, name = "gram1-", dir1 = dir1,
                  Sparse = TRUE, factor = factor, level.inf = level.inf)
saveRDS(gram1, "gram1.rds")
remove(gram1)

gram2 <- joinGram(n, name = "gram2-", dir1 = dir1,
                  Sparse = TRUE, factor = factor, level.inf = level.inf)
saveRDS(gram2, "gram2.rds")
remove(gram2)

gram3 <- joinGram(n, name = "gram3-", dir1 = dir1,
                  Sparse = TRUE, factor = factor, level.inf = level.inf)
saveRDS(gram3, "gram3.rds")
remove(gram3)

gram4 <- joinGram(n, name = "gram4-", dir1 = dir1,
                  Sparse = TRUE, factor = factor, level.inf = level.inf)
saveRDS(gram4, "gram4.rds")
remove(gram4)

remove(dir1, dir2, joinGram, factor, level.inf, n)
```

*Shows the characteristics of the ngrams created*

```r
gram1 <- readRDS("gram1.rds")
gram2 <- readRDS("gram2.rds")
gram3 <- readRDS("gram3.rds")
gram4 <- readRDS("gram4.rds")

ngramData <- cbind(dim(gram1)[1], dim(gram2)[1],
                   dim(gram3)[1], dim(gram4)[1])
object <- cbind(format(object.size(gram1), units = "MB"),
                format(object.size(gram2), units = "MB"),
                format(object.size(gram3), units = "MB"),
                format(object.size(gram4), units = "MB"))
ngramData <- rbind(ngramData, object)
colnames(ngramData) <- c("1-gram", "2-gram", "3-gram", "4-gram")
rownames(ngramData) <- c("Terms", "size")
ngramData
```

*Shows histogram of the ngrams*

```r
nn=20 # Number of bar
# 1-gram
termFreq <- gram1[order(gram1$Freq, decreasing = TRUE),]
termFreq <- termFreq[1:nn,]
g1 <- ggplot(termFreq, aes(x=reorder(word1, Freq), y=Freq)) +
    geom_bar(stat = "identity", fill="red") +  coord_flip() +
    theme(legend.title=element_blank()) +
    xlab("1-gram") + ylab("Frequency") +
    labs(title = "Top 1-grams by frequency")
print(g1)
# 2-gram
termFreq <- gram2[order(gram2$Freq, decreasing = TRUE),]
termFreq <- termFreq[1:nn,]
g1 <- ggplot(termFreq, aes(x=reorder(word1, Freq), y=Freq)) +
    geom_bar(stat = "identity", fill="red") +  coord_flip() +
    theme(legend.title=element_blank()) +
    xlab("2-gram") + ylab("Frequency") +
    labs(title = "Top 2-grams by frequency")
print(g1)
# 3-gram
termFreq <- gram3[order(gram3$Freq, decreasing = TRUE),]
termFreq <- termFreq[1:nn,]
g1 <- ggplot(termFreq, aes(x=reorder(word1, Freq), y=Freq)) +
    geom_bar(stat = "identity", fill="red") +  coord_flip() +
    theme(legend.title=element_blank()) +
    xlab("3-gram") + ylab("Frequency") +
    labs(title = "Top 3-grams by frequency")
print(g1)
# 4-gram
termFreq <- gram4[order(gram4$Freq, decreasing = TRUE),]
termFreq <- termFreq[1:nn,]
g1 <- ggplot(termFreq, aes(x=reorder(word1, Freq), y=Freq)) +
    geom_bar(stat = "identity", fill="red") +  coord_flip() +
    theme(legend.title=element_blank()) +
    xlab("4-gram") + ylab("Frequency") +
    labs(title = "Top 4-grams by frequency")
print(g1)
```

*Calculate the parameters for the interpolation algorithm*

```r
gram4 <- readRDS("gram4.rds")
gram3 <- readRDS("gram3.rds")
gram2 <- readRDS("gram2.rds")
gram1 <- readRDS("gram1.rds")

gram1$word1 <- as.character(gram1$word1)
gram2$word1 <- as.character(gram2$word1)
gram3$word1 <- as.character(gram3$word1)
gram4$word1 <- as.character(gram4$word1)

# Inicializa Lambdas = 0
L1 <- 0
L2 <- 0
L3 <- 0
L4 <- 0

LL <-data.frame()
ll <- 1

# Ejecuta recuento de acuerdo a algoritmo
for (i in 1:dim(gram4)[1]) {
      w <- words(gram4$word1[i])
      c1234 <- gram4$Freq[i]
      I234 <- grepl(paste0("^", w[2], " ", w[3], " ", w[4], "$"), gram3$word1)
      c234 <- gram3$Freq[I234]
      I34 <- grepl(paste0("^", w[3], " ", w[4], "$"), gram2$word1)
      c34 <- gram2$Freq[I34]
      I4 <- grepl(paste0("^", w[4], "$"), gram1$word1)
      c4 <- gram1$Freq[I4]
      
      N <- sum(gram1$Freq)
      
      LL[i,1] <- 0
      LL[i,2] <- 0
      LL[i,3] <- 0
      LL[i,4] <- 0
      
      if(length(c234)==0){c234 <- 0}
      if(length(c34)==0){c34 <- 0}
      if(length(c4)==0){c4 <- 0}
      if(length(N)==0){N <- 0}
      
      if(c234 > 1){LL[i,1] <- (c1234 - 1)/(c234 - 1)}
      if(c34 > 1){LL[i,2] <- (c234 - 1)/(c34 - 1)}
      if(c4 > 1){LL[i,3] <- (c34 - 1)/(c4 - 1)}
      if(N > 1){LL[i,4] <- (c4 - 1)/(N - 1)}

      if(which.max(LL[i,])==1){L4 <- L4 + 1}
      if(which.max(LL[i,])==2){L3 <- L3 + 1}
      if(which.max(LL[i,])==3){L2 <- L2 + 1}
      if(which.max(LL[i,])==4){L1 <- L1 + 1}
      
      ll <- ll + 1
      
}

# L <- L1+L2+L3+L4
L <- 1
L1 <- L1/L
L2 <- L2/L
L3 <- L3/L
L4 <- L4/L

L <- data.frame()
L[1,1] <- L4
L[1,2] <- L3
L[1,3] <- L2
L[1,4] <- L1

names(L) <- c("gram4", "gram3", "gram2", "gram1")
names(LL) <- c("gram4", "gram3", "gram2", "gram1")
saveRDS(L, "Lambdas_ngramas.rds")
saveRDS(LL, "prob_acum_ngramas.rds")

remove(c1234, I234, c234, I34, c34, I4, c4, i, ll, N, L1, L2, L3, L4, w)
```

*Show the parameters*

```r
lambdas <- readRDS("Lambdas_ngramas.rds")
prob <- readRDS("prob_acum_ngramas.rds")

lambdas <- rbind(as.character(lambdas), 
                 round(lambdas/sum(lambdas), digits = 3))
colnames(lambdas) <- c("4-gram", "3-gram", "2-gram", "1-gram")
rownames(lambdas) <- c("count", "parameter")
lambdas
```

*Performs tests*

```r
# Load data
L2 <- readRDS("Lambdas_ngramas.rds")
L1 <- data.frame(gram4 = 1, gram3 = 1, gram2 = 1, gram1 = 1,
                 stringsAsFactors = FALSE)
CorpusTest <- readRDS("CorpusTest.rds")

# Transform text
transfText <- function(text, profaneWords = NULL, stopWords = FALSE,
                       abv = TRUE, cont = TRUE){
      f <- content_transformer(function(x, patt1, patt2) gsub(patt1, patt2, x))
      if(!is.null(profaneWords)){
            con <- file("profaneList.txt", "rb")
            profaneWords <- readLines(con, encoding = "UTF-8", skipNul = T, warn = F)
            close(con)
      }
      text <- gsub("[[:punct:]]", "", text)
      text <- gsub(":", "", text)
      text <- gsub( "['']", "'", text)
      text <- gsub( "\\b(\\w+)(?:\\s+\\1\\b)+", "\\1", text)
      text <- gsub( "\\b(\\w+\\s\\w+)(\\s\\1)+", "\\1", text)
      if(abv){
            text <- qdap::replace_abbreviation(text)
      }
      if(cont){
            text <- qdap::replace_contraction(text)
      }
      text <- removeNumbers(text)
      text <- tolower(text)
      text <- removePunctuation(text, preserve_intra_word_contractions = cont)
      if(!is.null(profaneWords)){
            text <- removeWords(text, profaneWords)
      }
      if(stopWords){
            text <- removeWords(text, stopwords("english"))
      }
      text <- stripWhitespace(text)
      text <- gsub("^ ", "", text)
      text
}

# Sample de corpus test
Test <- data.frame()
ind <- data.frame()

n <- 50 # number of sample
j <- 1
for (k in 1:length(CorpusTest)) {
      set.seed(25*k)
      for (i in 1:n) {
            ind <- sample.int(length(CorpusTest[[k]]$content), 1)
            nl <- length(words(transfText(CorpusTest[[k]]$content[ind])))
            if(nl>5){
                  Test[j,1] <- transfText(CorpusTest[[k]]$content[ind])
                  j <- j + 1
            }
      }
}
remove(j, k, i, CorpusTest, n, ind)
gram1 <- readRDS("gram1c.rds")
gram2 <- readRDS("gram2c.rds")
gram3 <- readRDS("gram3c.rds")
gram4 <- readRDS("gram4c.rds")

# function predict word
next2Word <- function(x, L = c(1,1,1,1), ngram1 = NULL , ngram2 = NULL,
                      ngram3 = NULL, ngram4 = NULL, max = 50){
      gram4 <- data.frame()
      gram3 <- data.frame()
      gram2 <- data.frame()
      gram1 <- data.frame()
      L <- L/sum(L)
      n=length(x)
      obsmax <- max
      if(!is.null(ngram4) & n>2){
            ki = n - 2
            kn = n
            ki <- ifelse(ki<1, 1, ki)
            y <- ""
            for (i in ki:kn) {
                  if(i==ki){y<- x[ki]}
                  if(i>ki){y <- paste(y,x[i])}
            }
            id <- grepl(paste0("^",y,"$"), ngram4$word1)
            gram4 <- ngram4[id, ]
            obsmax <- ifelse(obsmax>dim(gram4)[1], dim(gram4)[1], obsmax)
            gram4 <- gram4[order(gram4$Freq, decreasing = TRUE),]
            gram4 <- gram4[1:obsmax,]
            gram4 <- cbind(gram4, Prob = gram4$Freq/sum(gram4$Freq),
                           Lambda = gram4$Freq*L$gram4/gram4$Freq)
            if(dim(gram4)[1]>0){
                  colnames(gram4) <- c("word1", "nextWord", "Freq", "Prob", "Lambda")
            }
      }
      obsmax <- max
      if(!is.null(ngram3) & n>1){
            ki = n - 1
            kn = n
            ki <- ifelse(ki<1, 1, ki)
            y <- ""
            for (i in ki:kn) {
                  if(i==ki){y<- x[ki]}
                  if(i>ki){y <- paste(y,x[i])}
            }
            id <- grepl(paste0("^",y,"$"), ngram3$word1)
            gram3 <- ngram3[id, ]
            obsmax <- ifelse(obsmax>dim(gram3)[1], dim(gram3)[1], obsmax)
            gram3 <- gram3[order(gram3$Freq, decreasing = TRUE),]
            gram3 <- gram3[1:obsmax,]
            gram3 <- cbind(gram3, Prob = gram3$Freq/sum(gram3$Freq),
                           Lambda = gram3$Freq*L$gram3/gram3$Freq)
            if(dim(gram3)[1]>0){
                  colnames(gram3) <- c("word1", "nextWord", "Freq", "Prob", "Lambda")
            }
      }
      obsmax <- max
      if(!is.null(ngram2) & n>0){
            ki = n
            kn = n
            ki <- ifelse(ki<1, 1, ki)
            y <- ""
            for (i in ki:kn) {
                  if(i==ki){y<- x[ki]}
                  if(i>ki){y <- paste(y,x[i])}
            }
            id <- grepl(paste0("^",y,"$"), ngram2$word1)
            gram2 <- ngram2[id, ]
            obsmax <- ifelse(obsmax>dim(gram2)[1], dim(gram2)[1], obsmax)
            gram2 <- gram2[order(gram2$Freq, decreasing = TRUE),]
            gram2 <- gram2[1:obsmax,]
            gram2 <- cbind(gram2, Prob = gram2$Freq/sum(gram2$Freq),
                           Lambda = gram2$Freq*L$gram2/gram2$Freq)
            if(dim(gram2)[1]>0){
                  colnames(gram2) <- c("word1", "nextWord", "Freq", "Prob", "Lambda")
            }
      }
      gram <- rbind(gram4, gram3, gram2)
      gram <- gram[!is.na(gram$word1),]
      dn <- unique(gram$nextWord)
      gram11 <- data.frame()
      if(length(dn)>0){
            for (i in 1:length(dn)) {
                  gram11[i,1] <- ngram1$word1[ngram1$word1==dn[i]]
                  gram11[i,2] <- ngram1$Freq[ngram1$word1==dn[i]]
                  gram11[i,3] <- ngram1$Freq[ngram1$word1==dn[i]]/sum(ngram1$Freq)
                  gram11[i,4] <- L$gram1
            }
            if(dim(gram11)[1]>0){
                  colnames(gram11) <- c("nextWord", "Freq", "Prob", "Lambda")
            }
            gram <- rbind(gram[,2:5], gram11)
      }
      Prob = gram$Prob*gram$Lambda
      gram <- data.frame(nextWord = gram[,1], Prob = Prob,
                         stringsAsFactors = FALSE)
      if(dim(gram)[1]>0){
            gram <- summarise(group_by(gram, nextWord), Prob=sum(Prob, na.rm = TRUE))
      }
      gram <- data.frame(gram, stringsAsFactors = FALSE)
      gram <- gram[order(gram$Prob, decreasing = TRUE),]
      gram
}

# funtion test
globalTest <- function(Test,L = c(1,1,1,1), l = 4,ngram1 = NULL , ngram2 = NULL,
                       ngram3 = NULL, ngram4 = NULL, obsmax = 10, pmin = 0){
      k <- 1
      Prob <- data.frame()
      l <- 4
      for (j in 1:dim(Test)[1]) {
            xx <- words(Test[j,1])
            nn <- length(words(xx))
            nn <- nn - l
            for (i in 1:nn) {
                  ki = i
                  kf = i + l - 1
                  y <- xx[ki:kf]
                  ref <- xx[kf + 1]
                  ngram <- next2Word(y, L = L, ngram1 = ngram1, ngram2 = ngram2,
                                     ngram3 = ngram3, ngram4 = ngram4, max = 50)
                  if(dim(ngram)[1]>0){
                        ngram <- ngram[order(ngram$Prob, decreasing = TRUE),]
                        N <- ngram$nextWord[1:obsmax]
                        p <- ngram$Prob[1:obsmax]
                  }
                  if(dim(ngram)[1]==0){
                        N <- " "
                        p <- pmin
                  }
                  id <- ref==N
                  N <- N[id]
                  p <- p[id]
                  if(length(N)>0){
                        N <- N[1]
                        p <- p[1]
                  }
                  if(length(N)==0){
                        N <- " "
                        p <- pmin
                  }
                  Prob[k,1] <- ref
                  Prob[k,2] <- N
                  Prob[k,3] <- 1*(ref!=N)
                  Prob[k,4] <- p
                  k <- k+1
            }
      }
      colnames(Prob) <- c("reference", "prediction", "error", "prob")
      Prob$prediction[is.na(Prob$prediction)] <- " "
      Prob$error[is.na(Prob$error)] <- 1
      Prob$prob[is.na(Prob$prob)] <- pmin
      Prob
}

# Estimate prob. min
gmin1 <- gram1$Freq[which.min(gram1$Freq)]/sum(gram1$Freq)
gmin2 <- gram2$Freq[which.min(gram2$Freq)]/sum(gram2$Freq)
gmin3 <- gram3$Freq[which.min(gram3$Freq)]/sum(gram3$Freq)
gmin4 <- gram4$Freq[which.min(gram4$Freq)]/sum(gram4$Freq)

pminL1 <- (gmin1*L1$gram1 + gmin2*L1$gram2 + gmin3*L1$gram3 + gmin4*L1$gram4)/sum(L1)
pminL2 <- (gmin1*L2$gram1 + gmin2*L2$gram2 + gmin3*L2$gram3 + gmin4*L2$gram4)/sum(L2)

# Tests
test1_1 <- globalTest(Test[1],L = L1, l = 4,ngram1 = gram1 , ngram2 = gram2, 
                    ngram3 = gram3, ngram4 = gram4, obsmax = 1, pmin = pminL1)
test1_5 <- globalTest(Test[1],L = L1, l = 4,ngram1 = gram1 , ngram2 = gram2, 
                    ngram3 = gram3, ngram4 = gram4, obsmax = 5, pmin = pminL1)
test1_10 <- globalTest(Test[1],L = L1, l = 4,ngram1 = gram1 , ngram2 = gram2, 
                    ngram3 = gram3, ngram4 = gram4, obsmax = 10, pmin = pminL1)
test1_100 <- globalTest(Test[1],L = L1, l = 4,ngram1 = gram1 , ngram2 = gram2, 
                    ngram3 = gram3, ngram4 = gram4, obsmax = 100, pmin = pminL1)

test2_1 <- globalTest(Test[1],L = L2, l = 4,ngram1 = gram1 , ngram2 = gram2,
                    ngram3 = gram3, ngram4 = gram4, obsmax = 1, pmin = pminL2)
test2_5 <- globalTest(Test[1],L = L2, l = 4,ngram1 = gram1 , ngram2 = gram2,
                    ngram3 = gram3, ngram4 = gram4, obsmax = 5, pmin = pminL2)
test2_10 <- globalTest(Test[1],L = L2, l = 4,ngram1 = gram1 , ngram2 = gram2,
                    ngram3 = gram3, ngram4 = gram4, obsmax = 10, pmin = pminL2)
test2_100 <- globalTest(Test[1],L = L2, l = 4,ngram1 = gram1 , ngram2 = gram2,
                    ngram3 = gram3, ngram4 = gram4, obsmax = 100, pmin = pminL2)

# save the test
saveRDS(test1_1, "test_lambda1_1.rds")
saveRDS(test1_5, "test_lambda1_5.rds")
saveRDS(test1_10, "test_lambda1_10.rds")
saveRDS(test1_100, "test_lambda1_100.rds")

saveRDS(test2_1, "test_lambda2_1.rds")
saveRDS(test2_5, "test_lambda2_5.rds")
saveRDS(test2_10, "test_lambda2_10.rds")
saveRDS(test2_100, "test_lambda2_100.rds")

# Function Perplexity
perplexity <- function(x){
      p <- 0
      n <- dim(x)[1]
      for (i in 1:n) {
            p <- p + log(x$prob[i], base = exp(1))
      }
      exp(-p/n)
}

# Calculate perplexity
perp1_1 <- perplexity(test1_1)
perp1_5 <- perplexity(test1_5)
perp1_10 <- perplexity(test1_10)
perp1_100 <- perplexity(test1_100)

perp2_1 <- perplexity(test2_1)
perp2_5 <- perplexity(test2_5)
perp2_10 <- perplexity(test2_10)
perp2_100 <- perplexity(test2_100)

perp <-rbind(cbind(perp1_1, perp1_5, perp1_10, perp1_100), 
             cbind(perp2_1, perp2_5, perp2_10, perp2_100))
colnames(perp) <- c("window 1", "window 5", "window 10", "window 100")
rownames(perp) <- c("Lambda equal", "Lambda adjust")

saveRDS(perp, "perplexity.rds") 

# Function error
errorAbs <- function(x, pmean, pmin){
      e0 <- sum(x$prob[x$error==0])
      l0 <- dim(x[x$error==0,])[1]
      e1 <- sum(x$prob[x$error==1])
      l1 <- dim(x[x$error==1,])[1]
      sqrt((pmean*l1 - e1)^2 + (pmin*l0 - e0)^2)
}

# Calculate error
error1_1 <- errorAbs(test1_1, 1, pminL1)
error1_5 <- errorAbs(test1_5, 1, pminL1)
error1_10 <- errorAbs(test1_10, 1, pminL1)
error1_100 <- errorAbs(test1_100, 1, pminL1)

error2_1 <- errorAbs(test2_1, 1, pminL2)
error2_5 <- errorAbs(test2_5, 1, pminL2)
error2_10 <- errorAbs(test2_10, 1, pminL2)
error2_100 <- errorAbs(test2_100, 1, pminL2)

error <-rbind(cbind(error1_1, error1_5, error1_10, error1_100), 
             cbind(error2_1, error2_5, error2_10, error2_100))
colnames(error) <- c("window 1", "window 5", "window 10", "window 100")
rownames(error) <- c("Lambda equal", "Lambda adjust")

saveRDS(error, "error.rds") 
```

*Shows the result of the tests*

```r
# Show perplexity
perp <- readRDS("perplexity.rds")
perp
```


```r
# Show error
error <- readRDS("error.rds")
error
```
---

## Sesion info


```r
sessionInfo()
```

```
R version 3.5.2 (2018-12-20)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 17134)

Matrix products: default

locale:
[1] LC_COLLATE=Spanish_Chile.1252  LC_CTYPE=Spanish_Chile.1252   
[3] LC_MONETARY=Spanish_Chile.1252 LC_NUMERIC=C                  
[5] LC_TIME=Spanish_Chile.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] ggplot2_3.1.0 dplyr_0.7.8   tm_0.7-6      NLP_0.2-0    

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.0       knitr_1.21       bindr_0.1.1      xml2_1.2.0      
 [5] magrittr_1.5     munsell_0.5.0    tidyselect_0.2.5 colorspace_1.4-0
 [9] R6_2.3.0         rlang_0.3.1      plyr_1.8.4       stringr_1.4.0   
[13] tools_3.5.2      grid_3.5.2       parallel_3.5.2   gtable_0.2.0    
[17] xfun_0.4         withr_2.1.2      htmltools_0.3.6  lazyeval_0.2.1  
[21] yaml_2.2.0       digest_0.6.18    assertthat_0.2.0 tibble_2.0.1    
[25] crayon_1.3.4     bindrcpp_0.2.2   purrr_0.3.0      glue_1.3.0      
[29] evaluate_0.12    slam_0.1-44      rmarkdown_1.11   stringi_1.2.4   
[33] compiler_3.5.2   pillar_1.3.1     scales_1.0.0     pkgconfig_2.0.2 
```


