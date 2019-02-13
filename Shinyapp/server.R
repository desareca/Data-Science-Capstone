# load Libraries
suppressMessages(library(shiny))
suppressMessages(library(tm))
suppressMessages(library(dplyr))
suppressMessages(library(shinythemes))
# load n-grams
gram4 <- readRDS("gram4c.rds")
gram3 <- readRDS("gram3c.rds")
gram2 <- readRDS("gram2c.rds")
gram1 <- readRDS("gram1c.rds")
gram1 <- gram1[order(gram1$Freq, decreasing = TRUE),]
# define length maximum of table
obsmaxNgram <- 100
# load profaneWords
con <- file("profaneList.txt", "rb")
profaneWords <- readLines(con, encoding = "UTF-8", skipNul = T, warn = F)
close(con)

# clean the entered text
transfText <- function(text, profaneWords = NULL, stopWords = FALSE,
                       abv = TRUE, cont = TRUE){
      # Create function to modify a text pattern
      f <- content_transformer(function(x, patt1, patt2) gsub(patt1, patt2, x))
      # Download profane words from Robert J Gabriel's github
      # "https://github.com/RobertJGabriel/Google-profanity-words/blob/master/list.txt"
      # Replace abbreviation
      if(abv){
            text <- qdap::replace_abbreviation(text)
      }
      # Replace contraction
      if(abv){
            text <- qdap::replace_contraction(text)
      }
      # Remove punctuation and junk
      text <- gsub("[[:punct:]]", "", text)
      # Remove :
      text <- gsub(":", "", text)
      # Replace Unicode apostrophe with ASCII apostrophe
      text <- gsub( "['']", "'", text)
      # Remove multiple repeating consecutive words
      text <- gsub( "\\b(\\w+)(?:\\s+\\1\\b)+", "\\1", text)
      # Remove multiple repeating consecutive pairs of words
      text <- gsub( "\\b(\\w+\\s\\w+)(\\s\\1)+", "\\1", text)
      
      # Remove numbers
      text <- removeNumbers(text)
      # Transform to tolower
      text <- tolower(text)
      # Remove punctuation y contraccion (optativo)
      text <- removePunctuation(text, preserve_intra_word_contractions = cont)
      # Remove profane words
      if(!is.null(profaneWords)){
            text <- removeWords(text, profaneWords)
      }
      if(stopWords){
            text <- removeWords(text, stopwords("english"))
      }
      # Remove strip extra whitespace
      text <- stripWhitespace(text)
      text <- gsub("^ ", "", text)
      text
}
# Predict words according to a text and the ngrams
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
# Define server logic required to predict next word
shinyServer(function(input, output) {
      fciaMin <- 2
      gram4 <- gram4[gram4$Freq >= fciaMin, ]
      gram3 <- gram3[gram3$Freq >= fciaMin, ]
      gram2 <- gram2[gram2$Freq >= fciaMin, ]
      gram1 <- gram1[order(gram1$Freq, decreasing = TRUE),]

      textTransf <- reactive({
            transfText(input$text, profaneWords = "profaneList.txt")
      })
      word <- reactive({
            x <- textTransf()
            words(x)
      })
      lambda <- reactive({
            L <- data.frame()
            L[1,1] <- input$L4
            L[1,2] <- input$L3
            L[1,3] <- input$L2
            L[1,4] <- input$L1
            colnames(L) <- c("gram4", "gram3", "gram2", "gram1")
            L
      })
      
      ngram <- reactive({
            xx <- word()
            nn <- length(xx)
            ngram <- data.frame()
            if(nn>0){
            ngram <- next2Word(xx, L = lambda(), ngram1 = gram1,
                               ngram2 = gram2, ngram3 = gram3,
                               ngram4 = gram4, max = obsmaxNgram)
            }
      })
      
      output$Pred <- renderDataTable({
            ngram()[1:input$slider,]
      })
      
      output$intext1 <- renderText({nextWord()[1,1]})
      output$table <- renderDataTable({
        a <- ngram()
        a
        })
})
