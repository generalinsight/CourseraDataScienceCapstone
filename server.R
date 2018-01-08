# Define server logic


library(shiny)
library(stringi)
library(stylo)
library(RWeka)
library(tm)
library(markdown)


# load files

gram2 <- readRDS(file = "f2data.rds")
gram3 <- readRDS(file = "f3data.rds")
gram4 <- readRDS(file = "f4data.rds")
gram5 <- readRDS(file = "f5data.rds")


###
##gram4 <- readRDS(file="./data/gram4.rds")
#gram3 <- readRDS(file="./data/gram3.rds")
#gram2 <- readRDS(file="./data/gram2.rds")


# clean input text entry

textclean <- function(text){
        textInput <- tolower(text)
        textInput <- removePunctuation(textInput)
        textInput <- removeNumbers(textInput)
        textInput <- stri_replace_all_regex(textInput, "[^[:alnum:]]", " ")
        textInput <- stripWhitespace(textInput)
        textInput <- txt.to.words.ext(textInput, language="English.all", preserve.case = TRUE)
        return(textInput)
}

predictword <- function(numPredictedWord, textInput) {
        wordCount <- length(textInput)
        if (wordCount >= 3) { textInput <- textInput[(wordCount-2):wordCount] }
        
        if (length(textInput)==3) {
                word <- as.character(gram4[gram4$Word1==textInput[1] & gram4$Word2==textInput[2] & 
                                                      gram4$Word3==textInput[3],][1:numPredictedWord,]$Word4)
                if(is.na(word[1])) {
                        word <- as.character(gram3[gram3$Word1==textInput[2] & gram3$Word2==textInput[3],][1:numPredictedWord,]$Word3)
                        if(is.na(word[1])) {
                                word <- as.character(gram2[gram2$Word1==textInput[3],][1:numPredictedWord,]$Word2)
                        }
                }
        } else if (length(textInput)==2) {
                word <- as.character(gram3[gram3$Word1==textInput[1] & gram3$Word2==textInput[2],][1:numPredictedWord,]$Word3)
                if(is.na(word[1])) {
                        word <- as.character(gram2[gram2$Word1==textInput[2],][1:numPredictedWord,]$Word2)
                }
        } else {
                word <- as.character(gram2[gram2$Word1==textInput[1],][1:numPredictedWord,]$Word2)
        }
        return (word)
}

shinyServer <- function (input, output) {
        output$html1 <- renderUI({
                textInput <- input$text1
                textInput <- textclean(textInput)
                predictword <- predictword(input$numPredictedWord, textInput)  
                
                if (input$numPredictedWord==1) {
                        str1 <- "Predicted word: "
                } else {
                        str1 <- "Predicted words: "
                }
                
                str2 <- ""
                for (i in 1:input$numPredictedWord) {
                        if(!is.na(predictword[i])) {
                                predictword[i] <- paste(i, ".", predictword[i])
                                str2 <- paste(str2, "<span style=", ">", h4(predictword[i], align = "left"), "</span>")
                        }
                }
                
                str1 <- h4(str1, align = "left")
                str3 <- h4('Entered text:', align = "left")
                str4 <- paste("<span style='color:blue'>", h4(input$text1, align = "left"), "</span>")
                if (input$displayEnterText==FALSE) {
                        str4 <- ""
                }
                HTML(paste(str1, str2, "</br>", str3, str4))
        })
} # end of function(input, output)