suppressWarnings(library(tm))
suppressWarnings(library(stringr))
suppressWarnings(library(shiny))
quadgram <- readRDS("quadgram.RData");
trigram <- readRDS("trigram.RData");
bigram <- readRDS("bigram.RData");

Prediction <- function(x) {
    nonum <- removeNumbers(removePunctuation(tolower(x)))
    final <- strsplit(nonum, " ")[[1]]
    
    if (length(final)>= 3) {
        final <- tail(final,3)
        if (identical(character(0),head(quadgram[quadgram$unigram == final[1] & quadgram$bigram == final[2] & quadgram$trigram == final[3], 4],1))){
            Prediction(paste(final[2],final[3],sep=" "))
        }
        else {head(quadgram[quadgram$unigram == final[1] & quadgram$bigram == final[2] & quadgram$trigram == final[3], 4],1)}
    }
    else if (length(final) == 2){
        final <- tail(final,2)
        if (identical(character(0),head(trigram[trigram$unigram == final[1] & trigram$bigram == final[2], 3],1))) {
            Prediction(final[2])
        }
        else {head(trigram[trigram$unigram == final[1] & trigram$bigram == final[2], 3],1)}
    }
    else if (length(final) == 1){
        final <- tail(final,1)
        if (identical(character(0),head(bigram[bigram$unigram == final[1], 2],1))) {head("the",1)}
        else {head(bigram[bigram$unigram == final[1],2],1)}
    }
}


shinyServer(function(input, output) {
    output$prediction <- renderPrint({
        result <- Prediction(input$inputString)
        result
    })
    
}
)