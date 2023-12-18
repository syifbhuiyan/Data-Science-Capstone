# Load required libraries
library(shiny)
library(tm)

# Function to custom clean entered text phrase
custom_input_text_clean <- function(testline) {
  line <- iconv(testline, "latin1", "ASCII", sub = "")
  line <- gsub('[0-9]+', '', line)
  line <- tolower(line)
  line <- removeWords(line, stopwords())
  line <- removePunctuation(line)
  line <- gsub('\\S+[^\x20-\x7E]', '', line)
  emptyLines <- grepl('^\\s*$', line)
  line <- line[!emptyLines]
  line <- stripWhitespace(line)
  return(line)
}

# Function to predict next word based on cleaned text phrase and model
predict_Backoff_custom_cleaning <- function(testline, modelsList, isDebugMode = FALSE) {
  maxNGramIndex <- length(modelsList)
  line <- custom_input_text_clean(testline)
  words <- unlist(strsplit(line, split = " "))
  len <- length(words)
  
  if (len < maxNGramIndex) {
    nGramIndex <- len + 1
    localModelsList <- modelsList[(maxNGramIndex - len):maxNGramIndex]
  } else {
    nGramIndex <- maxNGramIndex
    localModelsList <- modelsList
  }
  
  for (model in localModelsList) {
    pattern <- paste0("^", paste(words[(len - nGramIndex + 2):len], collapse = " "))
    nextWords <- model[grep(pattern, model$word)[1:5], 1]
    nextWords <- nextWords[!is.na(nextWords)]
    
    if (length(nextWords) != 0) {
      nextWordIndex <- sample(1:length(nextWords), 1)
      nextWord <- nextWords[nextWordIndex]
    } else {
      nextWord <- NA
    }
    
    nGramIndex <- nGramIndex - 1
    
    if (!is.na(nextWord)) {
      tempNextWord <- unlist(strsplit(as.character(nextWord), " "))
      nextWord <- tempNextWord[length(tempNextWord)]
      break
    }
  }
  
  if (is.na(nextWord)) {
    nextWord <- modelsList[[maxNGramIndex]][1, 1]
  }
  
  return(nextWord)
}

# Define server logic for the application that realizes single word prediction
shinyServer(function(input, output, session) {
  
  # Loading required n-gram models

  # "Clean & Predict" and "Clear" action buttons
  v <- reactiveValues(data = NULL)
  
  # "Clean & Predict" action button
  observeEvent(input$action, {
    v$data <- runif(100)
  })
  
  # Clear entered text phrase and "Clear" action button
  observeEvent(input$reset, {
    v$data <- NULL
    updateTextAreaInput(session, "textInput", label = "Enter text phrase (without last word):", value = "")
  })
  
  # Clean entered text phrase and clear cleaned text phrase
  output$cleanedText <- renderText({
    if (is.null(v$data)) return()
    custom_input_text_clean(input$textInput)
  })
  
  # Predict next word based on "News articles" text file or based on "Twitter data" text file
  output$predictedWord <- renderText({
    testString <- custom_input_text_clean(input$textInput)
    choices <- list("News articles data" = 1, "Twitter data" = 2)
    
    if (input$model == choices[["News articles data"]]) {
      if (is.null(v$data)) return()
      predict_Backoff_custom_cleaning(testString, nGramModelsListNews, FALSE)
    } else {
      if (is.null(v$data)) return()
      predict_Backoff_custom_cleaning(testString, nGramModelsListTwitter, FALSE)
    }
  })
})