#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that realizes single word prediction
shinyUI(fluidPage(

    # Application title
    titlePanel("SINGLE WORD PREDICTION - Lite Version"),

    # Application subtitle
    fluidRow(
        column(12,
               h4("based on n-gram model for News articles or Twitter text data")
        )
    ),
    
    tags$hr(style="border-color: black;"),
    
    # Sidebar panel with: select box to select n-gram model and with help text ----
    sidebarPanel(
        
        
        # Select box with a select input for each n-gram model
        selectInput("model", label = "Select 4-gram model for:", 
                    choices = list("News articles data" = 1, "Twitter data" = 2), 
                    selected = 1),
        
        # Include clarifying text ----
        helpText("Select 4-gram model calculated based on content of text file in English language. 
             First text file contains News articles data, second text file contains Twitter data."),
        
        tags$hr(style="border-color: black;"),
        
        tags$b("About:"),
        helpText("Both text files come from Coursera-SwiftKey dataset 
             (version downloaded 18th Dec 2023)."),
        helpText("Application contains 4-gram model for each among two text data files. 
             Each n-grams set is without n-grams occured with frequency equals one."),
        helpText("This is lite version of application 
             just to demonstrate ability to predict next word, with good accuracy, 
             based on entered text phrase in English language."),
        tags$a(href="https://www.linkedin.com/in/syif-m-bhuiyan-a7250a119/", "Author"),
        
        tags$hr(style="border-color: black;"),
        
        tags$b("Usage example:"),
        helpText("Entered text phrase:"), tags$i("I'm open to that and a lot of"),
        helpText("Cleaned text phrase:"), tags$i("open lot"),
        helpText("Predicted next word - News articles:"), tags$i("things"),
        helpText("Predicted next word - Twitter data:"), tags$i("doors")
    ),
    
    
    # Main panel with: input for enter text phrase, submit button, 
    # output for cleaned text phrase and output for single word prediction
    mainPanel(
        
        
        # Input box to enter text phrase without last word
        br(),  
        textAreaInput("textInput", label = "Enter text phrase without last word:", value = "", width = "100%", rows = 6),
        
        # Include clarifying text ----
        helpText("Mentioned last word will be predicted and displayed in last output text box."),
        
        helpText("This input box can be resized by move bottom right corner."),
        
        hr(),
        br(),
        
        # Action button to submit entered text phrase
        actionButton("action", label = tags$b("Clean & Predict"), width = "25%"),
        
        # Action button to clear entered text phrase
        actionButton("reset", label = tags$b("Clear"), width = "25%"),
        
        # Include clarifying text ----
        helpText("Press left button to clean entered text phrase and predict next word."),
        helpText("Edit entered text phrase to see new cleaned text phrase and new predicted next word or"),
        helpText("press right button to clear entered and cleaned text phrase and to clear predicted next word."),
        
        hr(),
        br(),
        
        tags$b("Cleaned text phrase:"),
        
        # Output box to display cleaned text phrase
        verbatimTextOutput("cleanedText", placeholder = TRUE),
        
        br(),
        
        tags$b("Predicted next word:"),
        
        # Output box to display predicted word
        fluidRow(column(4, verbatimTextOutput("predictedWord", placeholder = TRUE)))
        
    )
    
))