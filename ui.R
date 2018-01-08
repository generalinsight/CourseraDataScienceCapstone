
library(shiny)
library(markdown)

# Define UI for application
shinyUI <- fluidPage(
        navbarPage("wordpredict app",
                # multi-page user-interface that includes a navigation bar.
                tabPanel("Application",
                        sidebarLayout(
                                sidebarPanel(
                                      textInput(inputId = "text1", label = "Please enter your text here: ", value =""),
                                      selectInput(inputId = "numPredictedWord", label = "Number of predicted word to be displayed: ", selected= 1, choices = c(1,2,3,4)),
                                      checkboxInput(inputId="displayEnterText", "Display entered text", value = TRUE),
                                      h5("This is a swiftkey like next word prediction model using web data aggregated from various sources.")
                                ), # end of "sidebarPanel"
                                mainPanel(
                                        htmlOutput("html1")
                                ) # mainPanel
                        ) #sidebarLayout
                ) # end of "Application" tabPanel       
                
) # end of navbarPage
)