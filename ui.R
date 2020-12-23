suppressWarnings(library(shiny))
suppressWarnings(library(markdown))
shinyUI(navbarPage("Data Science Capstone",
                   tabPanel("Predict the Next Word",
                            sidebarLayout(
                              sidebarPanel(
                                textInput("inputString", "Enter a parcial sentence ",value = ""),
                                br(),
                                br(),
                                br(),
                                br()
                              ),
                              mainPanel(
                                h2("Predicted Word"),
                                verbatimTextOutput("prediction"),
                                textOutput('text1'),
                                br()
                              )
                            )
                            
                   )
                   )
)
