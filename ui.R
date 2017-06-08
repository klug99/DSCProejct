
suppressWarnings(library(shiny))
suppressWarnings(library(markdown))
shinyUI(navbarPage("Coursera Data Science Capstone: Course Project",
                   tabPanel("Predict the Next Word",
                              # Sidebar
                              sidebarLayout(
                              sidebarPanel(
                                helpText("Enter a sentence to begin the next word prediction"),
                                textInput("inputString", "Enter a partial sentence here",value = ""),
                                br(),
                                br(),
                                br(),
                                br()
                                ),
                              mainPanel(
                                  h2("Predicted Next Word"),
                                  verbatimTextOutput("prediction"),
                                  strong("Sentence Input:"),
                                  textOutput('text1'),
                                  br(),
                                  strong("Note:"),
                                  textOutput('text2')
                              )
                              )
                             
                  ),## Descriptionn for the project
                   tabPanel("Description",
                            mainPanel(
                            includeMarkdown("Description.html")
                            )
                   )
)
)