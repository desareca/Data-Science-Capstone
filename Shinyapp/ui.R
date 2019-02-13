# load Libraries
suppressMessages(library(shiny))
suppressMessages(library(tm))
suppressMessages(library(dplyr))
suppressMessages(library(shinythemes))
# load parameter lambda
lambda <- readRDS("Lambdas_ngramas.rds")
# Define interface required to predict next word
shinyUI(navbarPage("Data Science Capstone - Predict word", 
                   theme = shinytheme("superhero"),
                   tabPanel("Next Word Prediction",
                            shinyUI(fluidPage(
                                  theme = shinytheme("superhero"),
                                  sidebarLayout(
                                        sidebarPanel(
                                              HTML("<span style='color:white'>"),
                                              h4("Number words to predict:"),
                                              HTML("</span>"),
                                              hr(),
                                              sliderInput("slider", label="Predict", 1, 10, 5, step = NULL,
                                                          round = FALSE, format = NULL, locale = NULL, ticks = TRUE,
                                                          animate = FALSE, width = NULL, sep = ",", pre = NULL,
                                                          post = " Word", timeFormat = NULL, timezone = NULL,
                                                          dragRange = TRUE),
                                              hr(),
                                              HTML("<span style='color:white'>"),
                                              h4("Instructions:"),
                                              HTML("</span>"),
                                              HTML("<span style='color:lavender'>"),
                                              h5("Enter a word or phrase in the box above.
                                                 Below this table, a table will be 
                                                 generated with the most likely 
                                                 words to be the next one.",
                                                 align = "justify"),
                                              HTML("</span>"),
                                              br(),
                                              HTML("<span style='color:lavender'>"),
                                              h5("To increase or decrease
                                                 the number of words to predict must
                                                 be assigned in the slider, by default
                                                 it shows 5 in a range of 1 to 10 words.",
                                                 align = "justify"),
                                              HTML("</span>")
                                              ),
                                        mainPanel(
                                              textAreaInput("text", "", value = "", width = "600px",
                                                            height = NULL, cols = NULL, rows = NULL,
                                                            placeholder = "Please enter text here...",
                                                            resize = "both"),
                                              HTML("<span style='color:lavender'>"),
                                              h6("*Only english words are supported."),
                                              HTML("</span>"),
                                              hr(),
                                              HTML("<span style='color:orangered'>"),
                                              dataTableOutput("Pred"),
                                              HTML("</span>")
                                        )
                                        )
                                  )
                                  )
                            ),
                   tabPanel("Probabilities",
                            shinyUI(fluidPage(
                                  sidebarLayout(
                                        sidebarPanel(h4("Parameters"),
                                                     hr(),
                                              sliderInput("L1", label="Lambda 1",
                                                          0, 1, lambda[1,1]/sum(lambda)),
                                              sliderInput("L2", label="Lambda 2",
                                                          0, 1, lambda[1,2]/sum(lambda)),
                                              sliderInput("L3", label="Lambda 3",
                                                          0, 1, lambda[1,3]/sum(lambda)),
                                              sliderInput("L4", label="Lambda 4",
                                                          0, 1, lambda[1,4]/sum(lambda))
                                        ),
                                        mainPanel(h4("Table of probabilities"),
                                                  hr(),
                                                  HTML("<span style='color:lavender'>"),
                                                  h6(dataTableOutput("table")),
                                                  HTML("</span>")
                                        )
                                  )
                            )
                            )
                   ),
                   tabPanel("About This Application",
                            fluidPage(
                                  HTML("<span style='color:white'>"),
                                  h2("Introduction"),
                                  HTML("<span>"),
                                  
                                  HTML("<span style='color:lightsteelblue'>"),
                                  h5("This application is the final project for the 
                                     specialization of Coursera Data Science, 
                                     carried out by the Johns Hopkins University 
                                     and in cooperation with SwiftKey.",
                                     align = "justify"),
                                  h5("The main objective of this final project is 
                                     to build a brilliant application that is able 
                                     to predict the next word. The text data that
                                     is used to create a frequency dictionary and,
                                     therefore, to predict the following words come
                                     from a corpus called HC Corpora that can be 
                                     found in the following link.",
                                     align = "justify"),
                                  HTML("<span>"),
                                  a("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"),
                                  
                                  HTML("<span style='color:white'>"),
                                  h2("Methods"),
                                  HTML("<span>"),
                                  
                                  HTML("<span style='color:lightsteelblue'>"),
                                  h5("To begin with, the corpus was divided into 
                                     training and test sets based on data from 
                                     HC Corpora. From the training set samples 
                                     were created, where it was cleaned by lowercase
                                     letters, eliminating punctuation marks, links,
                                     blank spaces, numbers and all kinds of 
                                     special characters. These data samples
                                     then became n-grams. For this application,
                                     grams of 1, 2, 3 and 4 words are considered.",
                                     align = "justify"),
                                  h5("Once the n-grams were created, it was decided
                                     to use an interpolation algorithm, considering
                                     the adjustment of its parameters.",
                                     align = "justify"),
                                  HTML("<span>"),
                                  
                                  HTML("<span style='color:white'>"),
                                  h2("How to use this apliccation"),
                                  HTML("<span>"),
                                  
                                  HTML("<span style='color:lavender'>"),
                                  h4("Next Word Prediction tab"),
                                  HTML("<span>"),
                                  HTML("<span style='color:lightsteelblue'>"),
                                  h5("Enter a word or phrase in the upper box. 
                                     Under this box a table will be generated with
                                     the most likely words to be the following ones.",
                                     align = "justify"),
                                  h5("By default 5 words are shown, with the slider 
                                     you can modify the length of the table 
                                     between 1 to 10 words.",
                                     align = "justify"),
                                  
                                  HTML("<span style='color:lavender'>"),
                                  h4("Probabilities tab"),
                                  HTML("<span>"),
                                  HTML("<span style='color:lightsteelblue'>"),
                                  h5("In this tab you can review the words most 
                                     likely to be the following, the maximum words
                                     to display is 100.",
                                     align = "justify"),
                                  h5("In addition, you can modify the parameters 
                                     to check how the probabilities and variables
                                     vary. The initial values correspond to the
                                     parameters adjusted during the design.",
                                     align = "justify"),
                                  HTML("<span>"),
                                  
                                  HTML("<span style='color:white'>"),
                                  h2("Application information"),
                                  HTML("<span>"),
                                  HTML("<span style='color:lightsteelblue'>"),
                                  h5("The code of this application, as well as the
                                     reports of milestones, related scripts, 
                                     presentation, design, etc., can be found 
                                     in the GitHub repository:",
                                     align = "justify"),
                                  a("https://github.com/desareca/Data-Science-Capstone"),
                                  h5("The milestone report, presentation and design 
                                     report can be found at:",
                                     align = "justify"),
                                  a("https://rpubs.com/desareca"),
                                  h5("Information on Coursera's data science 
                                     specialization:",
                                     align = "justify"),
                                  a("https://www.coursera.org/specializations/jhu-data-science"),
                                  HTML("<span>"),
                                  
                                  
                                  HTML("<span style='color:white'>"),
                                  h2("References"),
                                  HTML("<span>"),
                                  HTML("<span style='color:lightsteelblue'>"),
                                  h5("For the development of this application, 
                                     the following sources of information were 
                                     used, both to create the ngrams and for 
                                     the interpolation algorithm:",
                                     align = "justify"),
                                  a("https://en.wikipedia.org/wiki/N-gram"),br(" "),
                                  a("https://core.ac.uk/download/pdf/61910629.pdf"),br(" "),
                                  a("http://www.cs.cornell.edu/courses/cs4740/2014sp/lectures/smoothing+backoff.pdf"),
                                  HTML("<span>"),
                                  
                                  br(" "),
                                  br(" ")
                            )
                   )
                   )
       
        )