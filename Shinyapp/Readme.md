# Data-Science-Capstone

Introduction
========================================================

This application is the final project for the specialization of Coursera Data Science, 
carried out by the Johns Hopkins University and in cooperation with SwiftKey.

The main objective of this final project is to build a brilliant application that is able 
to predict the next word. The text data that is used to create a frequency dictionary and,
therefore, to predict the following words come from a corpus called HC Corpora that can be 
found in the following link.

https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip

Methods
========================================================

In the design the corpus was divided into training and test sets. From the training set, samples were created, where cleanings and text transformations were carried out. With this, n-grams of 1, 2, 3 and 4 words were created. Once the n-grams were created, it was decided to use an interpolation algorithm, considering the adjustment of its parameters

To review the details of the complete design see:

https://rpubs.com/desareca/Milestone-Report <br/>
https://rpubs.com/desareca/Pred-Next-Word

Application
========================================================

To use the application enter your phrase in the entry field. In a table below will be shown the words most likely to be the next. There is the option to display from 1 to 10 most likely word by adjusting the slider.

In the adjacent tab, the probabilities of a larger set (up to 100 words) and the adjusted parameters are shown, with the option of modifying them and visualizing how the probabilities are modified.

https://desareca.shinyapps.io/Predict-word/
