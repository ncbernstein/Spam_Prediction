# Spam_Prediction
Project for predicting emails as spam using KNN model using R.

## Overview

I am fixing up an R script I wrote in college that parses emails into an R data frame and then predicts them as spam using a KNN neighbors model. The original project was a success -all the code runs- but the code was not organized well (clumsily put together between a few separate  . My goal is to make the code so that all you have to do is download a the directory an then run the main script. 

## Parts

* mainScript.R: This runs all the the other R files.
* spamdata.zip: This file contains 6541 email files across 5 directories labeled as spam or ham.
* readEmails.R: Parses emails. Creates an R list of 3 elements: header (named character vector), body (character vector), attachments (list containing attachments and their corresponding types)
* spam_fns.R: This file has functions that takes a parsed email (R list) and builds variables to be used in KNN model.
* KNN.R: Aftter a data frame has been assembled, this will compute distance matrix between each test case and all train cases. Then it chooses the test case as spam or ham based on the values of the three nearest neighbors
