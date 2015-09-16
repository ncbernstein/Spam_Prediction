# Spam_Prediction
Project for predicting emails as spam using a K-Nearest Neighbors model. This is all written with R.

## Overview

I am fixing up an R script I wrote in college that parses emails into an R data frame, and then predicts them as spam using a K-Nearest Neighbors model. The original project was a success - all the code runs - but the code was not organized well. My goal is to fix the code so that all you have to do is download a the directory an then run the main script. 

## List of files
* mainScript.R: This runs R files in source directory. Currently generates predicted values for test partition.
* source
 * readEmails.R: Parses emails. Creates an R list of 3 elements: header (named character vector), body (character vector), attachments (list containing attachments and their corresponding types)
 * spam_fns.R: This file has functions that takes a parsed email (R list) and builds variables to be used in KNN model.
 * KNN.R: Aftter a data frame has been assembled, this will compute distance matrix between each test case and all train cases. Then it chooses the test case as spam or ham based on the values of the three nearest neighbors
* spamdata.zip: This file contains 6541 email files across 5 directories labeled as spam or ham.
* emails.rda: output of readEmails.R, see main script

## Work left to do
* spam_fns.R: 
 * Need more variables for our prediction so I will add more

* mainScript.R
 * Cross Validation
 * Report performance of model
