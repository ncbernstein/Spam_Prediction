# Main script
source('readEmails.R')

spamDir <- "spamdata"
paths <- list.files(spamDir, recursive = TRUE)
emails <- lapply(paths, readMessage, spamDir)
names(emails) <- paths
save(emails, file = "emails.rda")

source("spam_fns.R")

#### NEED TO COMPLETE: variable creation ( generate nice dataframe)
#### re-implement KNN modularly