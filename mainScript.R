# Main script

# load functions to read raw emails
source('source/readEmails.R')

# if spamdata is zipped uncomment
# unzip('spamdata.zip')

# list emails from directory where raw emails are located
# and parse with readMessage (from readEmails.R)
spamDir <- "spamdata"
paths <- list.files(spamDir, recursive = TRUE)
emails <- lapply(paths, readMessage, spamDir)

# save R object if you want
save(emails, file = "emails.rda")
load('emails.rda')

# remove the one bad email
# this one is returned as "NULL" from our function
emails <- emails[-4865]

# this gives a T/F vector indicating spam/no spam
isSpam <- do.call(c, lapply(emails, function(x) 
  grepl("spam",strsplit(x$path, "/")[[1]][1])))

# small script to list functions in 'spam_fns.R'
# and apply them to get our variables or 'features'
# from the emails 
x <- new.env()
source('source/spam_fns.R', local=x)
fns <- ls(envir = x)
source('source/spam_fns.R')
apply.fns <- function(fn, emails)
  {
  f <- get(fn)
  do.call(c, lapply(emails, f))
  }

# data frame with results
results <- as.data.frame(do.call(cbind, lapply(fns, apply.fns, emails)))
colnames(results) <- fns

# partition data
part <- sample(1:nrow(results), round(nrow(results)*.1))

train <- results[-part,]
test <- results[part,]

isSpam.train <- isSpam[-part]
isSpam.test <- isSpam[part]


test <- impute.na(test)
train <- impute.na(train)

source('source/KNN.R')

ntest <- nrow(test)
spam.preds <- do.call(c, lapply(1:ntest, k.neighbor, test=test, train=train, isSpam=isSpam.train, k=10))


