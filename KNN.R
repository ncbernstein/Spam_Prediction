#################
### K-NN Code ###
#################

# IMPUTE NA VALUES
impute.na <- function(data)
  {
  # just impute the mean for now
  for(i in 1:ncol(data)-1){
    if(sum(is.na(data[, i])) > 0)
      data[which(is.na(data[, i])), i] <- mean(data[, i], na.rm=TRUE)
    }
  return(data)
  }

# DETERMINE CLASS
class.test <- function(ordered, isSpam, k, ntest)
  {
  # remove ids corresponding to other test data points
  ordered <- ordered[!ordered %in% 1:ntest]
  # take first k neighbors
  ordered <- ordered[1:k]
  # get spam labels for k neighbors
  isSpam.k <- isSpam[ordered]
  # spam if proportion spam neighbors > 50%
  p<-mean(isSpam.k)
  if (p>.5)
    return(TRUE)
  else return(FALSE)
  }

# main function
my.knn <- function(test, train, k)
  {
### GET DATA READY ###
  # impute missings 
  test <- impute.na(test)
  train <- impute.na(train)
  # separate spam vector
  isSpam <- c(test$isSpam, train$isSpam)
  
  ntest <- nrow(test)
  # merge test, train into single matrix for distance computations
  x <- rbind(test, train)
  x$isSpam <- NULL
  # scale x  
  x <- data.frame(scale(x))
  
### DISTANCE MATRIX ###
  # get the distance matrix
  d <- dist(x, upper=T)
  d <- as.matrix(d)
  # only interested in test distances 
  d <- d[, 1:ntest]
  # order distance matrix - ranks neighbors closest to farthest
  ordered <- apply(d, 2, order) 
  # make it a list
  i<- rep(1:ntest)
  ordered <- lapply(i, function(i,x)x[,i], x=ordered)

### CLASSIFICATION
  ps <- lapply(ordered, class.test, isSpam=isSpam, k=k, ntest=ntest)
  return(ps)
  }

# Merge predicted and actual (for CV results) 
results.cv <- function(i, df, predicted)
  {
  predicted <- unlist(predicted[i])
  actual <- df[which(df$group==i),"isSpam"]
  results <- data.frame(pred = predicted, isSpam = actual)
  results$resid <- abs(results$pred-results$isSpam)
  return(results)
  }

# error rates
err.rate <- function(i, results)
  {
  results <- results[[i]]
  return(t(data.frame(tapply(results$resid, results$isSpam, mean))))
  }

# confusion Matrix
confusion <- function(i, results)
  {
  results <- results[[i]]
  return(xtabs( ~ results$pred+results$isSpam))
  }

##############
### CV run ###
##############

load("E:/R/trainVariables.rda")
# shuffle dataframe
df <- trainVariables[sample.int(nrow(trainVariables)),]
# cross fold it
nfolds <- 10
groups <- rep(1:nfolds, length=nrow(df))
df$group <- groups
i<-(1:nfolds)

# run cv run
predict <- lapply(i, function(i, df)my.knn(test = df[which(df$group==i), 1:30], train=df[which(df$group!=i), 1:30], k = 5), df=df)

# DF of test pred vs actual
results <- lapply(i, results.cv, df=df, predicted=predict)

#error rates 
rates <- lapply(i, err.rate, results = results)
rates <- unlist(rates)
rates <- matrix(rates, ncol = 2, nrow = nfolds, byrow=T)
rates <- data.frame(rates)
colnames(rates) <- c("Type_II", "Type_I")
pander(rates)
#confusion matrix
cf <- lapply(i, confusion, results=results)
pander(print(cf))

#######################################

#########################
### train - test run  ###
#########################

load("E:/R/trainVariables.rda") 
load("E:/R/testData.rda")

te <- testVariables
tr <- trainVariables

test <- my.knn(test = te, train=tr, k = 5)
res<-data.frame(pred=unlist(test))
res$isSpam <- te$isSpam
res$resid<-abs(res$pred-res$isSpam)
x<-t(data.frame(tapply(res$resid, res$isSpam, mean)))

####################
# Some diagnostics #
####################
levels.vars<-lapply(trainVariables, function(x)length(levels(factor(x))))
na.count<-lapply(trainVariables, function(x)sum(is.na(x)))
na.count<-lapply(testVariables, function(x)sum(is.na(x)))
logicals<-lapply(trainVariables, function(x)is.logical(x))
# partition to train and test
partition.data <- function(data, percent)
{
  ids<-sample(1:nrow(data), size=percent*nrow(data), replace=FALSE)
  test <- vector(mode = "logical", length = nrow(data))
  test[ids] <- TRUE
  return(test)
}

####################################################################

################################
### CLASSIFICATION TREE CODE ###
################################
library(rpart)

########################
### TREES train-test ###
########################


load("E:/R/trainVariables.rda") 
load("E:/R/testData.rda")

test <- testVariables
train <- trainVariables

# build tree on train
tree <- rpart(isSpam ~ ., data=train)
tree
plot(tree)
text(tree, use.n = TRUE, cex = .6)

# predict test values
preds <- predict(tree, test)

# put in dataframe
results <- data.frame(isSpam<-test$isSpam)
results$pred <- FALSE
results[which(preds > .5), 2] <- TRUE
sum(results$pred)
results$resid <- abs(results$pred-results$isSpam)
tapply(results$resid, results$isSpam, mean)

##########################################################

##############
### CV run ###
##############

### build model and evaluate
predict.cv <- function(i, df)
{
  test <- df[which(df$group==i), 1:30]
  train <- df[which(df$group!=i), 1:30]
  tree <- rpart(isSpam ~ ., data=train)
  preds <- predict(tree, test)
  results <- data.frame(isSpam<-test$isSpam)
  results$pred <- FALSE
  results[which(preds > .5), 2] <- TRUE
  results$resid <- abs(results$pred-results$isSpam)
  return(results)
}

# error rates
err.rate <- function(i, results)
{
  results <- results[[i]]
  return(t(data.frame(tapply(results$resid, results$isSpam, mean))))
}

# confusion Matrix
confusion <- function(i, results)
{
  results <- results[[i]]
  return(xtabs( ~ results$pred+results$isSpam))
}

load("E:/R/trainVariables.rda")
# shuffle dataframe
df <- trainVariables[sample.int(nrow(trainVariables)),]
# cross fold it
nfolds <- 10
groups <- rep(1:nfolds, length=nrow(df))
df$group <- groups
i<-(1:nfolds)

# run cv run
results <- lapply(i, predict.cv, df=df)

#error rates 
rates <- lapply(i, err.rate, results = results)
rates <- unlist(rates)
rates <- matrix(rates, ncol = 2, nrow = nfolds, byrow=T)
rates <- data.frame(rates)
colnames(rates) <- c("Type_II", "Type_I")

#confusion matrix
cf <- lapply(i, confusion, results=results)
cf


