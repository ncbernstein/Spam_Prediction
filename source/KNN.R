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

k.neighbor <- function(ind, test, train, isSpam, k)
  {
  vec <- unlist(test[ind, ])
  for(i in 1:ncol(train))
    {
    train[, i] <- (train[, i] - vec[i])^2
    }
  dis <- sqrt(rowSums(train))
  ord <- order(dis)
  return(round(sum(isSpam[ord[1:k]])/k))
  }




