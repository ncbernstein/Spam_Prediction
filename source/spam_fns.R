is.re <- function(email)
  {
  head <- email$head
  if("subject" %in% names(head))
    isRe <- grepl('[Rr][Ee]:', head[which(names(head) == 'subject')])
  else
    isRe <- FALSE
  return(isRe)
  }


lines.count <- function(email)
  {
  length(email$body)
  }

char.count <- function(email)
  {
  do.call(sum, lapply(email$body, nchar, type = "bytes"))
  }


subj.exc.count <- function(email)
  {
  head <- email$head

    len <- nchar(head["subject"])
    len2 <- nchar(gsub("!", "", head["subject"]))
    return(len - len2)
  
  }

subj.ques.count <- function(email)
  {
  head <- email$head
  
    len <- nchar(head["subject"])
    len2 <- nchar(gsub("\\?", "", head["subject"]))
    return(len - len2)
  }

num.atts <- function(email)
  {
  length(email$atts)
  }



num.recip <- function(email)
  {
  head <- email$head
  sum(grepl("^(cc|to)(|[1-9][0-9]*)$", names(head)))
  }

percent.caps.body <- function(email)
  {
  allChar <- do.call(sum, lapply(email$body, nchar, type = "bytes"))
  noCaps <- do.call(sum, lapply(email$body, function(x) nchar(gsub("[A-Z]", "", x), type = "bytes")))
  x <- (allChar - noCaps)/noCaps
  if(is.na(x))
    return(0)
  else
    return(x)
  }

subj.percent.caps <- function(email)
  {
  head <- email$head
  allChar <- do.call(sum, lapply(head['subject'], nchar, type = "bytes"))
  noCaps <- do.call(sum, lapply(head['subject'], function(x) nchar(gsub("[A-Z]", "", x), type = "bytes")))
  x <- (allChar - noCaps)/noCaps
  if(is.na(x))
    return(0)
  else
    return(x)
  }
