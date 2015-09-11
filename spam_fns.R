load('emails.rda')
email <- emails[[10]]

is.spam <- function(email)
  {
  path <- email$path
  folder <- strsplit(paths[[1]], '/')[[1]][1]
  if(grepl('spam', folder))
    return(TRUE)
  else
    return(FALSE)
  }

is.Re <- function(email)
  {
  head <- email$head
  if("subject" %in% names(head))
    isRe <- grepl('[Rr][Ee]:', head[which(names(head) == 'subject')])
  else
    isRe <- FALSE
  return(isRe)
  }


### 3 Body lines
numLinesBody <- function(email)
  {
  length(email$body)
  }

### 4 Body chars
bodyCharCount <- function(email)
  {
  do.call(sum, lapply(email$body, nchar, type = "bytes"))
  }


### 6 !!!!
subjectExclamationCount <- function(email)
  {
  head <- email$head
  if("subject" %in% names(head))
    {
    len <- length(head["subject"])
    len2 <- length(gsub("!", "", head["subject"]))
    return(len - len2)
    }
  else return(0)                                            
  }

gsub("\\?", "","a!!  !??!subject")

### 7 ?????
subjectQuestCount <- function(email)
  {
  head <- email$head
  if("subject" %in% names(head))
    {
    len <- length(head["subject"])
    len2 <- length(gsub("\\?", "", head["subject"]))
    return(len - len2)
    }
  else return(0)
  }


### 8 # attachments
numAttachments <- function(email)
  {
  length(email$atts)
  }


#########################################
### 9 # recip
numRecip <- function(email)
  {
  head <- email$head
  sum(grepl("^(cc|to)(|[1-9][0-9]*)$", names(head)))
  }

### 10 %caps


percentCap <- function(email)
  {
  do.call(sum, lapply(email$body, nchar, type = "bytes"))
  
  }


### 11 punct !?$#%&@* -exclude ' since contractions are OK i.e. won't
subjectPunctuationCheck <- function(x)
{
  if(length(x$header) >= 1 && "subject" %in% tolower(names(x$header))){
    names(x$header) <- tolower(names(x$header))
    if(sum(grepl("[[:alpha:]]{1}[[:digit:]\\!?$#%&@*]{1}[[:alpha:]]{1}", x$header["subject"]))>0)
      return(TRUE)
    else return(FALSE)
  }
  else return(FALSE)
}

### 12 keywords - spam
subjectSpamWords <- function(x)
{ 
  keys<-c("[[:blank:]]viagra", "pounds", "free", "weight", "guarantee", "millions", "dollars", 
          "credit", "risk", "prescription", "generic", "drug", "money[[:blank:]]back",
          "credit[[:blank:]]card[[:blank:]]")
  keys<-paste(keys, collapse = "[[:blank:]]|[[:blank:]]")
  
  if(length(x$header) >= 1 && "subject" %in% tolower(names(x$header))){
    names(x$header) <- tolower(names(x$header))
    if(sum(grepl(keys, x$header["subject"]))>0)
      return(TRUE)
    else return(FALSE)
  }
  else return(FALSE)
}


### 13 % blank
percentSubjectBlanks <-function(x)
{
  if(length(x$header) >= 1 && "subject" %in% tolower(names(x$header))){
    names(x$header) <- tolower(names(x$header))
    chars <- str_length(x$header["subject"])
    blanks <- chars - str_length(gsub("[[:space:]]", "", x$header["subject"]))
    if(chars==0)
      return(0)
    else return(blanks/chars)
  }
  else return(0)
}


### 14 content type
contentType <-function(x)
{
  if(length(x$header) >= 1 && "content-type" %in% tolower(names(x$header))){
    names(x$header) <- tolower(names(x$header))
    x<-x$header["content-type"]
    x<-gsub("[[:space:]]", "", x)
    return(tolower(unlist(strsplit(x, split= ";"))[1]))
  }
  else return("NONE")
}


### 15 YELLING IN SUBJECT
isYelling <-function(x)
{
  if(length(x$header) >= 1 && "subject" %in% tolower(names(x$header))){
    names(x$header) <- tolower(names(x$header))
    x<-x$header["subject"]
    if(sum(grepl("[[:alpha:]]", x))==0)
      return(FALSE)
    else return(x == toupper(x))
  }
  else return(FALSE) 
}


### 16 ORIGINAL MESSAGE
isOriginalMessage <- function(x)
{
  if(sum(grepl("original message", tolower(x$body)))>=1)
    return(TRUE)
  else return(FALSE)
}

### 17 isInReplyTo
isInReplyTo <- function(x)
{
  if(length(x$header) >= 1 && "in-reply-to" %in% tolower(names(x$header))) return(TRUE)
  else return(FALSE)
}

### 18 numDollarSigns
numDollarSigns <- function(x)
{
  x<-x$body
  return(sum(str_length(x))-sum(str_length(gsub("\\$", "", x ))))
}

### 19 isPGPsigned
isPGPsigned <- function(x)
{
  test <- function(x){
    if(length(x$header) >= 1 && "content-type" %in% tolower(names(x$header))){
      names(x$header) <- tolower(names(x$header))
      y<-grepl("(pgp|gpg)-signature", tolower(x$header["content-type"]))
      return(sum(y)>0)
    }
    else
      return(FALSE)
  }
  
  x <- x$attachments
  if(length(x)==0)
    return(FALSE)
  else
    test <- sum(sapply(x, test))
  return(test>0)
}

### 20 hoursent
hourSent <- function(x)
{
  if(length(x$header) >= 1 && "date" %in% tolower(names(x$header))){
    names(x$header) <- tolower(names(x$header))
    x <- unlist(strsplit(x$header["date"], split = " "))
    hour <- unlist(strsplit(x[grep(":", x)[1]], split = ":"))[1]
    return(as.integer(hour))
  }
  else return("")
}