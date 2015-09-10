# FUNCTION: read the message
readMessage <- function(path, spamDir)
  {

  # Read the email
  con <- file(paste(spamDir, "/",  path, sep = ""), open = 'rt')
  email <- readLines(con)
  close(con)
  
  if(grepl("^mv ", email[1]))
    return(NULL)
  
  # Separate the header
  sep.email <- split.header(email)    
  return(list(path = path,
              head = sep.email$header,
              body = sep.email$body))
  }


# FUNCTION: Separate header and body
# Header contains 'KEY: VALUE' pairs on each line and
# ends at first blank line of email

split.header <- function(email)
  {
  require(stringr)
  start <- grep(": ", email)[1]
  
  if(is.na(start))
    return(list(header = "", 
                body = ""))
  
  # Split at first blank line
  split <- which(email[start:length(email)] == "")[1]  
  con <- textConnection(email[start:(split - 1)])
  
  # Read header as DCF (format is KEY: VALUE)
  head <-read.dcf(con, all = TRUE)
  head <- unlist(head)
  names(head) <- tolower(names(head))
  close(con)
  
  # Return list with header and body
  return(list(header = head, 
              body = email[-(1:split)]))
  }



spamDir <- "spamdata"
paths <- list.files(spamDir, recursive = TRUE)
emails <- lapply(paths[1:1000], readMessage, spamDir)



