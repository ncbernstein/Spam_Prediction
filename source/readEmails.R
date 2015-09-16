
##################################
## MAIN  ########################
# FUNCTION: read the message ###
###############################


readMessage <- function(path, spamDir)
  {

  # Read the email
  con <- file(paste(spamDir, "/",  path, sep = ""), open = 'rt')
  email <- readLines(con)
  close(con)
  
  if(grepl("^mv ", email[1]))
    return(NULL)
  
  # Separate the header
  sepEmail <- split.header(email)    
  head <- sepEmail$header
  
  #check for attachments
  fields <- names(head)
  ctLoc <- grep("^content-type$", fields)
  
  # content-type w/ boundary key -> attachments to extract
  if(length(ctLoc > 0) && grepl("boundary=", tolower(head[ctLoc]))){
    sepAtts <- sep.Att(sepEmail$body, head[ctLoc])
    return(list(head = sepEmail$header,
                body = sepAtts$body,
                atts = sepAtts$atts,
                path = path))}
  
    else 
    return(list(head = sepEmail$header,
                body = sepEmail$body,
                atts = character(),
                path = path)) 
  
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


# FUNCTION GET ATTACHMENTS
# separates body on boundary key from
# 'content-type' field in header

sep.Att <- function(body, ct)
  {
  # Split on '; ' then find split that contains 'boundary="xXxxxxxXXX"'
  ct <- unlist(strsplit(ct, ";[[:space:]]*"))
  key <- ct[grep("boundary", tolower(ct))]
  
  # keep only what's in between quotes
  key <- strsplit(key, "[(\\\')(\\\")]")[[1]][2]
 
  # key in body
  b1 <- paste('--', key, sep = "")
  b2 <- paste('--', key, "--", sep = "")
  
  # T/F vector for lines that = key in body
    bounds <- (body %in% c(b1, b2))
  # split on cumsum of that ^ vector
  parts <- split(body, cumsum(bounds))
  
  # sep attachments  
  isAtt <- do.call(c, lapply(parts, function(x) sum(x %in% b1) > 0))  
  atts <- parts[isAtt]
  atts <- lapply(atts, get.atts, b1)
    
  # body has no boundary keys except end boundary
  body <- do.call(c, parts[!isAtt])
  names(body) <- NULL  
  # remove end boundary
  body <- body[!(body %in% b2)]
  
  return(list(body = body, atts = atts)) 
  }

# FUNCTION: Creates list for each attachment
# content-type of attachment and text of 
# attachment

get.atts <- function(att, key)
  {
  # get cont-type of attachment
  ct <- att[grep('content-type', tolower(att))]
  ct <- strsplit(ct, "ype: ")[[1]][2]
  #remove boundary/content-type from body of attachment
  att <- att[-grep('content-type', tolower(att))]
  att <- att[-grep(key, att)]
  #return as list
  list(content = ct, text = att)
  }
