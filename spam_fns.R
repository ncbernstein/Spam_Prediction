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
