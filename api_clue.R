source("/drives/slave/TBU/LINCS_RawData/Functions_l1ktools.R")

# GETquery.api.CLUE (v.1.0) ::: QUERY to the https://api.clue.io/ via RESTful service.
## Description
###   By default, it will try to perform the query and retrieve if there 
###     was ANY response from the RESTful service. (status of the query).
###     If there was no response, it could be any of the following situations:
###       a) an Authentification failed because the user_key from the query is not correct.
###       b) an Authentification failed because the server banned the user due to massive 
###       queries in a short time (>2500 per hour). For instance, this happens if you loop 
###       too many small queries without any delay.
###       c) The Service is temporaly down.
## Parameters
### query.url : (character) The complete URL for the query RESTful (including key_user).
### tryGETresponse : (boolean) Choose if it try to get any response from the server to 
###                     avoid missing results due to beeing banned
### tryNtimesIfFailed : (positive integer) How many times it must try to get any response if it fail.
### tryAgainAfter_Xmins : (positive numeric) How long it must wait until the next try.
## Dependencies:
###   library(rjson) # Read json into a list in R
###   require(RCurl) # To get text from URL as json object
GETquery.api.CLUE <- function(query.url,tryGETresponse=TRUE,tryNtimesIfFailed=3,tryAgainAfter_Xmins=20) {
  require(rjson)
  require(RCurl)
  # Parameters sanity check:
  if(!is.character(query.url) | length(query.url)!=1)
    stop("ERROR #1 : The 'query.url' parameter must be a character vector of length 1");
  if(!is.logical(tryGETresponse) | length(tryGETresponse)!=1)
    stop("ERROR #2 : The 'tryGETresponse' parameter must be a logical of length 1");
  if(tryNtimesIfFailed%%1!=0 | length(tryNtimesIfFailed)!=1 | tryNtimesIfFailed<0)
    stop("ERROR #3 : The 'tryNtimesIfFailed' parameter must be an positive integer of length 1");
  if(!is.numeric(tryAgainAfter_Xmins) | length(tryAgainAfter_Xmins)!=1)
    stop("ERROR #4 : The 'tryAgainAfter_Xmins' parameter must be a positive number of length 1");
  
  
  # Using tryGETresponse, it avoids missing results because of massive queries to the API.
  # So, if the query fails, it will wait 'tryAgainAfter_Xmins' seconds until tryNtimesIfFailed gets 0.
  info.got <- vector()
  if(tryGETresponse) {
    tryAgain <- TRUE # First time
    while(tryAgain) {
      #It launch the query
      status <- tryCatch(
        info.got <- fromJSON(getURLContent(query.url, customrequest = "GET")),
        error = function(e) e
      )
      if(inherits(status,  "error")) {
        warning("api.clue.io's API did NOT response");
        if(tryNtimesIfFailed!=0) {
          tryNtimesIfFailed <- tryNtimesIfFailed-1;
          print(paste0("Go to sleep and try Again after ",tryAgainAfter_Xmins,"minutes"));
          Sys.sleep(60*tryAgainAfter_Xmins);
        } else {
          tryAgain <- FALSE; # Get out, it failed but the user do not want to try again
        }
      } else { # It works out well
        tryAgain <- FALSE; # Get out
      }
    } # END of while
  } else { # If tryGETresponse is disabled, just do the query:
    info.got <- fromJSON(getURLContent(query.url, customrequest = "GET"));
  }
  
  # it returns a list, empty if the query fails or got 0 results, or with something
  return(info.got); 
}

BUILDquery.filter <- function(service="profiles",
                              where=c("pert_type"="trt_sh"),
                              fields=NULL,
                              features=c("limit"=1000,"skip"=0),
                              user_key="9332b818ed68dd90d5915a83d5cf753a") {
  # Internal variables
  web.url <- "https://api.clue.io/api/";
  
  # Process params
  if(is.vector(where)) {
    where.url_items <- sapply(1:length(where),function(z) paste0("%22",names(where)[z],"%22:%22",where[z],"%22"))
  } else if (is.list(where)) {
    where.url_items <- sapply(1:length(where), function(z) {
      if(length(where[[z]])==1) {
        paste0("%22",names(where)[z],"%22:",where[[z]])    
      } else if (length(where[[z]])>1) {
        paste0("%22",names(where)[z],"%22:[",paste0("%22",where[[z]],"%22",collapse=","),"]")
      }
    })
  }
  where.url <- paste(where.url_items,collapse=",")
  
  # Process fields
  if(!is.null(fields)) {
    fields.url_items <- sapply(1:length(fields),function(z) paste0("%22",names(fields)[z],"%22:",fields[z]))
    fields.url <- paste(fields.url_items,collapse=",")
  }
  
  # Process features
  features.url_items <- sapply(1:length(features),function(z) paste0("%22",names(features)[z],"%22:",features[z]))
  features.url <- paste(features.url_items,collapse=",")
  
  # Build the query
  query.url <- paste0(web.url,service,"?filter={%22where%22:","{",where.url,"}")
  if(!is.null(fields)) query.url <- paste0(query.url,",%22fields%22:{",fields.url,"}");
  query.url <- paste0(query.url,",",features.url)
  query.url <- paste0(query.url,"}&user_key=",user_key)
  
  
  # Return the URL for the query
  return(query.url)
}


BUILDquery.count <- function(service="profiles",
                              where=c("pert_type"="trt_sh"),
                              user_key="9332b818ed68dd90d5915a83d5cf753a") {
  # Internal variables
  web.url <- "https://api.clue.io/api/";
  
  # Process params
  if(is.vector(where)) {
    where.url_items <- sapply(1:length(where),function(z) paste0("%22",names(where)[z],"%22:%22",where[z],"%22"))
  } else if (is.list(where)) {
    where.url_items <- sapply(1:length(where), function(z) {
      if(length(where[[z]])==1) {
        paste0("%22",names(where)[z],"%22:",where[[z]])    
      } else if (length(where[[z]])>1) {
        paste0("%22",names(where)[z],"%22:[",paste0("%22",where[[z]],"%22",collapse=","),"]")
      }
    })
  }
  where.url <- paste(where.url_items,collapse=",")

  # Build the query
  query.url <- paste0(web.url,service,"/count?={%22where%22","=",where.url,"}")
  query.url <- paste0(query.url,"&user_key=",user_key)
  
  
  # Return the URL for the query
  return(query.url)
}