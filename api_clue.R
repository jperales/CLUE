
# GETquery.api (v.1.0) ::: QUERY to the https://api.clue.io/ via RESTful service.
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
GETquery.api <- function(query.url,tryGETresponse=TRUE,tryNtimesIfFailed=3,tryAgainAfter_Xmins=20) {
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
        warning("LINCS API did NOT response");
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