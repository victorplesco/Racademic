#' @author Victor Plesco
#' 
#' 
#' @title supp_get_expansion_fields
#' @description Getter for the fields of the expansions available for request on the Twitter API v2. 
#'
#' @param expansion string \{"tweet" or "user"\}. Specifies an expansion for which a comma separated string of fields is returned.
#' @param fields vector of strings (e.g. "field_\{1\}", ...). Specifies a subset of fields to be returned (accepts "all"). To 
#' request all but some fields, append "-" in front of each field in the input vector (e.g. "-field_\{1\}", ...). 
#' 
#' @return A comma separated string of fields (e.g. "author_id,geo.place_id").
#' 
#' @export
supp_get_expansion_fields <- function(expansion, fields) {
  
  expansions_switch = switch(expansion,
    
    tweet = c(
      "author_id"                      ,
      "in_reply_to_user_id"            ,
      "entities.mentions.username"     ,
      "referenced_tweets.id.author_id" ,
      "referenced_tweets.id"           ,
      "attachments.media_keys"         ,
      "attachments.poll_ids"           ,
      "geo.place_id"                   
    ),
    
    user = c(
      "pinned_tweet_id"                
    ),
    
    stop("The provided expansion is wrong or is not handled!")
  );  

  if(identical(fields, "all")) { # Case 1: fields = c("all");
    return(paste(expansions_switch, collapse = ","));
  } else {
  
    # If mismatch;
    if(anyNA(match(sub("^-", "", fields), expansions_switch))) { 
      
      ifelse(any(sub("^-", "", fields) == "all"), # Case 2: fields = c("all", "field_{1}", ...);
             stop("The request to \"all\" is a standalone!"),
             stop("The following fields do not belong to ", expansion, # Case 3: fields = c("wrong_field_{1}", "field_{1}", ...);
                  " [", paste0(setdiff(sub("^-", "", fields), expansions_switch), collapse = ", "), "]!"));
    
      } else {
      
      # If no mismatch;
      ifelse(any(duplicated(sub("^-", "", fields))), # Case 4: fields = c("field_{1}", "field_{1}", ...);
             stop("Duplicated fields [", paste(sub("^-", "", fields)[which(duplicated(sub("^-", "", fields)))], collapse = ", "), "] have been requested!"),
             ifelse(any(grepl("-", fields) == any(!grepl("-", fields))), # Case 5: fields = c("-field_{1}", "field_{2}", ...);
                    stop("The request to \"-\" is a standalone!"),
                    ifelse(any(grepl("-", fields)), # Case 6: fields = c("-field_{1}", "-field_{2}", ...);
                           return(paste(grep(paste(sub("^-", "", fields), collapse = "|"), expansions_switch, invert = TRUE, value = TRUE), collapse = ",")),
                           return(paste(fields, collapse = ","))))); # Case 7: fields = c("field_{1}", "field_{2}", ...);
    };
  };
};