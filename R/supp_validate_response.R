#' @author: Victor Plesco
#' 
#' 
#' @title supp_validate_response
#' @description 
#' 
#' @param token racademic's object. Specifies a bearer token associated with a user-created APP (requires a developer account).
#' @param v2.endpoint string \{"/2/___"\}. Specifies an endpoint to which a request is made.
#' @param param_list mix (e.g. max_results = 10, query = "btc -is:retweet", ect.). Specifies list of parameters to be used within request.
#' 
#' @return If the request is successful returns a list with sub-lists "data", storing a tweet's data (each item is a tweet), and 
#' "includes", storing a tweet's expansions. If the request isn't successful appends, to the so far downloaded data, a sub-list "errors", 
#' storing the details of the errors in the request. 
#' 
#' @export
supp_validate_response <- function(token, v2.endpoint, param_list) {
  
  response = httr::GET(url = paste0("https://api.twitter.com", v2.endpoint), 
                       httr::add_headers(.headers = token), 
                       query = param_list); 
  parsed.content = httr::content(response, as = "parsed"); 
  
  if(response$status_code != 200) { # Non-200-series-HTTP errors;
    
    cat("\n\nWarning message:\n  Program has stopped due to error in response. Returning errors!\n",
        "\n Title:", parsed.content$title, "\n Status Code:", response$status_code, "\n Details:", parsed.content$detail,
        "\n For more details please visit:", parsed.content$type, "\n\n");
    return(list(`parsed.content` = parsed.content, `response.switch` = 0));
           
  } else {
    
    if("errors" %in% names(parsed.content)) { # 200-series-HTTP errors;
      cat("\n\nWarning message:\n  Program has stopped due to partial error in response. Returning errors!\n\n");
      return(list(`parsed.content` = parsed.content, `response.switch` = 0));
      
    } else {return(list(`response` = response, `parsed.content` = parsed.content, `response.switch` = 1));};
  };
};