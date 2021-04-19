#' @author Victor Plesco
#' 
#' 
#' @title supp_eval.response
#' @description Makes a request to a Twitter API v2 search endpoint and switches the return based on the encountered errors.
#' 
#' @param token object of class v2_create_token(). Bearer token associated with a user-created APP (requires a developer account).
#' @param v2.endpoint string \{"/2/tweets/search/all" or "/2/tweets/search/recent"\}. Twitter API v2 Search Endpoint.
#' @param param_list list (e.g. max_results = 10, ect.). List of a request's parameters.
#' 
#' @return A list composed of a response's metadata and its parsed content (with attached errors if encountered).
#' 
#' @export
supp_eval.response <- function(token, v2.endpoint, param_list) {
  
  repeat { # Ensures continuability in case of server breakdown;
    
    response = httr::GET(url = paste0("https://api.twitter.com", v2.endpoint), 
                         httr::add_headers(.headers = token), query = param_list); 
    
    # Parsing response to list format (ensures readability of errors);
    parsed.content = httr::content(response, as = "parsed");

    # Switch Legend:
    # NULL - Status Code == [500, 504] (repeats);
    # 0    - Status Code != 200 & Status Code == 200 + Unsolvable Partial Errors (ends program);
    # 1    - Status Code == 200 + No Partial Errors (continues program);
    # 2    - Status Code == 200 + Solvable Partial Errors (continues program);
    
    if(any(response$status_code %in% c(500:504))) { # Switch NULL: Status Code == [500, 504];
      
      cat("\n\nWarning message:\n  Program has stopped due to error in response. Trying again in 2s!\n",
          "\n Title:", as.character(parsed.content$title), 
          "\n Status Code:", as.character(response$status_code), 
          "\n Details:", as.character(parsed.content$detail),
          "\n For more details please visit:", as.character(parsed.content$type), "\n\n");
      Sys.sleep(10);
      
    } else {
      
      if(response$status_code == 200) {
        
        if("errors" %in% names(parsed.content)) {
          
          # Switch 0: Status Code == 200 + Unsolvable Partial Errors (usage capped problem, invalid request problem, ect.);
          if(any(lapply(parsed.content$errors, "[[", "title") %in% c("Forbidden", "Not Found Error", "Authorization Error") == FALSE)) {
            cat("\n\nWarning message:\n  Program has stopped due to partial errors in response. Returning errors!\n\n");
            return(list(0, `response` = response, `parsed.content` = parsed.content));
            
          } else {# Switch 2: Status Code == 200 + Solvable Partial Errors (resource not found problem, resource unauthorized problem, ect.);
            cat("\n\nWarning message:\n  Program has encountered partial errors in response. Attaching to payload!\n\n");
            return(list(2, `response` = response, `parsed.content` = parsed.content));
            
          };
        # Switch 1: Status Code == 200 + No Partial Errors (i.e. successful request);
        } else {return(list(1, `response` = response, `parsed.content` = parsed.content));};
        
      } else { # Switch 0: Status Code != 200 (i.e. unsuccessful request);
        cat("\n\nWarning message:\n  Program has stopped due to error in response. Returning errors!\n",
            "\n Title:", as.character(parsed.content$title), 
            "\n Status Code:", as.character(response$status_code), 
            "\n Details:", as.character(parsed.content$detail),
            "\n For more details please visit:", as.character(parsed.content$type), "\n\n");
        return(list(0, `response` = response, `parsed.content` = parsed.content));
        
      };
    };
  };
};