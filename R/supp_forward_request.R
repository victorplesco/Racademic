#' @author: Victor Plesco
#' 
#' 
#' @title supp_forward_request
#' @description 
#' 
#' @param token racademic's object. Specifies a bearer token associated with a user-created APP (requires a developer account).
#' @param v2.endpoint string \{"/2/___"\}. Specifies an endpoint to which a request is made.
#' @param param_list mix (e.g. max_results = 10, query = "btc -is:retweet", ect.). Specifies list of parameters to be used within request.
#' @param safe.dir string (e.g. "~/home/..."). Specifies path to a directory.
#' 
#' @return If the request is successful returns a list with sub-lists "data", storing a tweet's data (each item is a tweet), and 
#' "includes", storing a tweet's expansions. If the request isn't successful appends, to the so far downloaded data, a sub-list "errors", 
#' storing the details of the errors in the request. 
#' 
#' @export
supp_forward_request <- function(token, v2.endpoint, param_list, safe.dir) {
  
  safe.dir = supp_create_safedir(safe.dir = safe.dir); twitter_data = list(`data` = list(), `includes` = list());
  process_summary = list(`requests` = 0, `daily_counts` = list(), `current_token` = NULL, `next_token` = NULL);
  print(process_summary)
  # The following repeat statement will handle pagination;
  repeat { round.time = as.POSIXlt(Sys.time(), tz = "UTC");
  
    response_list = supp_validate_response(token = token, v2.endpoint = v2.endpoint, param_list = param_list);
    if(response_list$response.switch) { # If successful response;
      
      saveRDS(response_list$parsed.content[1:2], paste0(safe.dir, "content/", format(Sys.Date(), format = "%y%m%d"), "_", format(Sys.time(), format = "%H%M%S"), ".RDS"));
      twitter_data = mapply(c, twitter_data, response_list$parsed.content[1:2], SIMPLIFY = F); 
      
      ifelse(is.null(response_list$parsed.content$meta$next_token), 
             return(twitter_data), # If next_token is NULL;
             {process_summary$current_token = param_list$next_token; param_list$next_token = response_list$parsed.content$meta$next_token;});
      
      process_summary = supp_print_summary(process.data = process_summary, twitter.chunk = response_list$parsed.content);
      
      # Checking rate limits and imposing system sleep;
      supp_validate_limitrate(round.time = round.time,
        limit.remaining = as.numeric(response_list$response$headers$`x-rate-limit-remaining`),
        limit.reset = as.numeric(response_list$response$headers$`x-rate-limit-reset`));
      
    } else {twitter_data[["errors"]] = response_list$parsed.content$errors; return(twitter_data);}};
};