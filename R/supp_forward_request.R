#' @author: Victor Plesco
#' 
#' 
#' @title supp_forward_request
#' @description 
#' 
#' @param token
#' @param v2.endpoint string \{"/2/___"\}. 
#' @param param_list mix (e.g. max_results = 10, query = "btc -is:retweet", ect.).
#' @param safe.dir string (e.g. "~/home/...").
#' 
#' @return
#' 
#' @export
supp_forward_request <- function(token, v2.endpoint, param_list, safe.dir) {
  
  safe.dir = supp_create_safedir(safe.dir = safe.dir); twitter_data = list(`data` = list(), `includes` = list());
  process.summary = list(`Requests` = 0, `Dates` = list(), `Size` = 0, );
  repeat { round.time = as.POSIXlt(Sys.time(), tz = "UTC");
  
    if(response_list$response.switch) {
      
      saveRDS(response_list$parsed.content[1:2], paste0(safe.dir, "content/", format(Sys.Date(), format = "%y%m%d"), "_", format(Sys.time(), format = "%H%M%S"), ".RDS"));
      twitter_data$data = append(response_list$parsed.content$data, twitter_data$data); twitter_data$includes = append(response_list$parsed.content$includes, twitter_data$includes);
      
      supp_validate_rate(
        round.time = round.time,
        limit.remaining = as.numeric(response_list$response$headers$`x-rate-limit-remaining`),
        limit.reset = as.numeric(response_list$response$headers$`x-rate-limit-reset`));
      
      param_list$next_token = response_list$parsed.content$meta$next_token;
      cat("\n", response_list$parsed.content$meta$next_token);
      
      if(is.null(param_list$next_token)) {return(twitter_data);};
    } else {twitter_data[["errors"]] = response_list$parsed.content$errors; return(twitter_data);}};
};