#' @author: Victor Plesco
#' @lastupdate: 2021-05-04
#' 
#' 
#' @name: *v2_validate_request*
#' @description: 
#' 
#' @param v2.endpoint :
#' @param product_track : 
#' @param ... : 
#' 
#' 
v2_validate_request <- function(v2.endpoint = NULL, product_track = NULL, ...) {
  
  param_input = list(...); # list of query parameters;
  
  switch(v2.endpoint,
         
         `/2/tweets/search/all` = 
           {
             source(paste0(sub("search-tweets-r.*", "", dirname(rstudioapi::getSourceEditorContext()$path)), "search-tweets-r/v2/utilities/support.v2_validate_request/v2_validate_search_fullarchive_request.R"));
             v2_validate_search_fullarchive_request(endpoint = endpoint, product_track = product_track, param_input = param_input);
           },
         
         `/2/tweets/search/recent` =
           {
             source(paste0(sub("search-tweets-r.*", "", dirname(rstudioapi::getSourceEditorContext()$path)), "search-tweets-r/v2/utilities/support.v2_validate_request/v2_validate_search_recent_request.R"));
             v2_validate_search_recent_request(endpoint = endpoint, product_track = product_track, param_input = param_input);
           }
  );
}; # v2_validate_request(v2.endpoint = "/2/tweets/search/recent", max_results = 10, tweet.fields = c("all"), end_time = "2021-04-04T16:59:22Z");