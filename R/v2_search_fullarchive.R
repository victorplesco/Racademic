#' @author Victor Plesco
#' 
#' 
#' @title v2_get_endpoint_parameters
#' @description Method of [v2_search_fullarchive] and [v2_search_recent()] functions that serves as a container for the parameters of 
#' the endpoints available for request on the new Twitter API v2. 
#' 
#' @param v2.endpoint string ("/2/___"). . Specifies an endpoint for which a list of parameters is returned.
#' @param read.only boolean (TRUE, FALSE). Default=FALSE. Specifies the type of return of the function (see return for details).
#' @param custom.param vector of strings ("PARAM_1", ...). Default=NULL. Customizes the return, or print, by subsetting the parameters.
#' @param open.url boolean (TRUE, FALSE). Default=FALSE. If TRUE, opens the documentation page related to the v2.endpoint specified.
#' 
#' @return A list of parameters of the specified v2.endpoint having NULL as value (if read.only is FALSE), or will print the parameters 
#' alongside with their description (if read.only is TRUE). If no v2.endpoint is specified, prints all the endpoints available for request. 
#' 
#' @export
v2_search_fullarchive <- function(v2.token, product_track, query, 
                                  start_time = NULL, end_time = NULL, max_results = 10, next_token = NULL, since_id = NULL, until_id = NULL, 
                                  expansions = NULL, tweet.fields = NULL, user.fields = NULL, media.fields = NULL, place.fields = NULL, poll.fields = NULL) {
  
  param_list = supp_validate_request(
    v2.endpoint = "/2/tweets/search/all", product_track = product_track, query = query, 
    start_time = start_time, end_time = end_time, max_results = max_results, next_token = next_token, since_id = since_id, until_id = until_id, expansions = expansions,
    tweet.fields = tweet.fields, user.fields = user.fields, media.fields = media.fields, place.fields = place.fields, poll.fields = poll.fields);

  # while()
  # response = httr::GET(url = 'https://api.twitter.com/2/tweets/search/all', httr::add_headers(.headers = token), query = param_list);
  
  # source(paste0(sub("search-tweets-r.*", "", dirname(rstudioapi::getSourceEditorContext()$path)), "search-tweets-r/v2/support.micro/v2_validate_response.R"));
  # v2_validate_response(headers = response$all_headers);
  return(param_list)
  
}; 
