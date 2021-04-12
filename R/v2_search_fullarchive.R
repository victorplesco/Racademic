#' @author: Victor Plesco
#' 
#' 
#' @title v2_search_fullarchive
#' @description 
#' 
#' @param token racademic's object. A bearer token associated with a user-created APP (requires a developer account).
#' @param product_track string \{"academic" or "standard"\}.
#' @param safe.dir string (e.g. "~/home/...").
#' @param parse boolean \{TRUE or FALSE \}.
#' @param query string (e.g. "btc -is:retweet").
#' @param start_time string \{ \%Y-\%m-\%dT\%H:\%M:\%SZ \}.
#' @param end_time string \{ \%Y-\%m-\%dT\%H:\%M:\%SZ \}.
#' @param max_results integer [10, 500].
#' @param next_token 
#' @param since_id 
#' @param until_id 
#' @param expansions vector of strings (e.g. "expansion_\{1\}", ...). Specifies a subset of expansions to be returned (accepts "all"). 
#' To request all but some expansions, append "-" in front of each expansion in the input vector (e.g. "-expansion_\{1\}", ...). 
#' @param _.fields vector of strings (e.g. "field_\{1\}", ...). Specifies a subset of fields to be returned (accepts "all"). To 
#' request all but some fields, append "-" in front of each field in the input vector (e.g. "-field_\{1\}", ...). 
#' 
#' @return A list or parsed format of tweets and relative data.
#' 
#' @export
v2_search_fullarchive <- function(token, product_track, safe.dir = NULL, parse = FALSE, test = FALSE,
                                  query, start_time = NULL, end_time = NULL, max_results = 10, next_token = NULL, since_id = NULL, until_id = NULL, 
                                  expansions = NULL, tweet.fields = NULL, user.fields = NULL, media.fields = NULL, place.fields = NULL, poll.fields = NULL) {
  
  param_list = supp_validate_request(
    v2.endpoint = "/2/tweets/search/all", product_track = product_track, query = query, 
    start_time = start_time, end_time = end_time, max_results = max_results, next_token = next_token, since_id = since_id, until_id = until_id, expansions = expansions,
    tweet.fields = tweet.fields, user.fields = user.fields, media.fields = media.fields, place.fields = place.fields, poll.fields = poll.fields);
  
  if(test) {
    return(param_list);
  } else {
    twitter_data = supp_forward_request( # Download data;
      token = token, v2.endpoint = "/2/tweets/search/all", 
      param_list = param_list, safe.dir = safe.dir);
    
    ifelse(!is.null(parse),
           return(twitter_data), # Return data in list format;
           return(supp_parse_tweets(twitter_data)));
  };
}; 
