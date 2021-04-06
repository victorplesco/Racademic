v2_search_fullarchive <- function(token = NULL, 
                                  query = NULL, 
                                  start_time = NULL, end_time = NULL, max_results = NULL, next_token = NULL, since_id = NULL, until_id = NULL, expansions = NULL,
                                  tweet.fields = NULL, user.fields = NULL, media.fields = NULL, place.fields = NULL, poll.fields = NULL) {
  
  source(paste0(sub("search-tweets-r.*", "", dirname(rstudioapi::getSourceEditorContext()$path)), "search-tweets-r/v2/support.micro/v2_validate_request.R"));
  param_list = v2_validate_request(endpoint = "/2/tweets/search/all", 
                                   query = query, start_time = start_time, end_time = end_time, max_results = max_results, next_token = next_token, since_id = since_id, until_id = until_id, expansions = expansions,
                                   tweet.fields = tweet.fields, user.fields = user.fields, media.fields = media.fields, place.fields = place.fields, poll.fields = poll.fields);
  
  # response = httr::GET(url = 'https://api.twitter.com/2/tweets/search/all', httr::add_headers(.headers = token), query = param_list);
  
  # source(paste0(sub("search-tweets-r.*", "", dirname(rstudioapi::getSourceEditorContext()$path)), "search-tweets-r/v2/support.micro/v2_validate_response.R"));
  # v2_validate_response(headers = response$all_headers);
  print(param_list)
  return(param_list)
};

v2_search_fullarchive(max_results = 10, tweet.fields = "all")

