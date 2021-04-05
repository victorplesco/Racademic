v2_get_endpoint_conditions <- function(endpoint = NULL, parameter = NULL) {
  
  #' @description:
  #'
  #' @fullarchive.conditions
  `/2/tweets/search/all` = list (
    `max_results`          = list(`max_results_min` = 10, `max_results_max` = 500),
    `start_time`           = list(),
    `end_time`             = list()
  );
  #'
  #'
  #' @recent.conditions
  `/2/tweets/search/recent` = list (
    `max_results`          = list(`max_results_min` = 10, `max_results_max` = 100),
    `start_time`           = list(),
    `end_time`             = list()
  );
  
  #' @description:
  #' 
  #' @endpoints_list
  endpoints_list = list (
    `/2/tweets/search/all`    = `/2/tweets/search/all`,
    `/2/tweets/search/recent` = `/2/tweets/search/recent`
  );
  
  
};