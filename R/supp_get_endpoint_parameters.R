#' @author Victor Plesco
#' 
#' 
#' @title supp_get_endpoint_parameters
#' @description Getter for the parameters of the endpoints available for request on the new Twitter API v2.
#' 
#' @param v2.endpoint string ("/2/___"). Specifies an endpoint for which a list of parameters is returned.
#' @param params vector of strings ("PARAM_1", ...). If specified, subsets the returned parameters (accepts "all").
#' 
#' @return A list of parameters for the specified v2.endpoint having NULL as value.
#' 
#' @export
supp_get_endpoint_parameters <- function(v2.endpoint, params) {
  
  v2.endpoints_switch = switch(v2.endpoint,
    
    `/2/tweets/search/all` = list (
      `end_time`            = NULL,
      `expansions`          = NULL,
      `max_results`         = NULL,
      `media.fields`        = NULL,
      `next_token`          = NULL,
      `place.fields`        = NULL,
      `poll.fields`         = NULL,
      `query`               = NULL,
      `since_id`            = NULL,
      `start_time`          = NULL,
      `tweet.fields`        = NULL,
      `until_id`            = NULL,
      `user.fields`         = NULL
    ),
    
    `/2/tweets/search/recent` = list (
      `end_time`            = NULL,
      `expansions`          = NULL,
      `max_results`         = NULL,
      `media.fields`        = NULL,
      `next_token`          = NULL,
      `place.fields`        = NULL,
      `poll.fields`         = NULL,
      `query`               = NULL,
      `since_id`            = NULL,
      `start_time`          = NULL,
      `tweet.fields`        = NULL,
      `until_id`            = NULL,
      `user.fields`         = NULL
    ),
    
    stop("The provided endpoint is wrong or is not handled!")
  );

  if(any(params == "all")) {
    return(v2.endpoints_switch)
  } 
  else {
    if(length(which(params %in% names(v2.endpoints_switch) == TRUE)) != length(params)) { # Checking errors in params;
      stop("The following parameters do not belong to ", v2.endpoint, 
           " [", paste0(params[which(params %in% names(v2.endpoints_switch) == FALSE)], collapse = ", "), "] ");
    } else {return(v2.endpoints_switch[params]);};
  };
};