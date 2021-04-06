#' @author: Victor Plesco
#' @lastupdate: 2021-04-04
#' 
#' 
#' @name: *v2_get_endpoint_parameters*
#' @description: serves as a container for the parameters of the endpoints available for request on the new Twitter API v2. 
#' 
#' @param v2.endpoint : string {"/2/___"}. Default=NULL. Specifies an endpoint for which a list of parameters is returned.
#' @param read.only : boolean {TRUE, FALSE}. Default=FALSE. Specifies the type of return of the function (@see @return for details).
#' @param open.url : boolean {TRUE, FALSE}. Default=FALSE. If TRUE, opens the documentation page related to the @v2.endpoint specified.
#' 
#' @return: a list of parameters of the specified @v2.endpoint having NULL as value (if @read.only is FALSE), or will print the 
#' parameters alongside with their description (if @read.only is TRUE). If no @v2.endpoint is specified, prints all the endpoints 
#' available for request. 
#' 
#' 
v2_get_endpoint_parameters <- function(v2.endpoint = NULL, read.only = FALSE, open.url = FALSE) {
  
  #' @description: building a container for each endpoint with its respective parameters. Alongside each parameter there is a list with 
  #' a NULL value and a short description assessing the information it represents.
  #' [TO_BE_UPDATED: Add descriptions.]
  #' 
  #' `/2/tweets/search/all`
  `/2/tweets/search/all` = list (
    `end_time`            = list(NULL, "DESCRIPTION_NOT_AVAILABLE"),
    `expansions`          = list(NULL, "DESCRIPTION_NOT_AVAILABLE"),
    `max_results`         = list(NULL, "DESCRIPTION_NOT_AVAILABLE"),
    `media.fields`        = list(NULL, "DESCRIPTION_NOT_AVAILABLE"),
    `next_token`          = list(NULL, "DESCRIPTION_NOT_AVAILABLE"),
    `place.fields`        = list(NULL, "DESCRIPTION_NOT_AVAILABLE"),
    `poll.fields`         = list(NULL, "DESCRIPTION_NOT_AVAILABLE"),
    `query`               = list(NULL, "DESCRIPTION_NOT_AVAILABLE"),
    `since_id`            = list(NULL, "DESCRIPTION_NOT_AVAILABLE"),
    `start_time`          = list(NULL, "DESCRIPTION_NOT_AVAILABLE"),
    `tweet.fields`        = list(NULL, "DESCRIPTION_NOT_AVAILABLE"),
    `until_id`            = list(NULL, "DESCRIPTION_NOT_AVAILABLE"),
    `user.fields`         = list(NULL, "DESCRIPTION_NOT_AVAILABLE")
  );  
  #'
  #'
  #' `/2/tweets/search/recent`
  `/2/tweets/search/recent` = list (
    `end_time`            = list(NULL, "DESCRIPTION_NOT_AVAILABLE"),
    `expansions`          = list(NULL, "DESCRIPTION_NOT_AVAILABLE"),
    `max_results`         = list(NULL, "DESCRIPTION_NOT_AVAILABLE"),
    `media.fields`        = list(NULL, "DESCRIPTION_NOT_AVAILABLE"),
    `next_token`          = list(NULL, "DESCRIPTION_NOT_AVAILABLE"),
    `place.fields`        = list(NULL, "DESCRIPTION_NOT_AVAILABLE"),
    `poll.fields`         = list(NULL, "DESCRIPTION_NOT_AVAILABLE"),
    `query`               = list(NULL, "DESCRIPTION_NOT_AVAILABLE"),
    `since_id`            = list(NULL, "DESCRIPTION_NOT_AVAILABLE"),
    `start_time`          = list(NULL, "DESCRIPTION_NOT_AVAILABLE"),
    `tweet.fields`        = list(NULL, "DESCRIPTION_NOT_AVAILABLE"),
    `until_id`            = list(NULL, "DESCRIPTION_NOT_AVAILABLE"),
    `user.fields`         = list(NULL, "DESCRIPTION_NOT_AVAILABLE")
  );  
  
  
  #' @description: aggregating the previously created containers in a list with sub-lists format and adding metadata (e.g. url).
  #' 
  #' `v2.endpoints_list`
  v2.endpoints_list = list (
    `/2/tweets/search/all`    = list(`parameters` = `/2/tweets/search/all`,    `url` = "https://developer.twitter.com/en/docs/twitter-api/tweets/search/api-reference/get-tweets-search-all"),
    `/2/tweets/search/recent` = list(`parameters` = `/2/tweets/search/recent`, `url` = "https://developer.twitter.com/en/docs/twitter-api/tweets/search/api-reference/get-tweets-search-recent")
  );
  
  
  #' @description: building dependencies of @open.url and @read.only with @v2.endpoint. If @v2.endpoint is NULL, prints the endpoints
  #' available for request, else, depending on the value of @read.only, will return (if FALSE) or print (if TRUE) the parameters of the 
  #' specified @v2.endpoint. Also if @open.url is TRUE, the `browseURL()` function will be activated and the documentation page related 
  #' to the provided @v2.endpoint will be opened in your browser (@see `browseURL()` for more details).
  #' 
  #' 
  if(!is.null(v2.endpoint)) { # if an endpoint is provided;
    
    if(v2.endpoint %in% names(v2.endpoints_list) == FALSE) {
      stop("INPUT ERROR: the endpoint provided is wrong!");
    };
    
    if(open.url == TRUE) {
      browseURL(v2.endpoints_list[v2.endpoint][[1]]["url"][[1]]);
    };
    
    if(read.only == FALSE) {
      return(sapply(v2.endpoints_list[v2.endpoint][[1]][[1]], "[[", 1));
    } 
    else {
      knitr::kable(sapply(v2.endpoints_list[v2.endpoint][[1]][[1]], "[[", 2), 
                   caption = v2.endpoint, col.names = c("Description"), align = c("c", "c"));
    };
    
  } 
  else { # if no endpoint is provided;
    
    if(read.only == TRUE | open.url == TRUE) {
      stop("INPUT ERROR: no endpoint provided!")
    }
    else {
      knitr::kable(names(v2.endpoints_list), col.names = "Available endpoints", align = "c");
    };
    
  };
}; # v2_get_endpoint_parameters(v2.endpoint = "/2/tweets/search/all", read.only = TRUE, open.url = FALSE);