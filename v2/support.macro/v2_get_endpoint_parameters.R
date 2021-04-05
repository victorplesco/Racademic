#' @author: Victor Plesco
#' @lastupdate: 2021-04-04
#' 
#' 
#' @name: *v2_get_endpoint_parameters*
#' @description: this function serves as a descriptive function of the various endpoints available for request on the new Twitter API v2 
#' and their related parameters and returns, on request, all of the parameters of a specified *endpoint* as a list where each parameter 
#' has NULL as value (@e.g. `end_time` = NULL, `start_time` = NULL). 
#' 
#'     @usage [DESCRIPTIVE] A print can be requested by specifying *show.endpoints* or *show.parameters* as TRUE. The first will print 
#'     all the available endpoints and the second will print all the available parameters for a specified *endpoint*. 
#'     
#'     @usage [QUERY-BUILDING] A return can be requested by specifying *return* as TRUE and by providing an *endpoint*. All of the
#'     available parameters for the specified *endpoint* will be returned as a list. This function serves as the base structure for 
#'     building a query, since the returned list will be subsequently filled with input parameters and validated for product track
#'     limitations and logical inconsistencies. 
#' 
#' @param show.endpoints : boolean {TRUE, FALSE}. [Standalone]. Requires a print of all available to request endpoints. The parameter 
#' is a standalone and shouldn't be used together with other parameters of the function. Mainly it has to be used if you don't know
#' which are the endpoints of the new Twitter API v2.
#' 
#' @param endpoint : string {"/2/___"}. [Conjunction required]. Container for an endpoint to be used in concordance with *show.parameters* 
#' or *return* parameters. The container accepts only one endpoint per call of the function. Ideally to be used after an initial call
#' with show.endpoints = TRUE.
#' 
#' @param show.parameters : boolean {TRUE, FALSE}. [Conjunction required]. Requires a print of the parameters of a specified *endpoint* 
#' alongside with their description. The idea here is to use this print in order to visualize which are the parameters of an endpoint.
#' 
#' @param return : boolean {TRUE, FALSE}. [Conjunction required]. If TRUE will require a return of all the parameters related to a 
#' specified *endpoint*. This parameter has to be combined with *endpoint*.
#' 
#' @param open.url : boolean {TRUE, FALSE}. [Conjunction required]. Opens the documentation page related to the specified *endpoint*.
#' 
#' 
v2_get_endpoint_parameters <- function(show.endpoints = FALSE, endpoint = NULL, show.parameters = FALSE, return = FALSE, open.url = FALSE) {
  
  #' @description: building containers for each endpoint with its respective parameters. The parameters are taken from the Twitter
  #' documentation page "Tweet Search" and will serve as the structure for building your query. Alongside each parameter there is a short
  #' description assessing the information it represents.
  #' [TO_BE_UPDATED: Add descriptions to parameters of endpoints.]
  #' 
  #' `Search Tweets (full-archive)`
  `Search Tweets (full-archive)` = list (
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
  #' `Search Tweets (recent)`
  `Search Tweets (recent)` = list (
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

  
  #' @description:
  #' 
  #' @endpoints_list
  endpoints_list = list (
    `Search Tweets (full-archive)` = list(`Search Tweets (full-archive)`, `endpoint` = "/2/tweets/search/all",    `url` = "https://developer.twitter.com/en/docs/twitter-api/tweets/search/api-reference/get-tweets-search-all"),
    `Search Tweets (recent)`       = list(`Search Tweets (recent)`,       `endpoint` = "/2/tweets/search/recent", `url` = "https://developer.twitter.com/en/docs/twitter-api/tweets/search/api-reference/get-tweets-search-recent")
 );


  #' @description: if *show.endpoints* is set to TRUE, iterates through *endpoints_list* and prints the name of the endpoint alongside 
  #' with an integer as index and its url.
  #' 
  #' @show.endpoints
  if(show.endpoints == TRUE)
  {
    cat("\n Endpoints:\n", strrep("*", max(nchar(names(endpoints_list))) + 5), "\n")
    for(i in 1:length(endpoints_list))
    {
      cat(" ", i, ":", # Index of the endpoint;
          names(endpoints_list[i]), # Name of the endpoint;
          strrep(" ", (max(nchar(names(endpoints_list)))) - max(nchar(names(endpoints_list[i])))), # Padding for pretty printing;
          as.character(endpoints_list[[i]][2]), # URL of the endpoint;
          "\n");
    };
  };
  
  
  #' @description: if *endpoint* is provided, a series of outputs can be requested.
  #' [TO_BE_UPDATED: Modify the function in order to accept the index of an endpoint as endpoint input.]
  #' 
  #' @endpoint
  if(!is.null(endpoint)) 
  {
    
    
    #' @description: building dependencies of *endpoint* with other parameters. If object is != NULL, a conjunction among *open.url*,
    #' *show.parameters* or *return* is required, otherwise the function will return no output.
    #' 
    #' 
    if(open.url == FALSE & show.parameters == FALSE & return == FALSE) {
      stop("INPUT ERROR: no conjunction for endpoint provided!");
    };
    
    
    #' @description: if *open.url* is set to TRUE, activates the `browseURL()` function and opens the url of the documentation page of 
    #' the specified *endpoint*. Mainly this is done to make it easier for the user to reach the original descriptions of each *endpoint* 
    #' and its related parameters.
    #' 
    #' @open.url
    if(open.url == TRUE) {
      browseURL(endpoints_list[names(which(sapply(endpoints_list, "[[", 2) == endpoint))][[1]]["url"][[1]]);
    };
    
    
    #' @description: if *show.parameters* is set to TRUE, iterates through the container of a specified *endpoint* and prints its parameters 
    #' alongside with a short description.
    #' 
    #' @show.parameters
    if(show.parameters == TRUE) {
      cat("\n", paste0(endpoint, ":"), "\n", strrep("*", (max(nchar(names(endpoints_list[names(which(sapply(endpoints_list, "[[", 2) == endpoint))][[1]][[1]])))) + 5), "\n");
      for(i in 1:lengths(endpoints_list[names(which(sapply(endpoints_list, "[[", 2) == endpoint))][[1]][1], use.names = TRUE))
      {
        cat(if(i < 10) {" "},
            i, ":", # Index of the parameter;
            names(endpoints_list[names(which(sapply(endpoints_list, "[[", 2) == endpoint))][[1]][[1]])[i], # Name of the parameter;
            
            # Padding for pretty printing;
            strrep(" ", (max(nchar(names(endpoints_list[names(which(sapply(endpoints_list, "[[", 2) == endpoint))][[1]][[1]]))) - 
                         max(nchar(names(endpoints_list[names(which(sapply(endpoints_list, "[[", 2) == endpoint))][[1]][[1]][i]))))),
            as.character(endpoints_list[names(which(sapply(endpoints_list, "[[", 2) == endpoint))][[1]][[1]][[i]][2]), "\n"); # Description of the parameter;
      };
    };
    
    
    #' @description: if *return* is set to TRUE and an *endpoint* is provided, the function will return a list of its parameters having 
    #' their values set to NULL. This is done since, while making a request to the Twitter API v2, NULL values are not considered and 
    #' thus even if the user doesn't want to set a parameter, having it as NULL within the query won't harsh the results.
    #' 
    #' @return
    if(return == TRUE) {
      return(sapply(endpoints_list[names(which(sapply(endpoints_list, "[[", 2) == endpoint))][[1]][[1]], "[[", 1));
    };
  }
  else {
    #' @description: building dependencies of *open.url*, *show.parameters* and *return* with *endpoint* parameter. If one of the latter 
    #' is set to TRUE, an *endpoint* should be provided as well. Otherwise the function will throw an error.
    #' 
    #' 
    if(open.url == TRUE | show.parameters == TRUE | return == TRUE) {
      stop("INPUT ERROR: no endpoint provided!");
    };
  };
}; # v2_get_endpoint_parameters(show.endpoints = FALSE, endpoint = NULL, show.parameters = TRUE, return = FALSE, open.url = FALSE);