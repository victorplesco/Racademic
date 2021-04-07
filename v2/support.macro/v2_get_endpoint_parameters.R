#' @author: Victor Plesco
#' @lastupdate: 2021-04-04
#' 
#' 
#' @name: *v2_get_endpoint_parameters*
#' @description: serves as a container for the parameters of the endpoints available for request on the new Twitter API v2. 
#' 
#' @param v2.endpoint : string {"/2/___"}. Default=NULL. Specifies an endpoint for which a list of parameters is returned.
#' @param read.only : boolean {TRUE, FALSE}. Default=FALSE. Specifies the type of return of the function (@see @return for details).
#' @param custom.param : vector of strings {"PARAM_1", ...}. Default=NULL. Customizes the return, or print, by subsetting the parameters.
#' @param open.url : boolean {TRUE, FALSE}. Default=FALSE. If TRUE, opens the documentation page related to the @v2.endpoint specified.
#' 
#' @return: a list of parameters of the specified @v2.endpoint having NULL as value (if @read.only is FALSE), or will print the 
#' parameters alongside with their description (if @read.only is TRUE). If no @v2.endpoint is specified, prints all the endpoints 
#' available for request. 
#' 
#' 
v2_get_endpoint_parameters <- function(v2.endpoint = NULL, read.only = FALSE, custom.param = NULL, open.url = FALSE) {
  
  #' @description: to allow communication with main functions, @custom.param accepts "all" as an option, alongside NULL, for requesting 
  #' all the fields of a specified @v2.endpoint. To simplify code it was decided to switch @custom.param to NULL whenever "all" is requested.
  #' 
  #' 
  if(length(which(custom.param %in% "all" == TRUE)) == length(custom.param)) {custom.param = NULL};
  
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
  
  
  #' @description: building dependencies of @open.url and @read.only with @v2.endpoint. If @v2.endpoint is NULL, throws an error, else, 
  #' depending on the value of @read.only, will return (if FALSE) or print (if TRUE) the parameters of the specified @v2.endpoint. It is
  #' possible to customize the return, or print, by providing a set of parameters through @custom.param. Also if @open.url is TRUE, 
  #' the `browseURL()` function will be activated and the documentation page related to the provided @v2.endpoint will be opened in your 
  #' browser (@see `browseURL()` for more details).
  #' 
  #' 
  if(!is.null(v2.endpoint)) { # if an endpoint is provided;
    
    if(length(which(custom.param %in% names(v2.endpoints_list[v2.endpoint][[1]][[1]]) == TRUE)) != length(custom.param)) { # Checking errors in custom.param;
      stop("INPUT ERROR: the following parameters to not belong to ", v2.endpoint, 
           " [", paste0(custom.param[which(custom.param %in% names(v2.endpoints_list[v2.endpoint][[1]][[1]]) == FALSE)], collapse = ", "), "] ");
    };
    
    if(v2.endpoint %in% names(v2.endpoints_list) == FALSE) { # If no match with v2.endpoint;
      stop("INPUT ERROR: the endpoint provided is wrong!");
    }
    else { # If match with v2.endpoint;
      
      if(open.url == TRUE) {
        browseURL(v2.endpoints_list[v2.endpoint][[1]]["url"][[1]]);
      };
      
      if(read.only == FALSE) { # If read.only is TRUE
        if(!is.null(custom.param)) { # if custom parameters provided (RETURN);
          return(sapply(v2.endpoints_list[v2.endpoint][[1]][[1]][custom.param], "[[", 1));
        }
        else { # if no custom fields provided (RETURN);
          return(sapply(v2.endpoints_list[v2.endpoint][[1]][[1]], "[[", 1));
        };
      } 
      else { # If read.only is FALSE
        if(!is.null(custom.param)) { # if custom fields provided (PRINT);
          knitr::kable(sapply(v2.endpoints_list[v2.endpoint][[1]][[1]][custom.param], "[[", 2), 
                       caption = v2.endpoint, col.names = c("Description"), align = c("c", "c"));
        }
        else { # if no custom fields provided (PRINT);
          knitr::kable(sapply(v2.endpoints_list[v2.endpoint][[1]][[1]], "[[", 2), 
                       caption = v2.endpoint, col.names = c("Description"), align = c("c", "c"));
        };
      };
    };
  } 
  else { # if no endpoint is provided;
    stop("INPUT ERROR: no endpoint provided!");
  };
}; # v2_get_endpoint_parameters(v2.endpoint = "/2/tweets/search/all", read.only = TRUE, custom.param = c("start_time", "end_time", "query"), open.url = FALSE);
