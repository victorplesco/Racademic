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
#' @return:
#' 
#' 
v2_validate_request <- function(v2.endpoint = NULL, product_track = NULL, ...) {
  
  #' @description: 
  #' 
  #' 
  param_input = list(...)
  
  #' @description:
  #' [TO_BE_UPDATED: Finish v2_get_conditions function. Reminder: make product_track=NULL throw an error.]
  #' 
  #' `v2.param_conditions`
  v2.param_conditions = list (
    max_results_max       = 500,
    max_results_min       = 10,
    start_time_adjusted   = 30,
    query_length          = 1024
  );
  
  
  #' @description:
  #'
  #' `v2.param_list`
  source(paste0(sub("search-tweets-r.*", "", dirname(rstudioapi::getSourceEditorContext()$path)), "search-tweets-r/v2/support.macro/v2_get_endpoint_parameters.R"));
  param_list = v2_get_endpoint_parameters(v2.endpoint = v2.endpoint, read.only = FALSE, custom.param = names(param_input));
  
  
  #' @description:
  #' 
  #' `query`
  if(!is.null(param_input$query)) {
    #' [TO_BE_UPDATED: Finish v2_validate_query function.]
    param_list["query"] = param_input$query;
  }
  else {
    stop("INPUT ERROR: no query provided!");
  };
  
  #' @description:
  #' 
  #' `*_time`
  if(!is.null(param_input$start_time) & !is.null(param_input$end_time)) {
    
    if(is.na(strptime(param_input$start_time, format = "%Y-%m-%dT%H:%M:%SZ")) | is.na(strptime(param_input$end_time, format = "%Y-%m-%dT%H:%M:%SZ"))) {
      stop("INPUT ERROR: wrong format of input time!");
    }
    else {
      if(strptime(param_input$end_time, format = "%Y-%m-%dT%H:%M:%SZ") < strptime(param_input$start_time, format = "%Y-%m-%dT%H:%M:%SZ")) {
        stop("INPUT ERROR: end_time is smaller than start_time!");
      }
      else {
        param_list["start_time"] = as.character(param_input$start_time);
        param_list["end_time"]   = as.character(param_input$end_time);
      };
    };
  }
  else {
    if(!is.null(param_input$start_time) & is.null(param_input$end_time)) {
      stop("INPUT ERROR: no end_time provided!");
    };
    if(is.null(param_input$start_time) & !is.null(param_input$end_time)) {
      cat(paste("\nWarning message:\n  start_time not provided, setting default:\n\n    end_time:", 
                gsub("T|Z", " ", param_input$end_time), "\n  start_time:", 
                (as.POSIXct(param_input$end_time, format = "%Y-%m-%dT%H:%M:%SZ") - (60 * 60 * 24 * v2.param_conditions$start_time_adjusted)), "\n\n"));
    };
    if(is.null(param_input$start_time) & is.null(param_input$end_time)) {
      cat(paste("\nWarning message:\n  start_time and end_time not provided, setting default:\n\n    end_time:", 
                Sys.time() - 30, "\n  start_time:", (Sys.time() - 30) - (60 * 60 * 24 * v2.param_conditions$start_time_adjusted), "\n\n"));
    };
  };
  
  #' @description:
  #' 
  #' `max_results`
  if(!is.null(param_input$max_results)) {
    if(param_input$max_results < v2.param_conditions$max_results_min | param_input$max_results > v2.param_conditions$max_results_max) {
      stop("INPUT ERROR: max_results has to be in range [", v2.param_conditions$max_results_min, ", ", v2.param_conditions$max_results_max, "]!");
    }
    else {
      param_list["max_results"] = as.character(param_input$max_results);
    };
  }
  else {
    stop("INPUT ERROR: no max_results provided!");
  };
  
  
  #' @description:
  #' 
  #' `next_token`
  if(!is.null(param_input$next_token)) {
    param_list["next_token"] = param_input$next_token;
  };
  
  #' @description:
  #' 
  #' `since_id`
  if(!is.null(param_input$since_id)) {
    param_list["since_id"] = param_input$since_id;
  };
  
  #' @description:
  #' 
  #' `until_id`
  if(!is.null(param_input$until_id)) {
    param_list["until_id"] = param_input$until_id;
  };
  
  #' @description:
  #' 
  #' `expansions`
  if(!is.null(param_input$expansions)) {
    param_list["expansions"] = param_input$expansions;
  };
  
  #' @description:
  #' 
  #' `*.fields`
  source(paste0(sub("search-tweets-r.*", "", dirname(rstudioapi::getSourceEditorContext()$path)), "search-tweets-r/v2/support.macro/v2_get_object_fields.R"));
  if(!is.null(param_input$tweet.fields)) {param_list["tweet.fields"] = v2_get_object_fields(v2.object = "tweet.fields", custom.fields = param_input$tweet.fields);};
  if(!is.null(param_input$user.fields )) {param_list["user.fields" ] = v2_get_object_fields(v2.object = "user.fields",  custom.fields = param_input$user.fields );};
  if(!is.null(param_input$media.fields)) {param_list["media.fields"] = v2_get_object_fields(v2.object = "media.fields", custom.fields = param_input$media.fields);};
  if(!is.null(param_input$place.fields)) {param_list["place.fields"] = v2_get_object_fields(v2.object = "place.fields", custom.fields = param_input$place.fields);};
  if(!is.null(param_input$poll.fields )) {param_list["poll.fields" ] = v2_get_object_fields(v2.object = "poll.fields",  custom.fields = param_input$poll.fields );};
  
  #' @description 
  #' 
  #' @return
  return(param_list);
  
}; # v2_validate_request(v2.endpoint = "/2/tweets/search/all", query = "btc -is:retweet", product_track = NULL, max_results = 10, tweet.fields = c("id", "text"), end_time = "2021-04-04T16:59:22Z");