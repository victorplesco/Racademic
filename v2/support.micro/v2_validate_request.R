#' @author: Victor Plesco
#' @lastupdate: 2021-05-04
#' 
#' 
#' @name: *v2_validate_request*
#' @description: 
#' 
#' @param endpoint : 
#' @param ... : 
#' 
#' 
v2_validate_request <- function(endpoint = NULL, ...) {
  
  param_input = list(...); # list of input parameters;
  
  if(endpoint == "/2/tweets/search/all") {
    max_results_max = 500; max_results_min = 10; start_time_adjusted = 30;
  };
  if(endpoint == "/2/tweets/search/recent") {
    max_results_max = 100; max_results_min = 10; start_time_adjusted = 7;
  };

  source(paste0(sub("search-tweets-r.*", "", dirname(rstudioapi::getSourceEditorContext()$path)), "search-tweets-r/v2/support.macro/v2_get_endpoint_parameters.R"));
  param_list = v2_get_endpoint_parameters(endpoint = endpoint, return = TRUE);
  
  #' @description: checking if max_results belongs to the interval [max_results_min, max_results_max].
  #' 
  #' @max_results
  if(!is.null(param_input$max_results)) {
    if(param_input$max_results < max_results_min | param_input$max_results > max_results_max) {
      stop("INPUT ERROR: max_results has to be in range [", max_results_min, ", ", max_results_max, "]!");
    }
    else {
      param_list["max_results"] = as.character(param_input$max_results);
    };
  };
  
  #' @description:
  #' 
  #' @*_time
  if(!is.null(param_input$start_time) & !is.null(param_input$end_time)) {
    
    if(is.na(strptime(param_input$start_time, format = "%Y-%m-%dT%H:%M:%SZ")) | is.na(strptime(param_input$end_time, format = "%Y-%m-%dT%H:%M:%SZ"))) {
      stop("INPUT ERROR: wrong format of input time!");
    }
    else {
      if(strptime(param_input$end_time, format = "%Y-%m-%dT%H:%M:%SZ") < strptime(param_input$start_time, format = "%Y-%m-%dT%H:%M:%SZ")) {
        stop("INPUT ERROR: end_time is smaller than start_time!");
      };
    };
  }
  else {
    if(!is.null(param_input$start_time) & is.null(param_input$end_time)) {
      stop("INPUT ERROR: no end_time provided!");
    };
    if(is.null(param_input$start_time) & !is.null(param_input$end_time)) {
      cat(paste("\nWarning message:\n  start_time not provided, setting default:\n\n    end_time:", gsub("T|Z", " ", param_input$end_time), "\n  start_time:", (as.POSIXct(param_input$end_time, format = "%Y-%m-%dT%H:%M:%SZ") - (60 * 60 * 24 * start_time_adjusted)), "\n\n"));
    };
    if(is.null(param_input$start_time) & is.null(param_input$end_time)) {
      cat(paste("\nWarning message:\n  start_time and end_time not provided, setting default:\n\n    end_time:", Sys.time() - 30, "\n  start_time:", (Sys.time() - 30) - (60 * 60 * 24 * start_time_adjusted), "\n\n"));
    };
  };
  
  #' @description: checking the correctness of the input *fields* per requested *object*.
  #' @see: *v2_get_object_fields*
  #' 
  #' @*.fields
  source(paste0(sub("search-tweets-r.*", "", dirname(rstudioapi::getSourceEditorContext()$path)), "search-tweets-r/v2/support.macro/v2_get_object_fields.R"));
  if(!is.null(param_input$tweet.fields)) {param_list["tweet.fields"] = v2_get_object_fields(object = "tweet.fields", fields = param_input$tweet.fields);};
  if(!is.null(param_input$user.fields )) {param_list["user.fields" ] = v2_get_object_fields(object = "user.fields",  fields = param_input$user.fields );};
  if(!is.null(param_input$media.fields)) {param_list["media.fields"] = v2_get_object_fields(object = "media.fields", fields = param_input$media.fields);};
  if(!is.null(param_input$place.fields)) {param_list["place.fields"] = v2_get_object_fields(object = "place.fields", fields = param_input$place.fields);};
  if(!is.null(param_input$poll.fields )) {param_list["poll.fields" ] = v2_get_object_fields(object = "poll.fields",  fields = param_input$poll.fields );};

}; v2_validate_request(endpoint = "/2/tweets/search/recent", max_results = 10, tweet.fields = c("all"), end_time = "2021-04-04T16:59:22Z");

