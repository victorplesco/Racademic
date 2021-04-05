#' @author: Victor Plesco
#' @lastupdate: 2021-04-04
#' 
#' 
#' @name: *v2_validate_request*
#' @description: validates the input parameters necessary for the query;
#' @param endpoint : @see v2_get_query_parameters for the available Twitter API v2 endpoints;
#' @param ...      : @see v2_get_query_parameters for the list of parameters per endpoint;
#' @return: list of parameters in the format requested by Twitter API v2;
#' 
#' 
v2_validate_request <- function(endpoint = NULL, product_track = NULL, ...) {
  
  param_input = list(...); # list of input parameters. "..." are used since for each endpoint there are different parameters;
  
  #' @section:
  #' @description: in order to validate the request we first have to understand which are the parameters accepted by a specific endpoint.
  #' Therefore as a first step we call *v2_get_endpoint_parameters* and provide it the *endpoint* the user have specified and set *return*
  #' as TRUE in order to gather a list of accepted parameters by that endpoint that we save in `param_list`.
  #' @see: *v2_get_endpoint_parameters*
  #' 
  #' 
  #' @param_list
  source(paste0(sub("search-tweets-r.*", "", dirname(rstudioapi::getSourceEditorContext()$path)), "search-tweets-r/v2/support.macro/v2_get_query_parameters.R"));
  param_list = v2_get_endpoint_parameters(show.endpoints = FALSE, endpoint = endpoint, show.parameters = FALSE, return = TRUE, open.url = FALSE);
  
  #' [###################################################################################################################] '#
  #' [## From here on going,  one  should create a function assessing limits per product track and split the  validate ##] '#
  #' [## function into ad-hoc macro functions for each product track, in turn split into micro functions per endpoint. ##] '#
  #' [## The below code is valid only for the Academic Research product  track  and  is able to validate the query for ##] '#
  #' [## only the "search recent" and "search full-archive" endpoints.                                                 ##] '#
  #' [###################################################################################################################] '#

  #' Tested for input correctness:
  #' @query:                       [NO]
  #' @start_time:                  [YES]   
  #' @end_time:                    [YES]
  #' @max_results:                 [YES]
  #' @next_token:                  [NO]
  #' @since_id:                    [NO]
  #' @until_id:                    [NO]
  #' @expansions:                  [NO]
  #' @tweet.fields:                [YES]
  #' @user.fields:                 [YES]      
  #' @media.fields:                [YES]      
  #' @place.fields:                [YES]      
  #' @poll.fields:                 [YES]      
  
  #' @max_results
  #' @description: checking if max_results belong to the interval [10, 500];
  #' 
  #' 
  if(param_input$max_results < 10 | param_input$max_results > 500) {
    stop("INPUT ERROR: max_results has to be in range [10, 500]!");
  }
  else {
    param_list["max_results"] = as.character(param_input$max_results);
  };
  
  #' @section: start_time validation
  #' @description: checking the correctness of the format [YYYY-MM-DDTHH:mm:ssZ] and the dependency with end_time;
  if(!is.null(param_input$start_time)) {
    if(is.na(strptime(param_input$start_time, format = "%Y-%m-%dT%H:%M:%SZ"))) {
      stop("INPUT ERROR: wrong format of start_time!")
    }
    else {
      if(is.null(param_input$end_time)) {
        stop("INPUT ERROR: no end_time provided!")
      }
      else {
        if(is.na(strptime(param_input$end_time, format = "%Y-%m-%dT%H:%M:%SZ"))) {
          stop("INPUT ERROR: wrong format of end_time!")
        }
        else {
          if(strptime(param_input$end_time, format = "%Y-%m-%dT%H:%M:%SZ") < strptime(param_input$start_time, format = "%Y-%m-%dT%H:%M:%SZ")) {
            stop("INPUT ERROR: start_time is bigger than end_time!")
          }
          else {
            param_list["start_time"] = param_input$start_time;
          };
        }; 
      };
    };
  };

  #' @section: end_time validation
  #' @description: checking the correctness of the format [YYYY-MM-DDTHH:mm:ssZ] and the dependency with start_time;
  if(!is.null(param_input$end_time)) {
    if(is.na(strptime(param_input$end_time, format = "%Y-%m-%dT%H:%M:%SZ"))) {
      stop("INPUT ERROR: wrong format of end_time!")
    }
    else {
      if(strptime(param_input$end_time, format = "%Y-%m-%dT%H:%M:%SZ") < strptime(param_input$start_time, format = "%Y-%m-%dT%H:%M:%SZ")) {
        stop("INPUT ERROR: end_time is smaller than start_time!")
      }
      else {
        param_list["end_time"] = param_input$end_time;
      };
    };
  }
  else { 
    if(is.null(param_input$start_time)) {
      cat(paste("\nWarning message:\n  end_time and start_time not provided. Setting default:\n\n    end_time:", Sys.time() - 30, "\n  start_time:", (Sys.time() - 30) - (60 * 60 * 24 * 30), "\n\n"));
    }
    else {
      cat(paste("\nWarning message:\n  end_time not provided. Setting default:\n\n    end_time:", Sys.time() - 30, "\n  start_time:", param_input$start_time, "\n\n"));
    }
  };

  #' @section: *.fields validation
  #' @description: checking the correctness of the fields per requested object;
  #' @see: *v2_get_object_fields*
  #' 
  #'  
  source(paste0(sub("search-tweets-r.*", "", dirname(rstudioapi::getSourceEditorContext()$path)), "search-tweets-r/v2/support.macro/v2_get_object_fields.R"));
  param_list["tweet.fields"] = v2_get_object_fields(object = "tweet.fields", fields = param_input$tweet.fields);
  param_list["user.fields" ] = v2_get_object_fields(object = "user.fields",  fields = param_input$user.fields );
  param_list["media.fields"] = v2_get_object_fields(object = "media.fields", fields = param_input$media.fields);
  param_list["place.fields"] = v2_get_object_fields(object = "place.fields", fields = param_input$place.fields);
  param_list["poll.fields" ] = v2_get_object_fields(object = "poll.fields",  fields = param_input$poll.fields );
  
  #' @section: return
  #' @description: if no errors encountered returns the list of parameters filled with the specified ones. Parameters who weren't specified are returned as NULL;
  # return(param_list);  
}; v2_validate_request(endpoint = "/2/tweets/search/all", max_results = 10, tweet.fields = c("all"), end_time = "2021-04-04T16:59:22Z");
