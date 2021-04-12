#' @author: Victor Plesco
#' 
#' 
#' @title supp_validate_request
#' @description Support method for validating the query parameters of a specified endpoint (given its query conditions).
#' 
#' @param v2.endpoint string \{"/2/___"\}. Specifies an endpoint whose query parameters are validated.
#' @param product_track string \{"academic" or "standard"\}. Specifies the type of developer account in possess.
#' @param ... mix (e.g. max_results = 10, query = "btc -is:retweet", ect.). Query parameters to be validated.
#' 
#' @return A list of validated query parameters.
#' 
#' @export
supp_validate_request <- function(v2.endpoint, product_track, ...) {
  
  param_input = list(...); param_conditions = supp_get_request_conditions(v2.endpoint = v2.endpoint, product_track = product_track);
  
  # query;
  if(!is.null(param_input$query)) {
    if(nchar(param_input$query) > param_conditions$length.query) {
      stop("query length {", nchar(param_input$query), "} bigger than product track permited [", param_conditions$length.query, "]!");
    } else {param_input$query = param_input$query;}; 
  };

  # *_time;
  if(!is.null(param_input$start_time)) {
    if(!is.null(param_input$end_time)) {
      ifelse(is.na(strptime(param_input$start_time, format = "%Y-%m-%dT%H:%M:%SZ")),
             stop("Wrong format of start_time [", param_input$start_time, "] should receive %Y-%m-%dT%H:%M:%SZ!"),
             ifelse(is.na(strptime(param_input$end_time, format = "%Y-%m-%dT%H:%M:%SZ")),
                    stop("Wrong format of end_time [", param_input$end_time, "] should receive %Y-%m-%dT%H:%M:%SZ!"),
                    ifelse(with(lapply(param_input[c("start_time", "end_time")], function(x) strptime(x, format = "%Y-%m-%dT%H:%M:%SZ")), start_time > end_time), 
                           stop("end_time is smaller than start_time!"), {param_input[c("start_time", "end_time")] = param_input[c("start_time", "end_time")]})));
    } else {stop("No end_time provided!");};
  }
  else {
    if(!is.null(param_input$end_time)) {
      cat(paste("\nWarning message:\n  start_time not provided, Twitter will set to default:\n\n    end_time:",
                gsub("T|Z", " ", param_input$end_time), "\n  start_time:",
                param_conditions$default.start_time(as.POSIXct(param_input$end_time, format = "%Y-%m-%dT%H:%M:%SZ")), "(default)\n\n"));
      param_input$end_time = param_input$end_time;
    }
    else {
      cat(paste("\nWarning message:\n  start_time and end_time not provided, Twitter will set to default:\n\n    end_time:",
                param_conditions$default.end_time, "(default)\n  start_time:", param_conditions$default.start_time(param_conditions$default.end_time), "(default)\n\n"));
    };
  };
  
  # max_results;
  if(!is.null(param_input$max_results)) {
    if(param_input$max_results < param_conditions$min.max_results | param_input$max_results > param_conditions$max.max_results) {
      stop("max_results {", param_input$max_results, "} must be in range [", param_conditions$min.max_results, ", ", param_conditions$max.max_results, "]!");
    } else {param_input$max_results = as.character(param_input$max_results);}; 
  };
  
  # next_token;
  if(!is.null(param_input$next_token)) {
    param_input$next_token = param_input$next_token;
  };
  
  # since_id;
  if(!is.null(param_input$since_id)) {
    param_input$since_id = param_input$since_id;
  };
  
  # until_id;
  if(!is.null(param_input$until_id)) {
    param_input$until_id = param_input$until_id;
  };
  
  # *.fields;
  if(!is.null(param_input$tweet.fields)) {param_input$tweet.fields = supp_get_object_fields(object = "tweet", fields = param_input$tweet.fields);};
  if(!is.null(param_input$user.fields))  {param_input$user.fields  = supp_get_object_fields(object = "user",  fields = param_input$user.fields);};
  if(!is.null(param_input$media.fields)) {param_input$media.fields = supp_get_object_fields(object = "media", fields = param_input$media.fields);};
  if(!is.null(param_input$place.fields)) {param_input$place.fields = supp_get_object_fields(object = "place", fields = param_input$place.fields);};
  if(!is.null(param_input$poll.fields))  {param_input$poll.fields  = supp_get_object_fields(object = "poll",  fields = param_input$poll.fields);};
  
  # expansions;
  if(!is.null(param_input$expansions)) {
    param_input$expansions = supp_get_payload_expansions(payload = param_conditions$payload, expansions = param_input$expansions);
  }; 
  
  return(param_input);
}; 