#' @author: Victor Plesco
#' @lastupdate: 2021-04-05
#' 
#' 
#' @name: *v2_get_object_fields*
#' @description: this function serves as a descriptive function of the various objects available for request on the new Twitter API v2 
#' and their related fields and returns, on request, all or a subset of the fields of an object all collapsed together with a comma as 
#' separator (@e.g. "text,attachments"), as imposed by the conditions of the query to the API.
#' 
#'    @usage [DESCRIPTIVE] A print can be requested by specifying *show.objects* or *show.fields* as TRUE. The first will print all the
#'    available objects and the second will print all the available fields for a specified *object*. 
#'    
#'    @usage [QUERY-BUILDING] A return can be requested by providing to *fields* a list of necessary to you fields related to the 
#'    specified *object*. Upon request to return fields, the function will check if all of the provided *fields* do actually belong to 
#'    the specified *object* and after will return a single string of fields all collapsed with comma as separator. 
#' 
#' @param show.objects : boolean {TRUE, FALSE}. [Standalone]. Requires a print of all available to request objects. The parameter is a 
#' standalone and shouldn't be used together with other parameters of the function. Mainly it has to be used if you don't know which 
#' are the objects a query request to Twitter API v2 can return you. 
#' 
#' @param object : string {"___.fields"}. [Conjunction required]. Container for an object to be used in concordance with *show.fields*
#' or *fields* parameters. The container accepts only one object per call of the function. Ideally to be used after an initial call 
#' with show.objects = TRUE.
#'  
#' @param show.fields : boolean {TRUE, FALSE}. [Conjunction required]. Requires a print of the fields of a specified *object* alongside 
#' with their description. The idea here is to use this print in order to decide which fields to require to Twitter upon a search request.
#' 
#' @param fields : vector of strings {"FIELD_1", ...} or string {"all"}. [Conjunction required]. If *all* is used as input will require 
#' a return of all the fields related to a specified *object*, otherwise just the specified ones. This parameter has to be combined with 
#' *object*, since the function first maps the input *fields* with the correct ones already stored within the function.
#' 
#' @param open.url : boolean {TRUE, FALSE}. [Conjunction required]. Opens the documentation page related to the specified *object*.
#' 
#' 
v2_get_object_fields <- function(show.objects = FALSE, object = NULL, show.fields = FALSE, fields = NULL, open.url = FALSE) {
  
  #' @description: building containers for each object with its respective fields. The fields are taken from the Twitter documentation 
  #' page "Data dictionary" and will serve as the gold truth for detecting errors within the specified *fields* parameter. Alongside each 
  #' field there is a short description assessing the information it represents. 
  #' [TO_BE_UPDATED: Add descriptions to fields of objects.]
  #' 
  #' @tweet.fields
  tweet.fields = list (
    `id`                  = "DESCRIPTION_NOT_AVAILABLE",
    `text`                = "DESCRIPTION_NOT_AVAILABLE",
    `attachments`         = "DESCRIPTION_NOT_AVAILABLE",
    `author_id`           = "DESCRIPTION_NOT_AVAILABLE",
    `context_annotations` = "DESCRIPTION_NOT_AVAILABLE",
    `conversation_id`     = "DESCRIPTION_NOT_AVAILABLE",
    `created_at`          = "DESCRIPTION_NOT_AVAILABLE",
    `entities`            = "DESCRIPTION_NOT_AVAILABLE",
    `geo`                 = "DESCRIPTION_NOT_AVAILABLE",
    `in_reply_to_user_id` = "DESCRIPTION_NOT_AVAILABLE",
    `lang`                = "DESCRIPTION_NOT_AVAILABLE",
    `non_public_metrics`  = "DESCRIPTION_NOT_AVAILABLE",
    `organic_metrics`     = "DESCRIPTION_NOT_AVAILABLE",
    `possiby_sensitive`   = "DESCRIPTION_NOT_AVAILABLE",
    `promoted_metrics`    = "DESCRIPTION_NOT_AVAILABLE",
    `public_metrics`      = "DESCRIPTION_NOT_AVAILABLE",
    `referenced_tweets`   = "DESCRIPTION_NOT_AVAILABLE",
    `reply_settings`      = "DESCRIPTION_NOT_AVAILABLE",
    `source`              = "DESCRIPTION_NOT_AVAILABLE",
    `withheld`            = "DESCRIPTION_NOT_AVAILABLE"
  );  
  #'
  #' 
  #' @user.fields
  user.fields = list (
    `id`                  = "DESCRIPTION_NOT_AVAILABLE",
    `name`                = "DESCRIPTION_NOT_AVAILABLE",
    `username`            = "DESCRIPTION_NOT_AVAILABLE",
    `created_at`          = "DESCRIPTION_NOT_AVAILABLE",
    `description`         = "DESCRIPTION_NOT_AVAILABLE",
    `entities`            = "DESCRIPTION_NOT_AVAILABLE",
    `created_at`          = "DESCRIPTION_NOT_AVAILABLE",
    `entities`            = "DESCRIPTION_NOT_AVAILABLE",
    `location`            = "DESCRIPTION_NOT_AVAILABLE",
    `pinned_tweet_id`     = "DESCRIPTION_NOT_AVAILABLE",
    `profile_image_url`   = "DESCRIPTION_NOT_AVAILABLE",
    `protected`           = "DESCRIPTION_NOT_AVAILABLE",
    `public_metrics`      = "DESCRIPTION_NOT_AVAILABLE",
    `url`                 = "DESCRIPTION_NOT_AVAILABLE",
    `verified`            = "DESCRIPTION_NOT_AVAILABLE",
    `withheld`            = "DESCRIPTION_NOT_AVAILABLE"
  );  
  #'
  #'
  #' @media.fields
  media.fields = list (
    `media_key`           = "DESCRIPTION_NOT_AVAILABLE",
    `type`                = "DESCRIPTION_NOT_AVAILABLE",
    `duration_ms`         = "DESCRIPTION_NOT_AVAILABLE",
    `height`              = "DESCRIPTION_NOT_AVAILABLE",
    `non_public_metrics`  = "DESCRIPTION_NOT_AVAILABLE",
    `organic_metrics`     = "DESCRIPTION_NOT_AVAILABLE",
    `preview_image_url`   = "DESCRIPTION_NOT_AVAILABLE",
    `promoted_metrics`    = "DESCRIPTION_NOT_AVAILABLE",
    `public_metrics`      = "DESCRIPTION_NOT_AVAILABLE",
    `width`               = "DESCRIPTION_NOT_AVAILABLE"
  );  
  #'
  #'
  #' @poll.fields
  poll.fields = list (
    `id`                  = "DESCRIPTION_NOT_AVAILABLE",
    `options`             = "DESCRIPTION_NOT_AVAILABLE",
    `duration_minutes`    = "DESCRIPTION_NOT_AVAILABLE",
    `end_datetime`        = "DESCRIPTION_NOT_AVAILABLE",
    `voting_status`       = "DESCRIPTION_NOT_AVAILABLE"
  );  
  #' 
  #' 
  #' @place.fields
  place.fields = list (
    `full_name`           = "DESCRIPTION_NOT_AVAILABLE",
    `id`                  = "DESCRIPTION_NOT_AVAILABLE",
    `contained_within`    = "DESCRIPTION_NOT_AVAILABLE",
    `country`             = "DESCRIPTION_NOT_AVAILABLE",
    `country_code`        = "DESCRIPTION_NOT_AVAILABLE",
    `geo`                 = "DESCRIPTION_NOT_AVAILABLE",
    `name`                = "DESCRIPTION_NOT_AVAILABLE",
    `place_type`          = "DESCRIPTION_NOT_AVAILABLE"
  );  

  
  #' @description:
  #' 
  #' @objects_list
  objects_list = list (
    `tweet.fields` = list(`fields` = tweet.fields, `url` = "https://developer.twitter.com/en/docs/twitter-api/data-dictionary/object-model/tweet"),
    `user.fields`  = list(`fields` = user.fields,  `url` = "https://developer.twitter.com/en/docs/twitter-api/data-dictionary/object-model/user" ),
    `media.fields` = list(`fields` = media.fields, `url` = "https://developer.twitter.com/en/docs/twitter-api/data-dictionary/object-model/media"),
    `poll.fields`  = list(`fields` = poll.fields,  `url` = "https://developer.twitter.com/en/docs/twitter-api/data-dictionary/object-model/poll" ),
    `place.fields` = list(`fields` = place.fields, `url` = "https://developer.twitter.com/en/docs/twitter-api/data-dictionary/object-model/place")
  );
  
  
  #' @description: if *show.objects* is set to TRUE, iterates through *objects_list* and prints the name of each object alongside with 
  #' an integer as index. 
  #' 
  #' @show.objects
  if(show.objects == TRUE)
  {
    cat("\n Objects:\n", strrep("*", max(nchar(names(objects_list))) + 5), "\n");
    for(i in 1:length(objects_list))
    {
      cat(" ", i, ":", # Index of the object;
          names(objects_list[i]), # Name of the object;
          "\n");
    };
  };
  
  
  #' @description: if *object* is provided, a series of outputs can be requested.
  #' [TO_BE_UPDATED: Modify the function in order to accept the index of an object as object input.]
  #' 
  #' @object
  if(!is.null(object)) 
  {

    
    #' @description: building dependencies of *object* with other parameters. If object is != NULL, a conjunction among *open.url*, 
    #' *show.fields* or *fields* is required, otherwise the function will return no output.
    #' 
    #' 
    if(open.url == FALSE & show.fields == FALSE & is.null(fields)) {
      stop("INPUT ERROR: no conjunction for object provided!");
    };
    
    
    #' @description: if *open.url* is set to TRUE, activates the `browseURL()` function and opens the url of the documentation page of 
    #' the specified *object*. Mainly this is done to make it easier for the user to reach the original descriptions of each *object* 
    #' and its related fields.
    #' 
    #' @open.url
    if(open.url == TRUE) {
      browseURL(objects_list[object][[1]]["url"][[1]]);
    };

    
    #' @description: if *show.fields* is set to TRUE, iterates through the container of a specified *object* and prints its fields 
    #' alongside with a short description.
    #' 
    #' @show.fields
    if(show.fields == TRUE) {
      cat("\n", paste0(object, ":"), "\n", strrep("*", (max(nchar(names(objects_list[object][[1]]["fields"][[1]])))) + 5), "\n");
      for(i in 1:length(objects_list[object][[1]]["fields"][[1]])) 
      {
        cat(if(i < 10) {" "},
            i, ":", # Index of the field;
            names(objects_list[object][[1]]["fields"][[1]])[i], # Name of the field;
            
            # Padding for pretty printing;
            strrep(" ", (max(nchar(names(objects_list[object][[1]]["fields"][[1]]))) - max(nchar(names(objects_list[object][[1]]["fields"][[1]][i]))))),
            as.character(objects_list[object][[1]]["fields"][[1]][i]), "\n") # Description of the field;
      };
    };

    
    #' @description: if *fields* is provided, there are two main cases. If *fields* has "all" as input, a collapsed string composed of 
    #' the *fields*, each separated by a comma from the subsequent, of a specified  *object* is returned. If *fields* has a vector of 
    #' fields as input, it is firstly mapped to the fields present in *objects_list*, and if there is a match for all the elements the 
    #' vector is collapsed, as before, to a string having its elements separated by a comma. This process is done in order to prevent 
    #' bad requests to Twitter's API due to wrong built queries.
    #' 
    #' @fields
    if(!is.null(fields)) {
      
      # if "all" as input;
      if(fields == "all") { 
        return(paste(names(objects_list[object][[1]]["fields"][[1]]), collapse = ","));
      }
      
      # if vector as input;
      else {
        
        # if vector as input and perfect match;
        if(length(which(fields %in% names(objects_list[object][[1]]["fields"][[1]]) == TRUE)) == length(fields)) {
          return(paste(fields, collapse = ","));
        }
        
        # if vector as input and no perfect match;
        else { 
          error_list = c(); idx = 1;
          for(i in fields[which(fields %in% names(objects_list[object][[1]]["fields"][[1]]) == FALSE)]) 
          {error_list[idx] = paste0(" \"", i, "\" "); idx = idx + 1;};
          stop("INPUT ERROR: the following objects do not belong to ", object, " [", error_list, "] ");
        };
      };
    };
  }
  else {
    #' #' @description: building dependencies of *open.url*, *show.fields* and *fields* with *object* parameter. If one of the latter 
    #' is set to TRUE or is provided, an *object* should be provided as well. Otherwise the function will throw an error.
    #' 
    #' 
    if(open.url == TRUE | show.fields == TRUE | !is.null(fields)) {
      stop("INPUT ERROR: no object provided!");
    };
  };
}; # v2_get_object_fields(show.objects = FALSE, object = NULL, show.fields = TRUE, fields = NULL, open.url = FALSE);
