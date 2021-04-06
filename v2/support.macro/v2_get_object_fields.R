#' @author: Victor Plesco
#' @lastupdate: 2021-04-04
#' 
#' 
#' @name: *v2_get_object_fields*
#' @description: serves as a container for the fields of the objects available for request on the new Twitter API v2. 
#' 
#' @param v2.object : string {"/2/___"}. Default=NULL. Specifies an object for which a list of fields is returned.
#' @param read.only : boolean {TRUE, FALSE}. Default=FALSE. Specifies the type of return of the function (@see @return for details).
#' @param custom.fields : vector of strings {"FIELD_1", ...}. Default=NULL. Customizes the return, or print, by subsetting the fields.
#' @param open.url : boolean {TRUE, FALSE}. Default=FALSE. If TRUE, opens the documentation page related to the @v2.object specified.
#' 
#' @return: (if @read.only is TRUE) a list of parameters of the specified @v2.object all collapsed with comma as separator (@e.g. "id,
#' text,attachments") as requested by the Twitter API v2, or will print the fields of the specified @v2.object alongside with their 
#' description (if @read.only is FALSE). If no @v2.object is specified, prints all the objects available for request. 
#' 
#' 
v2_get_object_fields <- function(v2.object = NULL, read.only = FALSE, custom.fields = NULL, open.url = FALSE) {
  
  #' @description: building a container for each object with its respective fields. Alongside each field there is a short description 
  #' assessing the information it represents.
  #' [TO_BE_UPDATED: Add descriptions.]
  #' 
  #' `tweet.fields`
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
  #' `user.fields`
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
  #' `media.fields`
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
  #' `poll.fields`
  poll.fields = list (
    `id`                  = "DESCRIPTION_NOT_AVAILABLE",
    `options`             = "DESCRIPTION_NOT_AVAILABLE",
    `duration_minutes`    = "DESCRIPTION_NOT_AVAILABLE",
    `end_datetime`        = "DESCRIPTION_NOT_AVAILABLE",
    `voting_status`       = "DESCRIPTION_NOT_AVAILABLE"
  );  
  #' 
  #' 
  #' `place.fields`
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
  
  #' @description: aggregating the previously created containers in a list with sub-lists format and adding metadata (e.g. url).
  #' 
  #' `objects_list`
  v2.objects_list = list (
    `tweet.fields` = list(`fields` = tweet.fields, `url` = "https://developer.twitter.com/en/docs/twitter-api/data-dictionary/object-model/tweet"),
    `user.fields`  = list(`fields` = user.fields,  `url` = "https://developer.twitter.com/en/docs/twitter-api/data-dictionary/object-model/user" ),
    `media.fields` = list(`fields` = media.fields, `url` = "https://developer.twitter.com/en/docs/twitter-api/data-dictionary/object-model/media"),
    `poll.fields`  = list(`fields` = poll.fields,  `url` = "https://developer.twitter.com/en/docs/twitter-api/data-dictionary/object-model/poll" ),
    `place.fields` = list(`fields` = place.fields, `url` = "https://developer.twitter.com/en/docs/twitter-api/data-dictionary/object-model/place")
  );
  
  #' @description: building dependencies of @open.url and @read.only with @v2.object. If @v2.object is NULL, prints the objects
  #' available for request, else, depending on the value of @read.only, will return (if FALSE) or print (if TRUE) the fields of the 
  #' specified @v2.object. It is possible to customize the return, or print, by providing a set of fields through @custom.fields. Also if 
  #' @open.url is TRUE, the `browseURL()` function will be activated and the documentation page related to the provided @v2.object will 
  #' be opened in your browser (@see `browseURL()` for more details).
  #' 
  #' 
  if(!is.null(v2.object)) { # if an object is provided;
    
    if(length(which(custom.fields %in% names(v2.objects_list[v2.object][[1]]["fields"][[1]]) == TRUE)) != length(custom.fields)) { # Checking errors in custom.fields;
      stop("INPUT ERROR: the following fields to not belong to ", v2.object, 
           " [", paste0(custom.fields[which(custom.fields %in% names(v2.objects_list[v2.object][[1]]["fields"][[1]]) == FALSE)], collapse = ", "), "] ");
    };
    
    if(v2.object %in% names(v2.objects_list) == FALSE) { # Checking errors in object;
      stop("INPUT ERROR: the object provided is wrong!");
    };
    
    if(open.url == TRUE) {
      browseURL(v2.objects_list[v2.object][[1]]["url"][[1]]);
    };
    
    if(read.only == FALSE) {
      if(!is.null(custom.fields)) { # if custom fields provided (RETURN);
        return(paste(names(v2.objects_list[v2.object][[1]]["fields"][[1]][custom.fields]), collapse = ","));
      }
      else { # if no custom fields provided (RETURN);
        return(paste(names(v2.objects_list[v2.object][[1]]["fields"][[1]]), collapse = ","));
      };
    } 
    else {
      if(!is.null(custom.fields)) { # if custom fields provided (PRINT);
        knitr::kable(sapply(v2.objects_list[v2.object][[1]]["fields"][[1]][custom.fields], "[[", 1), 
                     caption = v2.object, col.names = c("Description"), align = c("c", "c"));
      }
      else { # if no custom fields provided (PRINT);
        knitr::kable(sapply(v2.objects_list[v2.object][[1]]["fields"][[1]], "[[", 1), 
                     caption = v2.object, col.names = c("Description"), align = c("c", "c"));
      };
    };
    
  } 
  else { # if no object is provided;
    
    if(read.only == TRUE | !is.null(custom.fields) | open.url == TRUE) {
      stop("INPUT ERROR: no object provided!");
    }
    else {
      knitr::kable(names(v2.objects_list), col.names = "Available Objects", align = "c");
    }
  };
}; # v2_get_object_fields(v2.object = "tweet.fields", read.only = TRUE, custom.fields = c("id", "text", "attachments"), open.url = FALSE);
