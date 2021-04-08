#' @author Victor Plesco
#' 
#' 
#' @title supp_get_object_fields
#' @description Getter for the fields of the objects available for request on the new Twitter API v2. 
#' 
#' @param v2.object string ("/2/___"). Specifies an object for which a list of fields is returned.
#' @param fields vector of strings ("FIELD_1", ...). If specified, subsets the returned fields (accepts "all"). 
#' 
#' @return A list of fields for the specified v2.object all collapsed with comma as separator (e.g. "id,text,attachments").
#' 
#' @export
supp_get_object_fields <- function(v2.object, fields) {
  
  v2.objects_switch = switch(v2.object,
    
     tweet.fields  = c(
      "id"                  ,
      "text"                ,
      "attachments"         ,
      "author_id"           ,
      "context_annotations" ,
      "conversation_id"     ,
      "created_at"          ,
      "entities"            ,
      "geo"                 ,
      "in_reply_to_user_id" ,
      "lang"                ,
      "non_public_metrics"  ,
      "organic_metrics"     ,
      "possiby_sensitive"   ,
      "promoted_metrics"    ,
      "public_metrics"      ,
      "referenced_tweets"   ,
      "reply_settings"      ,
      "source"              ,
      "withheld"            
    ),
    
     user.fields  = c(
      "id"                  ,
      "name"                ,
      "username"            ,
      "created_at"          ,
      "description"         ,
      "entities"            ,
      "created_at"          ,
      "entities"            ,
      "location"            ,
      "pinned_tweet_id"     ,
      "profile_image_url"   ,
      "protected"           ,
      "public_metrics"      ,
      "url"                 ,
      "verified"            ,
      "withheld"            
    ),
    
     media.fields  = c(
      "media_key"           ,
      "type"                ,
      "duration_ms"         ,
      "height"              ,
      "non_public_metrics"  ,
      "organic_metrics"     ,
      "preview_image_url"   ,
      "promoted_metrics"    ,
      "public_metrics"      ,
      "width"               
    ),
    
     poll.fields  = c(
      "id"                  ,
      "options"             ,
      "duration_minutes"    ,
      "end_datetime"        ,
      "voting_status"       
    ),
    
     place.fields  = c(
      "full_name"           ,
      "id"                  ,
      "contained_within"    ,
      "country"             ,
      "country_code"        ,
      "geo"                 ,
      "name"                ,
      "place_type"          
    ),
    
    stop("The provided object is wrong or is not handled!")
  );  
  
  if(any(fields == "all")) {
    return(paste(v2.objects_switch, collapse = ","));
  } 
  else {
    if(anyNA(match(fields, v2.objects_switch))) { # Checking errors in fields;
      stop("The following fields do not belong to ", v2.object, 
           " [", paste0(fields[which(fields %in% v2.objects_switch == FALSE)], collapse = ", "), "] ");
    } else {return(paste(fields, collapse = ","));};
  };
};