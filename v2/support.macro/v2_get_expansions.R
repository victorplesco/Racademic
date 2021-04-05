v2_get_expansions <- function(x, show.expansions = TRUE) {
  
  available_expansions = list (
    `author_id`                      = NULL,
    `in_reply_to_user_id`            = NULL,
    `entities.mentions.username`     = NULL,
    `referenced_tweets.id.author_id` = NULL,
    `referenced_tweets.id`           = NULL,
    `attachments.media_keys`         = NULL,
    `attachments.poll_ids`           = NULL,
    `geo.place_id`                   = NULL
  );
  
  #' @description 
  if(show.expansions == TRUE) 
  {
    for(i in 1:length(available_expansions)) 
    {
      cat(i, ":", # Index of the expansion;
          names(available_expansions[i]), # Name of the expansion;
          strrep(" ", (max(nchar(names(available_expansions))) - nchar(names(available_expansions[i])))), # padding for pretty printing;
          available_expansions[[i]], "\n") # Description of the expansion;
    };
  };
};