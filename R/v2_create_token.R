v2_create_token <- function(token = NULL) {
  
  if(is.null(token)) {
    stop("INPUT ERROR: No token provided!");
  } else {
    return(c(`Authorization` = sprintf('Bearer %s', token)));
  }
};