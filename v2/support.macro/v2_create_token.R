v2_create_token <- function(token = NULL) {
  return(c(`Authorization` = sprintf('Bearer %s', token)));
};