#' @author: Victor Plesco
#' 
#' 
#' @title v2_create_bearer_token
#' @description Creates a bearer token object.
#' 
#' @param v2.bearer_token string. Specifies a bearer token.
#' 
#' @return A bearer token object.
#' 
#' @export
v2_create_bearer_token <- function(v2.bearer_token) {
  return(c(`Authorization` = sprintf('Bearer %s', v2.bearer_token)));
};