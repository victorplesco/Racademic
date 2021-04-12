#' @author: Victor Plesco
#' 
#' 
#' @title supp_validate_limitrate
#' @description Support method for validating rate limits of an API request. Conceptually, the function must precede a second request 
#' 
#' @param round.time POSIXlt (e.g. "2021-04-11 18:54:23 UTC"). Specifies the time of request.
#' @param limit.remaining integer. Specifies the number of requests left for the 15-minute window.
#' @param limit.reset integer (UTC epoch seconds). Specifies the remaining window before the rate limit resets.
#' 
#' @export
supp_validate_limitrate <- function(round.time, limit.remaining, limit.reset) {
  
  if(limit.remaining == 0) { # requests/15-min limits;
    
    if(as.POSIXlt(Sys.time(), tz = "UTC") < as.POSIXlt(limit.reset, origin = "1970-01-01")){
      sleep.limit.reset = as.numeric(as.POSIXlt(limit.reset, origin = "1970-01-01") - as.POSIXlt(Sys.time(), tz = "UTC"));
      cat("\nWaiting", sleep.limit.reset, " minutes for rate limit reset!"); Sys.sleep(sleep.limit.reset);
    };
    
  } else { # request/s limits;
    
    if((round.time + 1) - as.numeric(as.POSIXlt(Sys.time(), tz = "UTC")) < 1) {
      # cat("\nSleeping for ", (round.time + 1) - as.numeric(as.POSIXlt(Sys.time(), tz = "UTC")));
      Sys.sleep((round.time + 1) - as.numeric(as.POSIXlt(Sys.time(), tz = "UTC")));};
  };
};