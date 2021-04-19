#' @author Victor Plesco
#' 
#' 
#' @title supp_eval.ratelimit
#' @description Manages the rate limits for the Twitter API v2 search endpoints.
#' 
#' @param request.time object of class POSIXlt (e.g. "2021-04-11 18:54:23 UTC"). Time of request.
#' @param request.remaining integer. Number of requests left for the 15-minute window.
#' @param request.reset integer. Remaining window before the rate limit resets (in UTC epoch seconds).
#' 
#' @export
supp_eval.ratelimit <- function(request.time, request.remaining, request.reset) {
  
  if(request.remaining == 0) { # requests/15-min limits;
    
    if(as.POSIXlt(Sys.time(), tz = "UTC") < as.POSIXlt(request.reset, origin = "1970-01-01", tz = "UTC")){
      sleep.request.reset = as.numeric(as.POSIXlt(request.reset, origin = "1970-01-01", tz = "UTC") - as.POSIXlt(Sys.time(), tz = "UTC"));
      cat("\nWaiting", sleep.request.reset, "minutes for rate limit reset!"); 
      Sys.sleep(sleep.request.reset * 60);
    };
    
  } else { # request/s limits;
    Sys.sleep(1);
  };
};