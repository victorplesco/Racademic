#' @author: Victor Plesco
#' 
#' 
#' @title supp_get_request_conditions
#' @description Getter for the query conditions of the combination of an endpoint and a product track.
#' 
#' @param v2.endpoint string \{"/2/___"\}. Specifies an endpoint for which a list of query conditions is returned.
#' @param product_track string \{"academic" or "standard"\}. Specifies the type of developer account in possess to which specific
#' query conditions apply. 
#' 
#' @return A list of query conditions (e.g. max.max_results = 500, min.max_results = 10, ect.).
#' 
#' @export
supp_get_request_conditions <- function(v2.endpoint, product_track) {
  
  switch(v2.endpoint,
   
   # Search Full-archive;
   `/2/tweets/search/all` = list (
         max.max_results       = 500,
         min.max_results       = 10,
         default.end_time      = Sys.time() - 30, # System time minus 30 seconds;
         default.start_time    = {function(end_time) {end_time - (60 * 60 * 24 * 30)}}, # end_time minus 30 days;
         length.query          = switch(product_track, academic = 1024, standard = 512, 
                                        stop("The provided product track is wrong or is not handled by racademic!")),
         payload               = "tweet"
       ),
   
   # Search Recent;
   `/2/tweets/search/recent` = list (
         max.max_results       = 100,
         min.max_results       = 10,
         default.end_time      = Sys.time() - 30, # System time minus 30 seconds;
         default.start_time    = {function(end_time) {end_time - (60 * 60 * 24 * 7)}}, # end_time minus 7 days;
         length.query          = switch(product_track, academic = 1024, standard = 512,
                                        stop("The provided product track is wrong or is not handled by racademic!")),
         payload               = "tweet"
       ),
   
   stop("The provided endpoint is wrong or is not handled by racademic!")
   );
}; 