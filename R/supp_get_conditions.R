#' @author: Victor Plesco
#' 
#' 
#' @title supp_get_conditions
#' @description 
#' 
#' @param v2.endpoint
#' @param product_track 
#' 
#' @return
#' 
#' @export
supp_get_conditions <- function(v2.endpoint, product_track) {
  
  switch(v2.endpoint,
         
   `/2/tweets/search/all` = list (
      
         max_results_max       = 500,
         max_results_min       = 10,

         end_time_default      = Sys.time(),
         start_time_default    = {function(end_time) {end_time - (60 * 60 * 24 * 30)}},
         
         query_length          = switch(product_track, academic = 1024, standard = 512)
       ),
   
   `/2/tweets/search/recent` = list (
         max_results_max       = 100,
         max_results_min       = 10,

         end_time_default      = Sys.time(),
         start_time_default    = {function(end_time) {end_time - (60 * 60 * 24 * 7)}},
         
         query_length          = switch(product_track, academic = 1024, standard = 512)
       ),
   
   stop("The provided endpoint is wrong or is not handled!")
   );
}; 