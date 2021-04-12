#' @author: Victor Plesco
#' 
#' 
#' @title supp_print_summary
#' @description 
#' 
#' @param process.data 
#' @param twitter.chunk 
#' 
#' @return
#' 
#' @export
supp_print_summary <- function(process.data, twitter.chunk) {
 
  # requests;
  process.data$requests = process.data$requests + 1; 
  
  # daily_counts;
  counts = as.list(table(as.character(strptime(lapply(twitter.chunk$data, "[[", "created_at"), format = "%Y-%m-%d"))));
  if(all(names(counts) %in% names(process.data$daily_counts))) {
    
    process.data$daily_counts[which(names(process.data$daily_counts) %in% names(counts))] = 
      mapply("+", process.data$daily_counts[names(process.data$daily_counts) %in% names(counts)], counts);
  } else {
    
    process.data$daily_counts = append(process.data$daily_counts, counts[setdiff(names(counts), names(process.data$daily_counts))], 
      after = length(process.data$daily_counts));
  };
  
  # next_token;
  process.data$next_token = ifelse(is.null(twitter.chunk$meta$next_token), NULL, twitter.chunk$meta$next_token);
  cat("\n Requests:", as.character(process.data$requests),
      "\n Current Token:", as.character(process.data$current_token),
      "\n Next Token:", as.character(process.data$next_token),
      "\n\n Date", "        Counts");
  for(i in 1:length(process.data$daily_counts)) 
  {cat("\n", names(process.data$daily_counts[i]), " ", as.character(process.data$daily_counts[i]))};
  cat("\n\n");  
  return(process.data);
};