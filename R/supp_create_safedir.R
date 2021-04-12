#' @author: Victor Plesco
#' 
#' 
#' @title supp_create_safedir
#' @description Support method for safeguarding in process downloaded data from possible crashes of the program. Given a path to an 
#' existing directory a "/data_<Sys.Date>_<Sys.Time>" folder is created with two sub-folders: "/content", aiming to store each response's 
#' parsed data in a .RDS file, and "/metadata", aiming to store process summary metadata (e.g. next_token, daily_counts, ect.).
#' 
#' @param safe.dir string (e.g. "~/home/..."). Specifies path to a directory.
#' 
#' @return Path to the newly created directory "/data__<Sys.Date>_<Sys.Time>".
#' 
#' @export
supp_create_safedir <- function(safe.dir) {
  
  data.dir = paste0("racademic_data_", format(Sys.Date(), format = "%y%m%d"), "_", format(Sys.time(), format = "%H%M%S")); # Create data folder;
  if(is.null(safe.dir)) { # If no safe.dir specified, use racademic's folder as base;
    
    safe.dir = paste0(sub("racademic.*", "", dirname(rstudioapi::getSourceEditorContext()$path)), "racademic/");
    while(data.dir %in% list.files(safe.dir)) { # Ensures no conflicts among directory names;
      data.dir = paste0("data_", format(Sys.Date(), format = "%y%m%d"), "_", format(Sys.time(), format = "%H%M%S")); Sys.sleep(0.1);
    }; dir.create(paste0(safe.dir, data.dir)); dir.create(paste0(safe.dir, data.dir, "/content/")); dir.create(paste0(safe.dir, data.dir, "/metadata/"));
    
  } else {
    
    if(dir.exists(safe.dir)) { 
      safe.dir = ifelse(identical(substring(safe.dir, nchar(safe.dir)), "/"), safe.dir, paste0(safe.dir, "/"));
      while(data.dir %in% list.files(safe.dir)) { # Ensures no conflicts among directory names;
        data.dir = paste0("racademic_data_", format(Sys.Date(), format = "%y%m%d"), "_", format(Sys.time(), format = "%H%M%S")); Sys.sleep(0.1);
      }; dir.create(paste0(safe.dir, data.dir)); dir.create(paste0(safe.dir, data.dir, "/content/")); dir.create(paste0(safe.dir, data.dir, "/metadata/"));
    } else {stop("The specified safe.dir doesn't exist. Provide an existing one!");};
    
  };
  return(paste0(safe.dir, data.dir, "/"));
};