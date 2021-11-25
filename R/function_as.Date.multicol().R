# date conversion function (columns with `date` or `alive` in their header are 
# converted to the `%Y-%m-%d` format)
as.Date.multicol <- function(df){
  if(sum(grepl(paste(c("date", "alive", "manip"), collapse = "|"), names(df))) > 1){
    
    df[, which(grepl(paste(c("date", "alive", "manip"), collapse = "|"), names(df)))] <-
      lapply(df[, which(grepl(paste(c("date", "alive", "manip"), collapse = "|"), names(df)))],
             function(x) as.Date(x, origin = "1970-01-01"))
    
  }
  else{
    
    df[, which(grepl(paste(c("date", "alive", "manip"), collapse = "|"), names(df)))] <- 
      as.Date(df[, which(grepl(paste(c("date", "alive", "manip"), collapse = "|"), 
                               names(df)))], 
              origin = "1970-01-01")
    
  }
  
  return(df)
}