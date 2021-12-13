# column structure functions
levels_check <- function(df, col_name)
{
  if(sum(grepl(col_name, names(df))) > 0){
    x <- unlist(df[which(grepl(col_name, names(df)))])
    levels(as.factor(x))
  }
  else{
    print("Dataframe doesn't have this column")
  }
}