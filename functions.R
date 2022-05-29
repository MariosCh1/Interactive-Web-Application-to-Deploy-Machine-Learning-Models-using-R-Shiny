##function for removing the column
removecolumn <- function(df, nameofthecolumn){
  df[ , -which(names(df) %in% nameofthecolumn)]
}

