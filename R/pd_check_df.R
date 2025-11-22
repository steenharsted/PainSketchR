pd_check_df <- function(df) {
  if (!is.data.frame(df)) { return("Is not a data frame")}
  if (nrow(df)<1) { return("Data frame empty")}

  # df should consist of:
  #   - id -- a string
  #   - stroke - a string
  #   - x - a numeric or integer
  #   - y - a numeric or integer

  #if (!all(c("x","y","id","stroke") %in% names(df))) {return("Missing columns (id,stroke,x,y")}
  #if (!all(lapply(df, typeof) %in% c("integer", "double"))) {
  #  return("Wrong data type of input. id, stroke, x and y should be numeric or integer.")
  #}

  ## How to handle stroke? ..as a factor or as an integer (index number)
  return("ok")
}