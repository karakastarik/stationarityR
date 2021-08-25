#Take difference of the model data

df_diff <- function(model,diff_value){
  df <-data.frame(diff(as.matrix(model$model),differences = diff_value))
  print(paste0("Unit root tests results prepared for difference: ",diff_value))
  return(df)
}
