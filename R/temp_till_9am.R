# The function temp_till_9am is used to temperatures for time up to and including 9am. With the function
# temp_after_9am used for the later times. The main function extracts the mean max and min temperatures from
# the closest BOM weather station and uses the first to functions to extract the hourly temperatures.

#' @export
#' 



temp_till_9am <- function(hour,max,min){
  
  # Simple cosine temperature model
  Thr <-  -(max-min)/2*cos(hour*pi/9) + (max +min)/2
  
  return(Thr)
  
}