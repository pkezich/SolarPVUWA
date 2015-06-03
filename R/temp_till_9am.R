# The function temp_till_9am is used to temperatures for time up to and including 9am. With the function
# temp_after_9am used for the later times. The main function extracts the mean max and min temperatures from
# the closest BOM weather station and uses the first to functions to extract the hourly temperatures.

#' 
#' 
#' @name temp_till_9am
#' @title temp_till_9am
#' @author Philip Kezich
#' @param hour Desired hour of the day (up to and including 9am)
#' @param max Maximum daily temperature
#' @param min Minimum daily temperature
#' @return The temperature value corresponding to our selected hour
#' @export
#' 



temp_till_9am <- function(hour,max,min){
  
  # Simple cosine temperature model
  Thr <-  -(max-min)/2*cos(hour*pi/9) + (max +min)/2
  
  return(Thr)
  
}