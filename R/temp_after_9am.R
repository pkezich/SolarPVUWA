#'
#' @name temp_after_9am
#' @title temp_after_9am
#' @author Philip Kezich
#' @description The function temp_till_9am is used to temperatures for time up to and including 9am.
#' @param hour Desired hour of the day (up to and including 9am)
#' @param max Maximum daily temperature
#' @param min Minimum daily temperature
#' @export
#'


temp_after_9am <-function(hour,max,min){
  
  # Simple cosine temperature model  
  Thr <- (max-min)/2*cos((hour-10)*pi/13) + (max +min)/2
  
}
