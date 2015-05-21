#'
#'
#'
#'
#'
#'@author Philip Kezich
#'@export
#'


temp_after_9am <-function(hour,max,min){
  
  # Simple cosine temperature model  
  Thr <- (max-min)/2*cos((hour-10)*pi/13) + (max +min)/2
  
}
