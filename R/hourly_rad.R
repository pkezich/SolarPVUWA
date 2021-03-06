#' @title hourly_rad
#' @author Philip Kezich
#' @description Returns values of the Month/Day combinations which correspond to the desired hour
#' @param hour Desired hour for which irradiation values are returned
#' @param df1 Data frame filtered to the desired Month-Day combination
#' @return Data frame filtered to contain only the irradiation values for the selected hour
#' @export
#'

hourly_rad <- function(hour,df1){
  
  if(as.numeric(hour)<10) {
    hour<- paste0("0",hour)
  }
  
  timevec<-df1[which(grepl(paste0(hour,":00:00"),as.character(df1$datetime))==1),]    
  df<-t(timevec$rad)
  hourly_vector<-suppressWarnings(as.numeric(df))
  
  
  means <- mean(hourly_vector)
  stdev <- sd(hourly_vector)
  
  params <- c(means,stdev)
}