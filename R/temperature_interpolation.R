#
# Simple Cosine function obtained from USAFETAC/PR-91/017, A Method for Estimating Missing Hourly Temperatures Using Daily Maximum and
# Minimum Temperatures, August 1991, by William R Schaub Jr.

#' @author Philip Kezich
#' @name temperature_interpolation
#' @title temperature_interpolation
#' @export




temperature_interpolation <- function(Postcode,MonthDay,pcode_lats_and_longs,bom_temps,bom_locs){


  # Extract latitude and longitude from our desired postcode
  Latitude <- pcode_lats_and_longs[which(Postcode==pcode_lats_and_longs$POA),3]
  Longitude <- pcode_lats_and_longs[which(Postcode==pcode_lats_and_longs$POA),2]  
  
  # Extract Month and Day from MonthDay string
  Month <- substr(MonthDay,1,2)
  Day <- substr(MonthDay,4,5)
  
  # Finds closest weather station and extract it's name
  row.match <- which(abs(as.numeric(bom_locs$Lat)-Latitude)+abs(as.numeric(bom_locs$Lon)-Longitude)==min(abs(as.numeric(bom_locs$Lat)-Latitude))+abs(as.numeric(bom_locs$Lon)-Longitude))
  station.name <- as.character(bom_locs[row.match,1])
  
  # Extract which temperature values correspond to that stations name as well as the desired month and day
  df1<-bom_temps[which(toupper(bom_temps$Name)==station.name),]
  df1 <-df1[which(df1$Month==as.numeric(Month)),]
  df1 <-df1[which(df1$Day==as.numeric(Day)),]
  
  # Extract the max and min daily values
  dfmax <-df1[which((df1$Variable=='MAXT')),]
  dfmin <-df1[which((df1$Variable=='MINT')),]
  
  
  mean_max_temp <- mean(dfmax$Value)
  mean_min_temp <- mean(dfmin$Value)
  
# Apply early hour function
early_hours <- seq(0,9)
early_hours_temps <- sapply(early_hours, function(x) temp_till_9am(x,mean_max_temp,mean_min_temp))

# Apply later hour function
later_hours <- seq(10,23)
later_hours_temps <- sapply(later_hours, function(x) temp_after_9am(x,mean_max_temp,mean_min_temp))


daily_temps <-c(early_hours_temps,later_hours_temps)

# Return temperature values for all hours
return(daily_temps)
  
}