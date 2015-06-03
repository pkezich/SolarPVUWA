
#'
#' @title calculate_postcode_pv
#' @name calculate_postcode_pv
#' @description This function produces an estimate for the mean and upper and lower bounds of the 90th
#' percentile for PV output in a specified postcode usign the solaR model of a small scale PV system
#' @author Philip Kezich
#' @param pv_data Data folder for installed PV and geographic data
#' @param irradiation_data Data folder for processed irradiation data
#' @param Postcode Desired Postcode String
#' @param MonthDay Desired Month-Day String
#' @examples
#' calculate_postcode_pv('C:/SolarPVUWA/data/Processed Data/','6004','06-21')
#' @return The total PV production for the mean and upper and lower bounds of daily solar irradiation, including a graph of the results
#' @import R.utils
#' @export


data(sysdata, envir=environment())

calculate_postcode_pv <- function(irradiation_data,Postcode,MonthDay){

#Calculates hourly temperatures for the selected date. Uses simple cosine model and max and min daily values
temperature_vector <- temperature_interpolation(Postcode, MonthDay)

# Calculates total electricty production for that postcode on that day
total_prod <- postcode_pv_output(Postcode,MonthDay,irradiation_data,InstalledPV,temperature_vector)


# Graphing of output
colours <- c( "blue", "orange", "red")
matplot(t(total_prod), type = c("b"),pch=21,col = colours,xlab="Time (Hours)",ylab="PV Output (kW)",main=paste0("PV Output for ",Postcode," on the ",MonthDay))

}