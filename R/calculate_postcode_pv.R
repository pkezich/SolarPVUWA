
#'
#' @title calculate_postcode_pv
#' @name calculate_postcode_pv
#' @description This script calculates the mean and standard deviation for a month-day combination using the processed values
#' created by the massSolarCruncher script by Grant Coble-Neale. The script then  calculated the outputs of PV
#' installations in the specified postcode for cases corresponding to the mean and upper and lower bounds of the 90th
#' percentile for irradiation
#' @author Philip Kezich
#' @param pv_data Data folder for installed PV and geographic data
#' @param irradiation_data Data folder for processed irradiation data
#' @param Postcode Desired Postcode
#' @param MonthDay Desired Month-Day String
#' @import R.utils
#' @import SolaR
#' @export

calculate_postcode_pv <- function(pv_data,irradiation_data,Postcode,MonthDay){

library(R.utils)
library(solaR)
library(ggplot2)

load(paste0(pv_data,'LatLongWA.Rdata'))
load(paste0(pv_data,'InstalledPV.Rdata'))
load(paste0(pv_data,'BOMDAILYTEMP.Rdata'))
load(paste0(pv_data,'BOMStatLocs.Rdata'))


source("temperature_interpolation.R")
source("hourly_rad.R")
source("temperature_interpolation.R")
source("postcode_pv_output.R")
source("month_switch.R")


#Calculates hourly temperatures for the selected date. Uses simple cosine model and max and min daily values
temperature_vector <- temperature_interpolation(Postcode, MonthDay, LatLongWA,BOMDAILYTEMP,BOMStatLocs)

# Calculates total electricty production for that postcode on that day
total_prod <- postcode_pv_output(Postcode,MonthDay,irradiation_data,InstalledPV,temperature_vector)


# Graphing of output
colours <- c( "blue", "orange", "red")
barplot(total_prod[1,],main=paste0("Estimated Daily PV Production for ",MonthDay),ylab = "Output (kW)",col=colours,axes=TRUE)

return(total_prod)

}