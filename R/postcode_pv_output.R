#' @title postcode_pv_output
#' @name postcode_pv_output
#' @description This function returns the mean and upper and lower bounds of the total electricity output 
#' for a desired postcode on our selected Month/Day combination. This function relies on the function
#' hourly_rad and uses the temperature output of the temperature_interpolation function as an input
#' 
#' @param Postcode Desired Postcode
#' @param MonthDay Desired Month/Day combination
#' @param data.folder Folder containing the processed Irradiation data
#' @param InstalledPV Data Frame containing the Installed PV values sourced from the
#' clean energy regulator
#' @param temp_vector Vector of hourly temperature values for our selected day
#' @export


postcode_pv_output <- function(Postcode, MonthDay,data.folder,InstalledPV,temp_vector){


# Create a vector of the  folders for each year
a<-list.dirs(path = data.folder)

# For each year load the respective postcode data frame and extract the row which contains the Month-Day-Hour combination
# append to data frame df1
for(i in 2:length(a)){
  tvalue <- loadToEnv(paste0(a[i],"/P",Postcode,".Rdata"))[[paste0("POA.",Postcode)]]; 
  c<-tvalue[which(grepl(MonthDay,as.character(tvalue$Date))==1),]
  if (exists('df1')) df1 <- rbind(df1,c)
  else df1<-c
}

# Creates vector representing hours of day
time_vector<-as.character(seq(0,23))

# Extracts Irradiation values for each hour
solar_hourly_vector<-lapply(time_vector, function(x) hourly_rad(x,df1))
solar_hourly_vector<-as.data.frame(solar_hourly_vector)

names(solar_hourly_vector)<-time_vector

# Extracts means and standard deviations for each hour
means <- colMeans(solar_hourly_vector,na.rm=TRUE)
standard_deviations <- sapply(solar_hourly_vector, sd)

# Multiplier for double sided 90% confidence level
multiplier <- 1.64

# Creates upper and lower bounds functions
lower_bounds <- means-multiplier*standard_deviations
upper_bounds <-  means+multiplier*standard_deviations

# Creates data frames for the fProd function using the irradiation and temperature
inclin <- data.frame(Gef=means,Ta=temp_vector)
inclin_min <- data.frame(Gef=lower_bounds,Ta=temp_vector)
inclin_max <- data.frame(Gef=upper_bounds,Ta=temp_vector)

# Applies fProd to each vector
pv_productivity <- fProd(inclin)
pv_productivity_min <- fProd(inclin_min)
pv_productivity_max <- fProd(inclin_max)

# Divides the simulated AC output by the default installed capacity (25,000W) to obtain the efficiency
pv_productivity <- pv_productivity$Pac/25000
pv_productivity_min <- pv_productivity_min$Pac/25000
pv_productivity_max <- pv_productivity_max$Pac/25000


# Extracts PV capacity for out desired postcode
df3<-InstalledPV[which(grepl(Postcode,as.character(InstalledPV$Small.Unit.Installation.Postcode))==1),]

installedcap <- df3[,2]

# Calculates Estimate of total PV production for the postcode
total_prod <- rbind(pv_productivity_min,pv_productivity,pv_productivity_max)
total_prod[is.na(total_prod)]<-0
total_prod <- total_prod#*installedcap

return(total_prod)

}