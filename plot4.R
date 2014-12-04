plot4 <- function(){
  ## function plot4.R makes panel with four plots
  ## showing change in time of variables from Houshehold power 
  ## consumption file, for the first two days in February 2007
  
  ## reading in household power consuption data 
  ## caution: all data read in as characters
  household_data <- read.table("household_power_consumption.txt", header = TRUE, sep = ";", na.strings = "?", colClasses ="character")

  ## subseting data set in order to work with two dates only
  data <- household_data[household_data$Date == "1/2/2007" | household_data$Date == "2/2/2007",] 

  ## reading date as "Date" class 
  data$Date <- as.Date(data$Date, "%d/%m/%Y")

  ## concatenate date and time
  time <- paste(data$Date, data$Time, sep =" ")
  
  ## coerce data and time into "POSIXlt" and "POSIXt" classes
  t <- strptime(time, "%Y-%m-%d %H:%M")
 
  gap <- as.numeric(data$Global_active_power) 
  grp <- as.numeric(data$Global_reactive_power) 
  voltage <- as.numeric(data$Voltage)
  energy1 <- as.numeric(data$Sub_metering_1)
  energy2 <- as.numeric(data$Sub_metering_2)
  energy3 <- as.numeric(data$Sub_metering_3)
  
  ## setting png device
  png(file = "plot4.png", width = 480, height = 480, units = "px")

  par(mfrow = c(2,2))  ## setting four plots panel
 
  ## top left plot
  ## Global Active Power histogram
  plot(t[gap != "NA"], gap[gap != "NA"], type = "l", xlab = "", ylab = "Global Active Power")
 
  ## top right plot
  ## Voltage as a function of time
  plot(t[voltage != "NA"], voltage[voltage != "NA"], type = "l", xlab = "datetime", ylab = "Voltage")
 
  ## bottom left plot
  ## Energy sub metering as a function of time
  plot(t[energy1 != "NA"], energy1[energy1 != "NA"], type = "l", col = "black", xlab = "", ylab = "Energy sub metering")
  points(t[energy2 != "NA"], energy2[energy2 != "NA"], type = "l", col = "red")
  points(t[energy3 != "NA"], energy3[energy3 != "NA"], type = "l", col = "blue")
  legend("topright", lty = 1, bty = "n", cex = 0.95, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

  ## bottom right plot
  ## Global reactive power as a function of time
  plot(t[grp != "NA"], grp[grp != "NA"], type = "l", xlab = "datetime", ylab = "Global_reactive_power")
  dev.off()
  
}