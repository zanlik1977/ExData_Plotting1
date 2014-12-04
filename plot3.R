plot3 <- function(){
  ## function plot3.R plots Energy sub metering
  ## as a function of time, for the first two days in February 2007
  
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

  energy1 <- as.numeric(data$Sub_metering_1)
  energy2 <- as.numeric(data$Sub_metering_2)
  energy3 <- as.numeric(data$Sub_metering_3)

  ## setting png device
  png(file = "plot3.png", width = 480, height = 480, units = "px")

  ## plotting Sub_metering_1 as a function of time
  plot(t[energy1 != "NA"],energy1[energy1 != "NA"], type = "l", col = "black", xlab = "", ylab = "Energy sub metering")

  ## overplotting with Sub_metering_2
  points(t[energy2 != "NA"],energy2[energy2 != "NA"], type = "l", col = "red")

  ## overplotting with Sub_metering_3
  points(t[energy3 != "NA"],energy3[energy3 != "NA"], type = "l", col = "blue")
  legend("topright", lty = 1, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  dev.off()
}