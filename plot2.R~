plot2 <- function(){
  ## function plot2.R plots Global Active Power
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
  
  gap <- as.numeric(data$Global_active_power) 
  
  ## setting png device
  png(file = "plot2.png", width = 480, height = 480, units = "px")

  ## plotting Global Active Power in kilowatts as a function of time
  plot(t[gap != "NA"], gap[gap != "NA"], type = "l", xlab = "", ylab = "Global Active Power (kilowatts)")
  dev.off()
}