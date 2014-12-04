plot1 <- function(){
  ## function plot1.R plots a histogram of 
  ## Global Active Power in kilowatts

  ## reading in household power consuption data 
  ## caution: all data read in as characters
  household_data <- read.table("household_power_consumption.txt", header = TRUE, sep = ";", na.strings = "?", colClasses ="character")
  
  ## subseting data set in order to work with two dates only
  data <- household_data[household_data$Date == "1/2/2007" | household_data$Date == "2/2/2007",] 

  gap <- as.numeric(data$Global_active_power) 

  ## setting png device
  png(file = "plot1.png", width = 480, height = 480, units = "px")
  
  ## plotting histogram of Global Active Power in kilowatts
  hist(gap[gap != "NA"], xlab = "Global Active Power (kilowatts)", col ="red", main = "Global Active Power")
  dev.off()
  
}