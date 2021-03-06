plot1 <- function(){
  ## function plot1.R plots a histogram of 
  ## Global Active Power in kilowatts

  ## In order to run this code, first download  
  ## https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip
  ## unzip the file and put the "household_power_consumption.txt" 
  ## file in your working directory (together with plot1.R)

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