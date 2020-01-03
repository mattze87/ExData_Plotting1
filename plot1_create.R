plot1_create <- function() {
  
  ######## CREATE HISTOGRAMM ########
  
  raw_data <- read.table("household_power_consumption.txt", sep = ";")
  
  rel_data <- raw_data[raw_data[,1] == "1/2/2007" | raw_data[,1] == "2/2/2007" | raw_data[,1] == "Date",] # select relevant data 
  
  names(rel_data) = c("Date","Time","Global_active_power","Global_reactice_power","Voltage","
                      Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3") # rename columns
  
  rel_data <- rel_data[2:dim(rel_data)[1],] # remove first row as it contains the column names 
  
  rel_data$Global_active_power <- as.numeric(as.character(rel_data$Global_active_power)) # convert datatype to numeric
  
  png(file="plot1.png")
  
  hist(rel_data$Global_active_power, col = "red", main = "Global Active Power", 
       xlab = "Global Active Power (kilowatts)", cex.lab = 0.8, cex.axis = 0.8, cex.main = 1) # create histogramm
  
  dev.off()
  
}
  