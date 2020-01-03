plot2_create <- function() {
  
  ######## CREATE LINE DIAGRAM ########
  
  raw_data <- read.table("household_power_consumption.txt", sep = ";")
  
  rel_data <- raw_data[raw_data[,1] == "1/2/2007" | raw_data[,1] == "2/2/2007" | raw_data[,1] == "Date",] # select relevant data 
  
  names(rel_data) = c("Date","Time","Global_active_power","Global_reactice_power","Voltage","
                      Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3") # rename columns
  
  rel_data <- rel_data[2:dim(rel_data)[1],] # remove first row as it contains the column names 
  
  rel_data$Global_active_power <- as.numeric(as.character(rel_data$Global_active_power)) # convert datatype to numeric

  rel_data <- cbind(rel_data,relTime = as.POSIXct(paste(as.Date(rel_data$Date, format = "%d/%m/%Y"),
                                                        rel_data$Time))) # create column relTIme to combine date and time 
  
  png(file="plot2.png")
  
  plot(rel_data$relTime, rel_data$Global_active_power, type = "l", 
       xlab = "", ylab = "Global Active Power (kilowatts)")    # create plot
  
  dev.off()
  
}
