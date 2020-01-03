plot3_create <- function() {
  
  ######## CREATE MULTIPLE LINE DIAGRAMS ########
  
  raw_data <- read.table("household_power_consumption.txt", sep = ";")
  
  rel_data <- raw_data[raw_data[,1] == "1/2/2007" | raw_data[,1] == "2/2/2007" | raw_data[,1] == "Date",] # select relevant data 
  
  names(rel_data) = c("Date","Time","Global_active_power","Global_reactice_power","Voltage","
                      Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3") # rename columns
  
  rel_data <- rel_data[2:dim(rel_data)[1],] # remove first row as it contains the column names 
  
  rel_data$Global_active_power <- as.numeric(as.character(rel_data$Global_active_power)) # convert datatype to numeric
  rel_data$Sub_metering_1 <- as.numeric(as.character(rel_data$Sub_metering_1)) 
  rel_data$Sub_metering_2 <- as.numeric(as.character(rel_data$Sub_metering_2))
  rel_data$Sub_metering_3 <- as.numeric(as.character(rel_data$Sub_metering_3))
  
  rel_data <- cbind(rel_data,relTime = as.POSIXct(paste(as.Date(rel_data$Date, format = "%d/%m/%Y"),
                                                        rel_data$Time))) # create column relTIme to combine date and time 
  
  png(file="plot3.png")
  
  plot(rel_data$relTime, rel_data$Sub_metering_1, type = "n", 
       xlab = "", ylab = "Energy sub metering", yaxt = "n") # create empty plot  
  
  axis(2, at = seq(0,30, by = 10), las =3) # set values for y-axis
  
  points(rel_data$relTime, rel_data$Sub_metering_1, type = "l")
  points(rel_data$relTime, rel_data$Sub_metering_2, type = "l", col = "red")
  points(rel_data$relTime, rel_data$Sub_metering_3, type = "l", col = "blue")
  
  legend("topright", lty =  1, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  
  dev.off()
  
}
