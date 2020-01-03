plot4_create <- function() {
  
  ######## CREATE MULTIPLE LINE DIAGRAMS ########
  
  raw_data <- read.table("household_power_consumption.txt", sep = ";")
  
  rel_data <- raw_data[raw_data[,1] == "1/2/2007" | raw_data[,1] == "2/2/2007" | raw_data[,1] == "Date",] # select relevant data 
  
  names(rel_data) = c("Date","Time","Global_active_power","Global_reactice_power","Voltage","
                      Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3") # rename columns
  
  rel_data <- rel_data[2:dim(rel_data)[1],] # remove first row as it contains the column names 
  
  rel_data$Global_active_power <- as.numeric(as.character(rel_data$Global_active_power)) # convert datatype to numeric
  rel_data$Global_reactice_power <- as.numeric(as.character(rel_data$Global_reactice_power)) 
  rel_data$Sub_metering_1 <- as.numeric(as.character(rel_data$Sub_metering_1)) 
  rel_data$Sub_metering_2 <- as.numeric(as.character(rel_data$Sub_metering_2))
  rel_data$Sub_metering_3 <- as.numeric(as.character(rel_data$Sub_metering_3))
  rel_data$Voltage <- as.numeric(as.character(rel_data$Voltage))
  
  
  rel_data <- cbind(rel_data,relTime = as.POSIXct(paste(as.Date(rel_data$Date, format = "%d/%m/%Y"),
                                                        rel_data$Time))) # create column relTIme to combine date and time 
  
  
  png(file="plot4.png")
  
  par(mfcol=c(2,2))
  
  ########## PLOT 1 ########## 
  
  plot(rel_data$relTime, rel_data$Global_active_power, type = "l", 
       xlab = "", ylab = "Global Active Power", cex.lab = 0.8, cex.axis = 0.8)    # create plot
  
  ########## PLOT 2 ##########
  
  plot(rel_data$relTime, rel_data$Sub_metering_1, type = "n", 
       xlab = "", ylab = "Energy sub metering", yaxt = "n", cex.lab = 0.8, cex.axis = 0.8) # create empty plot  
  
  axis(2, at = seq(0,30, by = 10), las =3, cex.axis = 0.8) # set values for y-axis
  
  points(rel_data$relTime, rel_data$Sub_metering_1, type = "l")
  points(rel_data$relTime, rel_data$Sub_metering_2, type = "l", col = "red")
  points(rel_data$relTime, rel_data$Sub_metering_3, type = "l", col = "blue")
  
  legend("topright", lty =  1, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), bty = "n", cex = 0.8)
  
  ########## PLOT 3 ##########
  
  plot(rel_data$relTime, rel_data$Voltage, type = "l", 
       xlab = "datetime", ylab = "Voltage", yaxt = "n", cex.lab = 0.8, cex.axis = 0.8)    # create plot
  
  axis(2, at = seq(234,246, by = 2), las =3, cex.axis = 0.8) # set values for y-axis
  
  ########## PLOT 4 ##########
  
  plot(rel_data$relTime, rel_data$Global_reactice_power, type = "n", 
       xlab = "datetime", ylab = "Global_reactive_power", yaxt = "n", cex.lab = 0.8, cex.axis = 0.8)    # create plot
  
  axis(2, at = seq(0,0.5, by = 0.1), las =3, cex.axis = 0.7) # set values for y-axis
  points(rel_data$relTime, rel_data$Global_reactice_power, type = "l")
  
  dev.off()
}
