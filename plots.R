library(dplyr)
library(lubridate)

##data import
directory <- './specdata'

setwd(directory)
initial_data <- read.table("household_power_consumption.txt", 
                           header=TRUE,sep=";", nrows=5)

#We will only be using data from the dates 2007-02-01 and 2007-02-02. One alternative is to readthe data from just those dates rather than reading in the entire dataset and subsetting to those  dates.
HPC_dat<-read.table("household_power_consumption.txt", 
                header=TRUE,sep=";", skip=66630, 
                nrows=2900, col.names=names(initial_data), na.strings=c("?"),
                colClasses=c("character", "character","numeric",
                             "numeric","numeric","numeric",
                             "numeric","numeric","numeric"))

#converting and subsetting of date

HPC_dat$Date<-as.Date(HPC_dat$Date, format = "%d/%m/%Y")
HPC_dat$Time<-strptime(paste(HPC_dat$Date,HPC_dat$Time),"%F %T")
HPC_dat<-subset(HPC_dat,HPC_dat$Date %in% as.Date(c("2007-02-01","2007-02-02")))


##plot 1

plot1 <- function(data) {
  par(mfrow=c(1,1),mar=c(5,4.5,4,2))
  hist(HPC_dat$Global_active_power, 
       col="red", 
       main="Global Active Power", 
       xlab="Global Active Power (kilowatts)")
}

##plot 2

plot2 <- function(data) {
  par(mfrow=c(1,1),mar=c(5,4.5,4,2))
  plot(HPC_dat$Time,HPC_dat$Global_active_power, 
       ylab="Global Active Power (kilowatts)", 
       xlab="", 
       pch =".", 
       type="l")
}

##plot 3

plot3 <- function(data) {
  par(mfrow=c(1,1),mar=c(4,4,4,2))
  plot(HPC_dat$Time,HPC_dat$Sub_metering_1,
       ylab="Energy sub metering", 
       xlab="", 
       type="l", 
       col="black")
  points(HPC_dat$Time,HPC_dat$Sub_metering_2, 
         col="red", 
         type="l")
  points(HPC_dat$Time,HPC_dat$Sub_metering_3, 
         col="blue", 
         type="l")
  legend("topright", lwd=1, 
         col=c("black", "red", "blue"), 
         legend=names(HPC_dat[,7:9]))
}

##plot 4

plot4 <- function(data) {
  par(mfcol=c(2,2), mar=c(4.5,5,2,2))
  # Plot 4.a
  plot(HPC_dat$Time,HPC_dat$Global_active_power, 
       ylab="Global Active Power (kilowatts)", 
       xlab="", 
       pch =".", 
       type="l")
  # Plot 4.b
  plot(HPC_dat$Time,HPC_dat$Sub_metering_1,
       ylab="Energy sub metering", 
       xlab="", 
       type="l", 
       col="black")
  points(HPC_dat$Time,HPC_dat$Sub_metering_2, 
         col="red", 
         type="l")
  points(HPC_dat$Time,HPC_dat$Sub_metering_3, 
         col="blue", 
         type="l")
  legend("topright", 
         lwd=1, 
         col=c("black", "red", "blue"), 
         legend=names(HPC_dat[,7:9]), 
         bty="n")
  # Plot 4.3
  plot(HPC_dat$Time,HPC_dat$Voltage, 
       ylab="Voltage", 
       xlab="datetime", 
       type="l")
  # Plot 4.4
  plot(HPC_dat$Time,HPC_dat$Global_reactive_power, 
       ylab="Global_reactive_power", 
       xlab="datetime", 
       type="l")
}



# Export PNGs of the plots
png("plot1.png")
plot1(subset)
dev.off()
png("plot2.png")
plot2(subset)
dev.off()
png("plot3.png")
plot3(subset)
dev.off()
png("plot4.png")
plot4(subset)
dev.off()