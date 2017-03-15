library(grDevices)
#Read tables
power <- read.table("household_power_consumption.txt", sep = ";", header = TRUE)

#Used in all plotX.R to unfactor variables into numeric values
unfactor <- function(x){as.numeric(levels(x)[as.integer(x)])}

#Standardize time variables
power$Date <- as.Date(power$Date , format = "%d/%m/%Y")
power_sub <- power[grep("2007-02-0[1|2]", power$Date),]
datetime <- paste(power_sub$Date, power_sub$Time)
power_sub$Datetime <- as.POSIXct(datetime)

#Unfactor variable to plot
power_sub$Global_active_power <- unfactor(power_sub$Global_active_power)

#Create png file and plot 
png(filename = "plot2.png")
with(power_sub, plot(Global_active_power ~ Datetime, type = "l", ylab = "Global Active Power (kilowatts)", xlab = ""))
dev.off()
