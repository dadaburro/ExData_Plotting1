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
power_sub$Global_reactive_power <- unfactor(power_sub$Global_reactive_power)
power_sub$Sub_metering_1 <- unfactor(power_sub$Sub_metering_1)
power_sub$Sub_metering_2 <- unfactor(power_sub$Sub_metering_2)
power_sub$Voltage <- unfactor(power_sub$Voltage)

#Create png file and plot 
png(filename = "plot4.png")
par(mfrow = c(2,2), mar = c(4,4,2,2))
with(power_sub, plot(Global_active_power ~ Datetime, type = "l", ylab = "Global Active Power (kilowatts)", xlab = ""))
with(power_sub, plot(Voltage ~ Datetime, type = "l", ylab = "Voltage", xlab = "Datetime"))
with(power_sub, plot(Sub_metering_1 ~ Datetime, type = "l", ylab = "Energy sub metering", xlab = ""))
with(power_sub, lines(Sub_metering_2 ~ Datetime, col = "red"))
with(power_sub, lines(Sub_metering_3 ~ Datetime, col = "blue"))
legend("topright", col = c("black", "red", "blue"), legend= c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), lty = 1, bty = "n")
with(power_sub, plot(Global_reactive_power ~ Datetime, type = "l", ylab = "Global_reactive_power", xlab = "Datetime"))
dev.off()
