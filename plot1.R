## Hist of Global Active Power

library(dplyr)

consumption <- read.table('./household_power_consumption.txt', header = TRUE,
                          sep = ';', na.strings = '?', 
                          colClasses = c('character', 'character', rep('numeric', 7)))
consumption <- subset(consumption, Date == '1/2/2007' | Date == '2/2/2007')
consumption$Date <- strptime(consumption$Date, '%d/%m/%Y')

## setup 1X1
par(mfrow = c(1, 1), cra = c(480, 480))

hist(consumption$Global_active_power, col = "red", main = "Global Active Power",
     xlab = 'Global Active Power (kilowatts)')

dev.copy(png, 'plot1.png')
dev.off()


