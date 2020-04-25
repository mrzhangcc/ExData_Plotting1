## The distribute of Sub metering between Thursday and Saterday

library(dplyr)
library(lubridate)
# read data
consumption <- read.table('./household_power_consumption.txt', header = TRUE,
                          sep = ';', na.strings = '?', 
                          colClasses = c('character', 'character', rep('numeric', 7)))
consumption <- subset(consumption, Date == '1/2/2007' | Date == '2/2/2007')
consumption$Date <- strptime(consumption$Date, '%d/%m/%Y')
consumption <- consumption  %>% tbl_df() %>%
  mutate(Datetime = as.Date(paste(Date, Time))) %>%
  select(-(Date:Time)) %>%
  arrange(Datetime) %>%
  mutate(id = 1:n())

## setup 1X1
par(mfrow = c(1, 1), cra = c(480, 480))
# plot data
with(consumption, plot(id, Sub_metering_1, 
                       type = 'l', col = 'black', 
                       ylab = "Energy sub metering", xlab = '', xaxt = 'n'))
with(consumption, lines(id, Sub_metering_2, col = "red"))
with(consumption, lines(id, Sub_metering_3, col = "blue"))

legend("topright", lty = 1, col = c("black", "red", "blue"),
       legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))


ids <- consumption %>% 
  select(Datetime, id) %>%
  mutate(Datetime = wday(Datetime)) %>%
  group_by(Datetime) %>%
  summarize(m = min(id)) %>%
  filter(Datetime == 6)
# add axis
axis(1, at = c(min(consumption$id), ids$m, 
               max(consumption$id)), labels = c("Thu", "Fri", "Sat"))

# save to png
dev.copy(png, 'plot3.png')
dev.off()

