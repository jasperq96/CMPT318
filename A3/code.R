library(dplyr) # need to overwrite default filter() function
library(data.table) # need for as.ITime()
library(lubridate)
library(ggplot2)
library(depmixS4)
current_dir <- getwd()
setwd(current_dir) 

#Grab data from text file
rawData = read.table("Group_Assignment_3_Dataset.txt", sep = ',', header = TRUE)
set.seed(1)
#-------------------Data Formatting
rawData$dayOfWeek <- wday(dmy(rawData$Date)) #Sunday is 1

#function to assign weekNum
f <- function(date, origin) {
  (as.numeric(dmy(date) - dmy(origin)) %/% 7) + 1
}

#add week Num
rawData <- rawData %>%
  mutate(weekNum = f(rawData$Date, origin="01/01/2007"))

#scale Global_active_power, #Global_reactive_power and Global_intensity
rawData$Scaled_global_active_power <- scale(rawData$Global_active_power, center = TRUE, scale = TRUE)
rawData$Scaled_global_reactive_power <- scale(rawData$Global_reactive_power, center = TRUE, scale = TRUE)
rawData$Scaled_global_intensity <- scale(rawData$Global_intensity, center = TRUE, scale = TRUE)


#choose a specific weekday(chose wednesday)
wednesday <- filter(rawData, dayOfWeek==3)

ggplot(data = wednesday, mapping=aes(x=Time)) +
  geom_point(aes(y=Scaled_global_active_power), color="red")

#Time window : 06:30 ~ 9:30
wednesday$Time <- as.ITime(wednesday$Time)
windowStart = as.ITime("6:30:00")
windowEnd = as.ITime("9:30:00")
newDataSet <- filter(wednesday, wednesday$Time>windowStart & wednesday$Time<windowEnd)

ggplot(data = newDataSet, mapping=aes(x=Time)) +
  geom_point(aes(y=Scaled_global_active_power), color="red")

#training models
totaltimes <- rep(c(179), times=52)

model3states <- depmix(response = Scaled_global_active_power ~ 1, data = newDataSet, nstates = 3,ntimes = totaltimes)
fitmodel3states <- fit(model3states)
summary(fitmodel3states)
print(fitmodel3states)

model5states <- depmix(response = Scaled_global_active_power ~ 1, data = newDataSet, nstates = 5,ntimes = totaltimes)
fitmodel5states <- fit(model5states)
summary(fitmodel5states)
print(fitmodel5states)

model9states <- depmix(response = Scaled_global_active_power ~ 1, data = newDataSet, nstates = 9,ntimes = totaltimes)
fitmodel9states <- fit(model9states)
summary(fitmodel9states)
print(fitmodel9states)

model12states <- depmix(response = Scaled_global_active_power ~ 1, data = newDataSet, nstates = 12,ntimes = totaltimes)
fitmodel12states <- fit(model12states)
summary(fitmodel12states)
print(fitmodel12states)

model15states <- depmix(response = Scaled_global_active_power ~ 1, data = newDataSet, nstates = 15,ntimes = totaltimes)
fitmodel15states <- fit(model15states)
summary(fitmodel15states)
print(fitmodel15states)

plot(1:5, c(BIC(fitmodel3states),BIC(fitmodel5states),BIC(fitmodel9states),BIC(fitmodel12states),BIC(fitmodel15states)), ty="b")
plot(1:5, c(logLik(fitmodel3states),logLik(fitmodel5states),logLik(fitmodel9states),logLik(fitmodel12states),logLik(fitmodel12states)), ty="b")

