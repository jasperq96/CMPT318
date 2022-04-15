library(dplyr) # need to overwrite default filter() function
library(data.table) # need for as.ITime()
library(lubridate)
library(psych) # For geometric.mean()
library(corrplot)
# Set Working Directory
# May need to manually set to own working directory, can be done under 'Session' Tab
current_dir <- getwd()
setwd(current_dir) 

#Grab data from text file
rawData = read.table("Group_Assignment_1_Dataset.txt", sep = ',', header = TRUE)

#-------------------Data Formatting

#Label weeks and grabbing assigned weeks
rawData$weekNum <- format(as.Date(rawData$Date, format = "%d/%m/%Y"),"%V")
wd <- filter(rawData, rawData$weekNum == "05")
#Label time of day (According to Google +6pm is night, any time before day)
dataTime <- as.ITime(format(wd$Time, format = "%H:%M:%S"))
morningEnd = as.ITime("18:00:00")
nightEnd = as.ITime("6:00:00")
wd$TimeOfDay <- ifelse(dataTime < morningEnd & dataTime > nightEnd, "Day", "Night")

#Label the day of the week
dayOfWeek <- wday(dmy(wd$Date)) #Sunday is 1
wd$day <- ifelse(dayOfWeek %in% c("1","7"), "weekend","weekday")

#-------------------Question 1

stats <- function(data, title){
  cat(title, "Arithmetic Mean", mean(data), "\n")
  cat(title, "Geometric Mean", geometric.mean(data), "\n")
  cat(title, "Median", median(data), "\n")
  cat(title, "Mode", (names(table(data))[table(data)==max(table(data))]), "\n") 
  cat(title, "Standard Deviation", sd(data), "\n")
  cat("\n")
}

#For data set A
stats(wd$Global_active_power, "Global Active Power(A)")
stats(wd$Global_reactive_power, "Global Reactive Power(B)")
stats(wd$Voltage, "Voltage(C)")

# Min/Max for Global Active Power
weekendDay <- wd$Global_active_power[wd$day == "weekend" & wd$TimeOfDay == "Day"]
min(weekendDay)
max(weekendDay)

weekendNight <- wd$Global_active_power[wd$day == "weekend" & wd$TimeOfDay == "Night"]
min(weekendNight)
max(weekendNight)

WeekdayDay <- wd$Global_active_power[wd$day == "weekday" & wd$TimeOfDay == "Day"]
min(WeekdayDay)
max(WeekdayDay)

weekdayNight <- wd$Global_active_power[wd$day == "weekday" & wd$TimeOfDay == "Night"]
min(weekdayNight)
max(weekdayNight)

# Min/Max for Global Reactive Power
weekendDay <- wd$Global_reactive_power[wd$day == "weekend" & wd$TimeOfDay == "Day"]
min(weekendDay)
max(weekendDay)

weekendNight <- wd$Global_reactive_power[wd$day == "weekend" & wd$TimeOfDay == "Night"]
min(weekendNight)
max(weekendNight)

WeekdayDay <- wd$Global_reactive_power[wd$day == "weekday" & wd$TimeOfDay == "Day"]
min(WeekdayDay)
max(WeekdayDay)

weekdayNight <- wd$Global_reactive_power[wd$day == "weekday" & wd$TimeOfDay == "Night"]
min(weekdayNight)
max(weekdayNight)

#------------------------question 2
corr_data <- wd[,3:9]
head(corr_data,20)
corr_matrix <-cor(corr_data,method="pearson")
corr_matrix <- round(corr_matrix,3)
corr_matrix
corrplot(corr_matrix,method="number")
#------------------------------- 

#--------------------------------------------- Question 3 start

#----------------Plot Global_intensity vs Time-------------
SundayDay <- filter(wd, wday(dmy(wd$Date)) == 1 & wd$TimeOfDay == "Day")
SundayNight <- filter(wd, wday(dmy(wd$Date)) == 1 & wd$TimeOfDay == "Night")
MondayDay <- filter(wd, wday(dmy(wd$Date)) == 2 & wd$TimeOfDay == "Day")
MondayNight <- filter(wd, wday(dmy(wd$Date)) == 2 & wd$TimeOfDay == "Night")
TuesdayDay <- filter(wd, wday(dmy(wd$Date)) == 3 & wd$TimeOfDay == "Day")
TuesdayNight <- filter(wd, wday(dmy(wd$Date)) == 3 & wd$TimeOfDay == "Night")
WednesdayDay <- filter(wd, wday(dmy(wd$Date)) == 4 & wd$TimeOfDay == "Day")
WednesdayNight <- filter(wd, wday(dmy(wd$Date)) == 4 & wd$TimeOfDay == "Night")
ThursdayDay <- filter(wd, wday(dmy(wd$Date)) == 5 & wd$TimeOfDay == "Day")
ThursdayNight <- filter(wd, wday(dmy(wd$Date)) == 5 & wd$TimeOfDay == "Night")
FridayDay <- filter(wd, wday(dmy(wd$Date)) == 6 & wd$TimeOfDay=="Day")
FridayNight <- filter(wd, wday(dmy(wd$Date)) == 6 & wd$TimeOfDay=="Night")
SaturdayDay <- filter(wd, wday(dmy(wd$Date)) == 7 & wd$TimeOfDay=="Day")
SaturdayNight <- filter(wd, wday(dmy(wd$Date)) == 7 & wd$TimeOfDay=="Night")

#WeekdayDayTime
ggplot(SundayDay, aes(Time)) +
  geom_point(aes(y=MondayDay$Global_intensity), colour="red") +
  geom_point(aes(y=TuesdayDay$Global_intensity), colour="blue") +
  geom_point(aes(y=WednesdayDay$Global_intensity), colour="green") +
  geom_point(aes(y=ThursdayDay$Global_intensity), colour="purple") +
  geom_point(aes(y=FridayDay$Global_intensity), colour="orange")

#WeekdayNightTime
ggplot(SundayNight, aes(Time)) +
  geom_point(aes(y=MondayNight$Global_intensity), colour="red") +
  geom_point(aes(y=TuesdayNight$Global_intensity), colour="blue") +
  geom_point(aes(y=WednesdayNight$Global_intensity), colour="green") +
  geom_point(aes(y=ThursdayNight$Global_intensity), colour="purple") +
  geom_point(aes(y=FridayNight$Global_intensity), colour="orange")

#WeekendDayTime
ggplot(SundayDay, aes(Time)) +
  geom_point(aes(y=SaturdayDay$Global_intensity), colour="red") +
  geom_point(aes(y=Global_intensity), colour="blue")

#WeekendNightTime
ggplot(SundayNight, aes(Time)) +
  geom_point(aes(y=SaturdayNight$Global_intensity), colour="red") +
  geom_point(aes(y=Global_intensity), colour="blue")

#--------Compute average Global_intensity value for each time window-----------

avgGlobalIntensity <- aggregate(x = wd$Global_intensity, by = list(as.ITime(format(wd$Time, format = "%H:%M:%S")), wd$day), FUN = mean)

weekdayDayWindowStart = as.ITime("06:00:00")
weekdayDayWindowEnd = as.ITime("15:00:00")
weekdayNightWindowStart = as.ITime("18:00:00")
weekdayNightWindowEnd = as.ITime("01:00:00")
weekendDayWindowStart = as.ITime("08:00:00")
weekendDayWindowEnd = as.ITime("18:00:00")
weekendNightWindowStart = as.ITime("18:00:00")
weekendNightWindowEnd = as.ITime("03:00:00")

WeekdayDaySlot <- filter(avgGlobalIntensity, avgGlobalIntensity$Group.2 == "weekday" & avgGlobalIntensity$Group.1>weekdayDayWindowStart & avgGlobalIntensity$Group.1<weekdayDayWindowEnd)
WeekdayNightSlot <- filter(avgGlobalIntensity, avgGlobalIntensity$Group.2 == "weekday" & (avgGlobalIntensity$Group.1>=weekdayNightWindowStart | avgGlobalIntensity$Group.1<=weekdayNightWindowEnd))
WeekendDaySlot <- filter(avgGlobalIntensity, avgGlobalIntensity$Group.2 == "weekend" & avgGlobalIntensity$Group.1>weekendDayWindowStart & avgGlobalIntensity$Group.1<weekendDayWindowEnd)
WeekendNightSlot <- filter(avgGlobalIntensity, avgGlobalIntensity$Group.2 == "weekend" & (avgGlobalIntensity$Group.1>=weekendNightWindowStart | avgGlobalIntensity$Group.1<=weekendNightWindowEnd))
WeekdayDaySlot2 <- filter(avgGlobalIntensity, avgGlobalIntensity$Group.2 == "weekday" & avgGlobalIntensity$Group.1>weekdayDayWindowStart & avgGlobalIntensity$Group.1<weekdayDayWindowEnd)
WeekdayNightSlot2 <- filter(avgGlobalIntensity, avgGlobalIntensity$Group.2 == "weekday" & (avgGlobalIntensity$Group.1>=weekdayNightWindowStart | avgGlobalIntensity$Group.1<=weekdayNightWindowEnd))
WeekendDaySlot2 <- filter(avgGlobalIntensity, avgGlobalIntensity$Group.2 == "weekend" & avgGlobalIntensity$Group.1>weekendDayWindowStart & avgGlobalIntensity$Group.1<weekendDayWindowEnd)
WeekendNightSlot2 <- filter(avgGlobalIntensity, avgGlobalIntensity$Group.2 == "weekend" & (avgGlobalIntensity$Group.1>=weekendNightWindowStart | avgGlobalIntensity$Group.1<=weekendNightWindowEnd))


#-----------------------Linear and Polynomial regression------------------------------

weekdayDayfit <- lm(formula=x ~ Group.1, data=WeekdayDaySlot)

Group.1 = c(as.ITime("06:01:30"), as.ITime("09:01:30"), as.ITime("12:00:30"), as.ITime("14:58:30"))

weekdayDaynewTimes <- data.frame(Group.1)
weekdayDaypredictedValues <- predict(weekdayDayfit, newdata = weekdayDaynewTimes)

weekdayDayconfidence <- predict(weekdayDayfit, newdata = weekdayDaynewTimes, interval = "confidence")

weekdayDayprediction <- predict(weekdayDayfit, newdata = weekdayDaynewTimes, interval = "prediction")

weekdayDaypredicted <- predict(weekdayDayfit, interval="prediction")
WeekdayDaySlot = cbind(WeekdayDaySlot, weekdayDaypredicted)

weekdayDayfit2 <- lm(x~poly(Group.1,3,raw=TRUE), data=WeekdayDaySlot2)
weekdayDaypredicted2 <- predict(weekdayDayfit2, interval="prediction")
WeekdayDaySlot2 = cbind(WeekdayDaySlot2, weekdayDaypredicted2)

ggplot(data=WeekdayDaySlot, mapping=aes(x=Group.1, y=x)) +
  stat_smooth(formula = y~x, method = lm, color="red") +
  geom_ribbon( aes(ymin = lwr, ymax = upr), alpha = .15) +
  geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed") +
  geom_point(data=WeekdayDaySlot2, mapping=aes(x=Group.1, y=x)) +
  geom_line(data=WeekdayDaySlot2, mapping=aes(x=Group.1, y=fit), size=1, color="blue") + 
  geom_ribbon( aes(ymin = WeekdayDaySlot2$lwr, ymax = WeekdayDaySlot2$upr), color="blue", linetype="dashed", alpha = .15)



weekdayNightfit <- lm(formula=x ~ Group.1, data=WeekdayNightSlot)

Group.1 = c(as.ITime("18:00:30"), as.ITime("21:01:30"), as.ITime("00:00:30"), as.ITime("00:58:30"))

weekdayNightnewTimes <- data.frame(Group.1)
weekdayNightpredictedValues <- predict(weekdayNightfit, newdata = weekdayNightnewTimes)

weekdayNightconfidence <- predict(weekdayNightfit, newdata = weekdayNightnewTimes, interval = "confidence")

weekdayNightprediction <- predict(weekdayNightfit, newdata = weekdayNightnewTimes, interval = "prediction")

weekdayNightpredicted <- predict(weekdayNightfit, interval="prediction")
WeekdayNightSlot = cbind(WeekdayNightSlot, weekdayNightpredicted)

weekdayNightfit2 <- lm(x~poly(Group.1,3,raw=TRUE), data=WeekdayNightSlot2)
weekdayNightpredicted2 <- predict(weekdayNightfit2, interval="prediction")
WeekdayNightSlot2 = cbind(WeekdayNightSlot2, weekdayNightpredicted2)

ggplot(data=WeekdayNightSlot, mapping=aes(x=Group.1, y=x)) +
  stat_smooth(formula = y~x, method = lm, color="red") +
  geom_ribbon( aes(ymin = lwr, ymax = upr), alpha = .15) +
  geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed") +
  geom_point(data=WeekdayNightSlot2, mapping=aes(x=Group.1, y=x)) +
  geom_line(data=WeekdayNightSlot2, mapping=aes(x=Group.1, y=fit), size=1, color="blue") + 
  geom_ribbon( aes(ymin = WeekdayNightSlot2$lwr, ymax = WeekdayNightSlot2$upr), color="blue", linetype="dashed", alpha = .15)




weekendDayfit <- lm(formula=x ~ Group.1, data=WeekendDaySlot)

Group.1 = c(as.ITime("08:01:30"), as.ITime("11:01:30"), as.ITime("14:00:30"), as.ITime("17:58:30"))

weekendDaynewTimes <- data.frame(Group.1)
weekendDaypredictedValues <- predict(weekendDayfit, newdata = weekendDaynewTimes)

weekendDayconfidence <- predict(weekendDayfit, newdata = weekendDaynewTimes, interval = "confidence")

weekendDayprediction <- predict(weekendDayfit, newdata = weekendDaynewTimes, interval = "prediction")

weekendDaypredicted <- predict(weekendDayfit, interval="prediction")
WeekendDaySlot = cbind(WeekendDaySlot, weekendDaypredicted)

weekendDayfit2 <- lm(x~poly(Group.1,3,raw=TRUE), data=WeekendDaySlot2)
weekendDaypredicted2 <- predict(weekendDayfit2, interval="prediction")
WeekendDaySlot2 = cbind(WeekendDaySlot2, weekendDaypredicted2)

ggplot(data=WeekendDaySlot, mapping=aes(x=Group.1, y=x)) +
  stat_smooth(formula = y~x, method = lm, color="red") +
  geom_ribbon( aes(ymin = lwr, ymax = upr), alpha = .15) +
  geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed") +
  geom_point(data=WeekendDaySlot2, mapping=aes(x=Group.1, y=x)) +
  geom_line(data=WeekendDaySlot2, mapping=aes(x=Group.1, y=fit), size=1, color="blue") + 
  geom_ribbon( aes(ymin = WeekendDaySlot2$lwr, ymax = WeekendDaySlot2$upr), color="blue", linetype="dashed", alpha = .15)




weekendNightfit <- lm(formula=x ~ Group.1, data=WeekendNightSlot)

Group.1 = c(as.ITime("18:00:30"), as.ITime("21:01:30"), as.ITime("00:00:30"), as.ITime("02:58:30"))

weekendNightnewTimes <- data.frame(Group.1)
weekendNightpredictedValues <- predict(weekendNightfit, newdata = weekendNightnewTimes)

weekendNightconfidence <- predict(weekendNightfit, newdata = weekendNightnewTimes, interval = "confidence")

weekendNightprediction <- predict(weekendNightfit, newdata = weekendNightnewTimes, interval = "prediction")

weekendNightpredicted <- predict(weekendNightfit, interval="prediction")
WeekendNightSlot = cbind(WeekendNightSlot, weekendNightpredicted)

weekendNightfit2 <- lm(x~poly(Group.1,3,raw=TRUE), data=WeekendNightSlot2)
weekendNightpredicted2 <- predict(weekendNightfit2, interval="prediction")
WeekendNightSlot2 = cbind(WeekendNightSlot2, weekendNightpredicted2)

ggplot(data=WeekendNightSlot, mapping=aes(x=Group.1, y=x)) +
  stat_smooth(formula = y~x, method = lm, color="red") +
  geom_ribbon( aes(ymin = lwr, ymax = upr), alpha = .15) +
  geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed") +
  geom_point(data=WeekendNightSlot2, mapping=aes(x=Group.1, y=x)) +
  geom_line(data=WeekendNightSlot2, mapping=aes(x=Group.1, y=fit), size=1, color="blue") + 
  geom_ribbon( aes(ymin = WeekendNightSlot2$lwr, ymax = WeekendNightSlot2$upr), color="blue", linetype="dashed", alpha = .15)
