library(data.table)
library(dplyr)
library(lubridate)
library(depmixS4)
library(ggplot2)
set.seed(1)
#-----------------------------------------Format Data-----------------------------------------
rawData = read.table("CleanedData.txt", sep = ',', header = TRUE)

dateTime <- paste(rawData$Date, rawData$Time)
rawData$timestamp <- as.POSIXlt(strptime(dateTime, format = "%d/%m/%Y %H:%M:%S"))

rawData$weekNum <- format(as.Date(rawData$Date, format = "%d/%m/%Y"),"%V")

rawData$dayOfWeek <- wday(dmy(rawData$Date)) #Sunday is 1
rawData[3:9] <- as.data.frame(scale(rawData[3:9], center = TRUE, scale = TRUE))
#Filter for time window
wd = filter(rawData, dayOfWeek == '2')
time <- as.ITime(wd$Time)
filteredData <- filter(wd, time >= as.ITime("18:00") & time <= as.ITime("23:59"))


  #ggplot(data = filteredData, mapping=aes(x=Time)) +
  #  geom_point(aes(y=Global_active_power), color="red")
  #ggsave(filename = "global_active_power2.png") 
  #ggplot(data = filteredData, mapping=aes(x=Time)) +
  #  geom_point(aes(y=Global_intensity), color="red")
  #ggsave(filename = "global_intesnity2.png");

#set up Test and Train Data
trainingData <- subset(filteredData, as.Date(filteredData$timestamp) < as.Date("2009-1-1 00:00:00 PST"))
testData <- subset(filteredData, as.Date(filteredData$timestamp) >= as.Date("2009-1-1 00:00:00 PST"))

#Filter data to only include response variables
trainDataForMulti <- trainingData[,c(1,2, 3, 6, 9)] #might need to change filter depending on depmix response variables
testDataForMulti <- testData[,c(1,2, 3, 6, 9)] #might need to change filter depending on depmix response variables

dateNumberTrain <-trainingData[c(1)]
dateNumberTest <-testData[c(1)]
dateNumberTrain <- unique(dateNumberTrain)
dateNumberTest <- unique(dateNumberTest)
#--------------------------------------------------------------------------------------------

#-----------------------------------------Grab Anomaly Data 1-----------------------------------------
anomalyData1 = read.table("DataWithAnomalies1.txt", sep = ',', header = TRUE)

dateTime <- paste(anomalyData1$Date, anomalyData1$Time)
anomalyData1$timestamp <- as.POSIXlt(strptime(dateTime, format = "%d/%m/%Y %H:%M:%S"))

anomalyData1$weekNum <- format(as.Date(anomalyData1$Date, format = "%d/%m/%Y"),"%V")

anomalyData1$dayOfWeek <- wday(dmy(anomalyData1$Date)) #Sunday is 1
anomalyData1[3:9] <- as.data.frame(scale(anomalyData1[3:9], center = TRUE, scale = TRUE))
#Filter for time window
anomalyData1.day = filter(anomalyData1, dayOfWeek == '2')
time <- as.ITime(anomalyData1.day$Time)
filteredAnomalyData1 <- filter(anomalyData1.day, time >= as.ITime("18:00") & time <= as.ITime("23:59"))
anomaly1.data.for.hmm <- filteredAnomalyData1[,c(1,2, 3, 6, 9)] #might need to change filter depending on depmix response variables
#-----------------------------------------------------------------------------------------------------

#-----------------------------------------Grab Anomaly Data 2-----------------------------------------
anomalyData2 = read.table("DataWithAnomalies2.txt", sep = ',', header = TRUE)

dateTime <- paste(anomalyData2$Date, anomalyData2$Time)
anomalyData2$timestamp <- as.POSIXlt(strptime(dateTime, format = "%d/%m/%Y %H:%M:%S"))

anomalyData2$weekNum <- format(as.Date(anomalyData2$Date, format = "%d/%m/%Y"),"%V")

anomalyData2$dayOfWeek <- wday(dmy(anomalyData2$Date)) #Sunday is 1
anomalyData2[3:9] <- as.data.frame(scale(anomalyData2[3:9], center = TRUE, scale = TRUE))
#Filter for time window: Monday 18:00 -> 23:59
anomalyData2.day = filter(anomalyData2, dayOfWeek == '2')
time <- as.ITime(anomalyData2.day$Time)
filteredanomalyData2 <- filter(anomalyData2.day, time >= as.ITime("18:00") & time <= as.ITime("23:59"))
anomaly2.data.for.hmm <- filteredanomalyData2[,c(1,2, 3, 6, 9)] #might need to change filter depending on depmix response variables
#-----------------------------------------------------------------------------------------------------

#-----------------------------------------Grab Anomaly Data 1-----------------------------------------
anomalyData3 = read.table("DataWithAnomalies3.txt", sep = ',', header = TRUE)

dateTime <- paste(anomalyData3$Date, anomalyData3$Time)
anomalyData3$timestamp <- as.POSIXlt(strptime(dateTime, format = "%d/%m/%Y %H:%M:%S"))

anomalyData3$weekNum <- format(as.Date(anomalyData3$Date, format = "%d/%m/%Y"),"%V")

anomalyData3$dayOfWeek <- wday(dmy(anomalyData3$Date)) #Sunday is 1
anomalyData3[3:9] <- as.data.frame(scale(anomalyData3[3:9], center = TRUE, scale = TRUE))
#Filter for time window
anomalyData3.day = filter(anomalyData3, dayOfWeek == '2')
time <- as.ITime(anomalyData3.day$Time)
filteredanomalyData3 <- filter(anomalyData3.day, time >= as.ITime("18:00") & time <= as.ITime("23:59"))
anomaly3.data.for.hmm <- filteredanomalyData3[,c(1,2, 3, 6, 9)] #might need to change filter depending on depmix response variables
#-----------------------------------------------------------------------------------------------------

#----------------------------------------Training Model----------------------------------------
set.seed(1) #needed to get starting values that will result in the right model
numStates <- 1:6 * 4

col = c("States", "BIC", "LogLik","NormalizedBIC","NormalizedLogLik")
stats = array(0, dim = c(24, length(col)))
statsTrain = array(0, dim = c(24, length(col)))
colnames(stats) = col
colnames(statsTrain) = col
#This loop takes awhile
for (i in numStates){
  print(paste("Running",i,"States"))
  
  #I tried a few combinations, and it seems that with Global_active_power, Global_intensity, Sub_metering_3, it gets an error "Starting values not feasible; please provide them" Don't know why
  
  trainModel = depmix(list(Global_intensity ~ 1, Global_active_power ~ 1), data = trainDataForMulti, nstates = i, family = list(gaussian(), gaussian()))
  fitTrainModel <- fit(trainModel)
  
  testModel = depmix(list(Global_intensity ~ 1, Global_active_power ~ 1), data = testDataForMulti, nstates = i, family = list(gaussian(), gaussian()))
  #fitTestModel <- fit(testModel)
  
  Hmmtest <- setpars(testModel, getpars(fitTrainModel))
  HmmtestFB <- forwardbackward(Hmmtest)
  
  stats[i, "States"] <- i
  #stats[i, "BIC"] <- BIC(fitTestModel)
  stats[i, "LogLik"] <- HmmtestFB$logLike
  #stats[i, "NormalizedBIC"] <- stats[i, "BIC"]/nrow(dateNumberTest)
  stats[i, "NormalizedLogLik"] <- stats[i, "LogLik"]/nrow(dateNumberTest)
  
  statsTrain[i, "States"] <- i
  statsTrain[i, "BIC"] <- BIC(fitTrainModel)
  statsTrain[i, "LogLik"] <- logLik(fitTrainModel)
  statsTrain[i, "NormalizedBIC"] <- statsTrain[i, "BIC"]/nrow(dateNumberTrain)
  statsTrain[i, "NormalizedLogLik"] <- statsTrain[i, "LogLik"]/nrow(dateNumberTrain)
}
#-----------------------------------------------------------------------------------------------

#-------------------------------------------------TASK 3----------------------------------------------

states.for.best.model <- 12 #change this once we know best model states and then delete this comment

monday.model = depmix(list(Global_intensity ~ 1, Global_active_power ~ 1), data = trainDataForMulti, nstates = states.for.best.model, family = list(gaussian(), gaussian()))
fit.monday.model <- fit(monday.model)
monday.object <- forwardbackward(fit.monday.model)

#-----------------------------------------Anomaly Detection 1-----------------------------------------
anomaly1.model = depmix(list(Global_intensity ~ 1, Global_active_power ~ 1), data = anomaly1.data.for.hmm, nstates = states.for.best.model, family = list(gaussian(), gaussian()))
anomaly1 <- setpars(anomaly1.model, getpars(fit.monday.model))
anomaly1.object <- forwardbackward(anomaly1)
#-----------------------------------------------------------------------------------------------------

#-----------------------------------------Anomaly Detection 2-----------------------------------------
anomaly2.model = depmix(list(Global_intensity ~ 1, Global_active_power ~ 1), data = anomaly2.data.for.hmm, nstates = states.for.best.model, family = list(gaussian(), gaussian()))
anomaly2 <- setpars(anomaly2.model, getpars(fit.monday.model))
anomaly2.object <- forwardbackward(anomaly2)
#-----------------------------------------------------------------------------------------------------

#-----------------------------------------Anomaly Detection 3-----------------------------------------
anomaly3.model = depmix(list(Global_intensity ~ 1, Global_active_power ~ 1), data = anomaly3.data.for.hmm, nstates = states.for.best.model, family = list(gaussian(), gaussian()))
anomaly3 <- setpars(anomaly3.model, getpars(fit.monday.model))
anomaly3.object <- forwardbackward(anomaly3)
#-----------------------------------------------------------------------------------------------------
anomaly1_row<-nrow(unique(anomaly1.data.for.hmm[c(1)]))
anomaly2_row<-nrow(unique(anomaly2.data.for.hmm[c(1)]))
anomaly3_row<-nrow(unique(anomaly3.data.for.hmm[c(1)]))
  
