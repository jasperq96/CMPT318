# If devtools doesn't work, need to install Rtools first at https://cran.r-project.org/bin/windows/Rtools/rtools40.html
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)



#------------------------------------------Functions------------------------------------------
# standardize <- function(column){
#   columnData <- timeWindowData[, c("timestamp", "dayTime", column)]
#   
#   columnData$Date <- format(as.POSIXct(columnData$timestamp), format = "%H:%M:%S")
#   columnData$Date <- as.POSIXct(columnData$Date, format = "%H:%M:%S")
#   
#   standardizedColumn <- columnData %>%
#     group_by(Date, dayTime) %>%
#     summarise(avgValue = mean(get(column)))
#   
#   avgStandardDeviation = sd(standardizedColumn$avgValue)
#   avgMean = mean(standardizedColumn$avgValue)
#   standardizedColumn$standardizedValue <- (standardizedColumn$avgValue - avgMean) / avgStandardDeviation
#   
#   return(standardizedColumn)
# }
#---------------------------------------------------------------------------------------------

rawData = read.table("CleanedData.txt", sep = ',', header = TRUE)

#-----------------------------------------Format Data-----------------------------------------
dateTime <- paste(rawData$Date, rawData$Time)
rawData$timestamp <- as.POSIXlt(strptime(dateTime, format = "%d/%m/%Y %H:%M:%S"))

rawData$weekNum <- format(as.Date(rawData$Date, format = "%d/%m/%Y"),"%V")

dayOfWeek <- wday(dmy(rawData$Date)) #Sunday is 1
rawData$day <- ifelse(dayOfWeek %in% c("1","7"), "weekend","weekday")
rawData$dayOfWeek <- wday(dmy(rawData$Date))
rawData$year <- as.integer(format(rawData$timestamp, format = "%y"))

rawData$dayTime <- as.ITime(rawData$Time)
dataTime <- as.numeric(hms(rawData$Time)) #convert to integer for faster calculation in while loop
timeWindow <- vector(mode = "integer", length(rawData$dayTime))

i = 1
#Converted comparison times to integer using as.numeric(hms("00:00:00"))
while (i <= length(timeWindow)){
  if ((dataTime[i] >= 0)&(dataTime[i] < 14400)){ # 00:00:00 - 04:00:00
    timeWindow[i] <- 1
  } else if ((dataTime[i] >= 14400)&(dataTime[i] < 28800)){ # 04:00:00 - 08:00:00
    timeWindow[i] <- 2
  } else if ((dataTime[i] >= 28800)&(dataTime[i] < 43200)){ # 08:00:00 - 12:00:00
    timeWindow[i] <- 3
  } else if ((dataTime[i] >= 43200)&(dataTime[i] < 57600)){ # 12:00:00 - 16:00:00
    timeWindow[i] <- 4
  } else if ((dataTime[i] >= 57600)&(dataTime[i] < 72000)){ # 16:00:00 - 20:00:00
    timeWindow[i] <- 5
  } else if ((dataTime[i] >= 72000)&(dataTime[i] <= 86399)){ # 20:00:00 - 23:59:59
    timeWindow[i] <- 6
  } 
  i = i + 1
}
rawData$timeWindow <- timeWindow
#---------------------------------------------------------------------------------------------

#----------------------------------Standardize rawData----------------------------------------

rawData[3:9] <- as.data.frame(scale(rawData[3:9], center = TRUE, scale = TRUE))

#---------------------Calculating PCA for 7 Variables for each time window--------------------
 j = 1
 while (j <= 2){#7
   i = 1
   while (i <= 2){#6
     dayData <- subset(rawData, dayOfWeek == j)
     timeWindowData <- subset(dayData, timeWindow == i)

     # # Order is: Global_active_power | Global_reactive_power | Voltage | Global_intensity | Sub_metering_1 | Sub_metering_2 | Sub_metering_3
     # globalActivePowerData <- standardize("Global_active_power")
     # globalReactivePowerData <- standardize("Global_reactive_power")
     # voltageData <- standardize("Voltage")
     # globalIntensityData <- standardize("Global_intensity")
     # subMetering1Data <- standardize("Sub_metering_1")
     # subMetering2Data <- standardize("Sub_metering_2")
     # subMetering3Data <- standardize("Sub_metering_3")
     
     pca <- prcomp(timeWindowData[3:9], scale = TRUE)
     
     # #-------------Output PCA results (only run if you do not have the files-------------
     # sink ("PCAoutput.txt",append = TRUE)
     # cat("\n Day ", j," Time Window ", i, "\n")
     # sink()
     # sink ("PCAoutput.txt",append = TRUE)
     # print(summary(pca))
     # sink()
     # sink ("PCAoutput.txt",append = TRUE)
     # print(pca$rotation)
     # sink()
     
     fileName <- paste0("Day",j,"Window",i,".png")
     plotTitle <- paste("PCA graph for day ",j," Time window ",i)
     plot <- ggbiplot(pca, scale = .75, alpha = 0.1, var.scale = 1.0) +
       ggtitle(plotTitle)
     ggsave(fileName, plot = plot)
     
     #-------------------------------------------------------------------------------------
     i = i + 1
   }
   j = j + 1
 }
#---------------------------------------------------------------------------------------------

#------------------------------Calculating PCA for 7 Variables--------------------------------
pcaData <- rawData[3:9]
pca <- prcomp(pcaData, scale = TRUE)
summary(pca)


#----------------------------------Analyze PCA Results----------------------------------------
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
barplot(pca.var.per, main="Scree Plot", xlab = "Principal Component", ylab = "Percent Variation")

pca.data <- data.frame(Sample=rownames(pca$x), X=pca$x[,1], Y=pca$x[,2])

ggplot(data=pca.data, aes(x=X, y=Y, label=Sample)) + 
  geom_point() +
  xlab(paste("PC1- ",pca.var.per[1], "%", sep="")) + 
  ylab(paste("PC2- ",pca.var.per[2], "%", sep="")) + 
  ggtitle("PCA graph")

loading_scores = pca$rotation[,1]
score <- abs(loading_scores)
score_ranked <- sort(score, decreasing = TRUE)
top_7 <- names(score_ranked[1:7])
pca$rotation[top_7, 1]
