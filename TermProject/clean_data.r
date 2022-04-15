#Only run if you don't have the "CleanedData.txt" file
rawData <- read.table("TermProjectData.txt", sep = ',', header = TRUE)

variables <- c("Global_active_power","Global_reactive_power","Global_intensity","Voltage", "Sub_metering_1","Sub_metering_2","Sub_metering_3")
col = 1

while (col <= length(variables)){
  variables[col]
  row = 1
  default = mean(rawData[[variables[col]]],na.rm = TRUE)
  
  while (row <= nrow(rawData))  {
    if(is.na(rawData[row, variables[col]])){
      rawData[row, variables[col]] = default
    }
    else{
      default = rawData[row, variables[col]]
    }
    row = row + 1
  }
  col = col + 1
}

write.table(rawData, "CleanedData.txt", sep=",")