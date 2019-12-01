source("src/functions.R")

herringsFilename <- 'files/herrings.csv'
rawData <- loadData(herringsFilename)

herringsData <- transformData(rawData)

dataSummary(herringsData)

