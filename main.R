source("src/functions.R")

herringsFilename <- 'files/herrings.csv'
rawData <- loadData(herringsFilename)

smallData <- rawData[500:530, ]

herringsData <- transformData(rawData)

dataSummary(herringsData)

