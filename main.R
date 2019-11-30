library(dplyr)

source('src/functions.R')

herringsFilename <- 'files/herrings.csv'
rawData <- loadData(herringsFilename)

rawData <- rawData[500:530, ]

herringsData <- transformData(rawData)
