library(dplyr)
source("src/functions.R")

herringsFilename <- 'files/herrings.csv'
rawData <- loadData(herringsFilename)

herringsData <- transformData(rawData)

dataSummary(herringsData)

variablesAnalysis.lengthAll(herringsData)
variablesAnalysis.lengthByYear(herringsData)
variablesAnalysis.lengthByYear_(herringsData)
