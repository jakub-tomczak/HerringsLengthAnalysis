source("src/functions.R")

herringsFilename <- 'files/herrings.csv'
rawData <- loadData(herringsFilename)

herringsData <- transformData(rawData)
herringsDataYears <- addYearsColumns(herringsData)

dataSummary(herringsData)

variablesAnalysis.lengthAll(herringsDataYears)
variablesAnalysis.lengthByYear(herringsDataYears)
variablesAnalysis.lengthByYear_(herringsDataYears)

# removes columns totaln and recr
variableAnalysis.drawCorrelationPlot(herringsData)

columnsToRemove <- variableAnalysis.getTooCorrelatedColumns(herringsData)
analysedHerringsData <- variableAnalysis.removeColumns(herringsData, columnsToRemove)

featureSelection.rankImportance(analysedHerringsData)

variableAnalysis.drawDifferentCharts(analysedHerringsData)

predictionData <- predictions.prepareData(analysedHerringsData)

methods <- c("knn"=predictions.knn,
             "random forest"=predictions.randomForests,
             "extreme gradient boosting", predictions.xGradientBoosting)
results <- lapply(methods, function(x){
  x(predictionData)
})

