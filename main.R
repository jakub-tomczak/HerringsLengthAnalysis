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
print(columnsToRemove)
analysedHerringsData <- variableAnalysis.removeColumns(herringsData, columnsToRemove)

variableAnalysis.drawDifferentCharts(analysedHerringsData)
variableAnalysis.drawHistograms(analysedHerringsData)

predictionData <- predictions.prepareData(analysedHerringsData)

methods <- list(
  list(name="linear regression", fun=predictions.lm),
  list(name="knn",fun=predictions.knn),
  list(name="neural network", fun = predictions.neuralNetwork),
  list(name="extreme gradient boosting", fun = predictions.xGradientBoosting))

methodsWithPredictions <- predictions.runMethods(methods, predictionData)

predictionCharts <- lapply(methodsWithPredictions, function(x)
{
  predictions.predictionChart(x$predictions, x$name, predictionData) 
})
plot_grid(plotlist = predictionCharts)


importancesCharts <- lapply(methodsWithPredictions, function(x)
{
  prediction.importanceChart(x$importances)
})
plot_grid(plotlist = importancesCharts)

stats <- lapply(methodsWithPredictions, function(x){ x$stats })
combineStats <- data.frame(matrix(unlist(stats), ncol=2, byrow = T))
