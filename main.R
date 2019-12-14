source("src/functions.R")

herringsFilename <- 'files/herrings.csv'
rawData <- loadData(herringsFilename)

herringsData <- transformData(rawData)
herringsDataYears <- addYearsColumns(herringsData)

dataSummary(herringsData)

variablesAnalysis.lengthAll(herringsDataYears)
variablesAnalysis.lengthByYear(herringsDataYears)
variablesAnalysis.lengthByYear_(herringsDataYears)

# convert column recr from factor to numeric values
herringsData$recr <- as.numeric(herringsData$recr)
# removes columns totaln and recr
variableAnalysis.drawCorrelationPlot(herringsData)

importantFeatures <- featureSelection.rankImportance(predictionData$train, floor(dim(rawData)[1]*.05))
print(importantFeatures)

columnsToRemove <- variableAnalysis.getTooCorrelatedColumns(herringsData)
print(columnsToRemove)
analysedHerringsData <- variableAnalysis.removeColumns(herringsData, columnsToRemove)

variableAnalysis.drawDifferentCharts(analysedHerringsData)
variableAnalysis.drawHistograms(analysedHerringsData)

predictionData <- predictions.prepareData(analysedHerringsData)

methods <- list(
  list(name="linear regression", fun=predictions.lm),
  list(name="lasso",fun=predictions.lasso),
  list(name="ridge",fun=predictions.ridge),
  list(name="ElasticNet",fun=predictions.elasticNet),
  list(name="knn",fun=predictions.knn),
  list(name="extreme gradient boosting", fun = predictions.xGradientBoosting))

methodsWithPredictions <- predictions.runMethods(methods, predictionData)

predictionCharts <- lapply(methodsWithPredictions, function(x)
{
  predictions.predictionChart(x$predictions, x$name, predictionData) 
})
plot_grid(plotlist = predictionCharts)


importancesCharts <- lapply(methodsWithPredictions, function(x)
{
  predictions.importanceCharts(x)
})
plot_grid(plotlist = importancesCharts)

stats <- predictions.aggregateStats(methodsWithPredictions)
predictions.plotStats(stats, "RMSE", "RMSE")
predictions.plotStats(stats, "R.Squared", "R^2")
predictions.plotStats(stats, "Time", "Czas wykonywania [s]")

# remind features selection analysis
print(importantFeatures)
print(names(importantFeatures$fit$coefficients[2:6]))
# "fbar"  "sal"   "sst"   "cfin2" "recr" 

####################################################
### second analysis, only with chosen attributes ###
####################################################

# top5 best attributes (based on all algorithms)
bestAttributes <- c("sst", "nao", "fbar", "sal", "cfin1", "length")
methods <- list(
  list(name="linear regression", fun=predictions.lm),
  list(name="lasso",fun=predictions.lasso),
  list(name="ridge",fun=predictions.ridge),
  list(name="ElasticNet",fun=predictions.elasticNet),
  list(name="extreme gradient boosting", fun = predictions.xGradientBoosting))

bestAttributesData <- herringsData[, bestAttributes]
bestPredictionData <- predictions.prepareData(bestAttributesData)
methodsWithPredictionsBestData <- predictions.runMethods(methods, bestPredictionData)
bestImportancesCharts <- lapply(methodsWithPredictionsBestData, function(x)
{
  predictions.importanceCharts(x)
})
plot_grid(plotlist = bestImportancesCharts)

bestStats <- predictions.aggregateStats(methodsWithPredictionsBestData)
predictions.plotStats(bestStats, "RMSE", "RMSE")
predictions.plotStats(bestStats, "R.Squared", "R^2")
predictions.plotStats(bestStats, "Time", "Czas wykonywania [s]")

####################################################
### length by year vs attributes from xgb ##########
####################################################

mean_by_year <- herringsDataYears %>%
                  select(c(bestAttributes, "year")) %>%
                  group_by(year) %>%
                  summarise_all(mean)

variablesAnalysis.lengthByYearVsVariable(mean_by_year, "sst")
variablesAnalysis.lengthByYearVsVariable(mean_by_year, "nao")
variablesAnalysis.lengthByYearVsVariable(mean_by_year, "fbar")
variablesAnalysis.lengthByYearVsVariable(mean_by_year, "sal")
variablesAnalysis.lengthByYearVsVariable(mean_by_year, "cfin1")
