source("src/functions.R")

herringsFilename <- 'files/herrings.csv'
rawData <- loadData(herringsFilename)

####################################################
################ data transform ####################
####################################################

herringsData <- transformData(rawData)

herringsDataYears <- addYearsColumns(herringsData)
variablesAnalysis.lengthAll(herringsDataYears)
variablesAnalysis.lengthByYear(herringsDataYears)
variablesAnalysis.lengthByYear_(herringsDataYears)

# convert column recr from factor to numeric values
herringsData$recr <- as.numeric(herringsData$recr)

####################################################
################## data summary ####################
####################################################

dataSummary(herringsData)

####################################################
############# variables analysis ###################
####################################################

variableAnalysis.drawDifferentCharts(analysedHerringsData)
variableAnalysis.drawHistograms(analysedHerringsData)

####################################################
############### correlation analysis ###############
####################################################

variableAnalysis.drawCorrelationPlot(herringsData)

columnsToRemove <- variableAnalysis.getTooCorrelatedColumns(herringsData)
print(columnsToRemove)
columnsToRemove <- c(columnsToRemove, "totaln")
analysedHerringsData <- variableAnalysis.removeColumns(herringsData, columnsToRemove)

importantFeatures <- featureSelection.rankImportance(predictionData$train, floor(dim(rawData)[1]*.05))
print(importantFeatures)

####################################################
############ herrings length animation #############
####################################################

animation.run()

####################################################
################# models training ##################
####################################################

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
### third analysis, only with feature selected   ###
####################################################

# top5 best attributes (based on all algorithms)
# remind features selection analysis
print(importantFeatures)
featureSelection <- names(importantFeatures$fit$coefficients[2:6])
print(featureSelection)
# "fbar"  "sal"   "sst"   "cfin2" "recr" 

methods <- list(
  list(name="linear regression", fun=predictions.lm),
  list(name="lasso",fun=predictions.lasso),
  list(name="ridge",fun=predictions.ridge),
  list(name="ElasticNet",fun=predictions.elasticNet),
  list(name="extreme gradient boosting", fun = predictions.xGradientBoosting))

selectedAttributes <- herringsData[, c(featureSelection, 'length')]
selectedPredictionData <- predictions.prepareData(selectedAttributes)
methodsWithPredictionsSelectedData <- predictions.runMethods(methods, selectedPredictionData)
bestImportancesCharts <- lapply(methodsWithPredictionsSelectedData, function(x)
{
  predictions.importanceCharts(x)
})
plot_grid(plotlist = bestImportancesCharts)

selectedAttrStats <- predictions.aggregateStats(methodsWithPredictionsSelectedData)
predictions.plotStats(selectedAttrStats, "RMSE", "RMSE")
predictions.plotStats(selectedAttrStats, "R.Squared", "R^2")
predictions.plotStats(selectedAttrStats, "Time", "Czas wykonywania [s]")


####################################################
## length by year vs best and selected attributes ##
####################################################

mean_by_year <- herringsDataYears %>%
                  select(c(bestAttributes, "year", "cfin2")) %>%
                  group_by(year) %>%
                  summarise_all(mean)

variablesAnalysis.lengthByYearVsVariable(mean_by_year, "sst")
variablesAnalysis.lengthByYearVsVariable(mean_by_year, "nao")
variablesAnalysis.lengthByYearVsVariable(mean_by_year, "fbar")
variablesAnalysis.lengthByYearVsVariable(mean_by_year, "sal")
variablesAnalysis.lengthByYearVsVariable(mean_by_year, "cfin1")
# cfin2 is being picked by rfe
variablesAnalysis.lengthByYearVsVariable(mean_by_year, "cfin2")
