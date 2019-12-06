library(ggplot2)
library(reshape2)
library(caret)
library(dplyr)

columnsForCorrelation <- c("length", "cfin1",  "cfin2",  "chel1",  "chel2",
                           "lcop1",  "lcop2",  "fbar", "cumf",
                           "sst", "sal", "xmonth", "nao")

polishColumnsNames <- list(
                        "length" = "długość śledzia [cm]",
                        "cfin1" = "dostępność planktonu [Calanus finmarchicus gat. 1]",
                        "cfin2" = "dostępność planktonu [Calanus finmarchicus gat. 2]",
                        "chel1" = "dostępność planktonu [Calanus helgolandicus gat. 1]",
                        "chel2" = "dostępność planktonu [Calanus helgolandicus gat. 2]",
                        "lcop1" = "dostępność planktonu [widłonogi gat. 1]",
                        "lcop2" = "dostępność planktonu [widłonogi gat. 2]",
                        "fbar" = "natężenie połowów w regionie [ułamek pozostawionego narybku]",
                        "recr" = "roczny narybek [liczba śledzi]",
                        "cumf" = "łączne roczne natężenie połowów w regionie [ułamek pozostawionego narybku]",
                        "totaln" = "łączna liczba ryb złowionych w ramach połowu [liczba śledzi]",
                        "sst" = "temperatura przy powierzchni wody [°C]",
                        "sal" = "poziom zasolenia wody [Knudsen ppt]",
                        "xmonth" = "miesiąc połowu [numer miesiąca]",
                        "nao" = "oscylacja północnoatlantycka [mb]")

getDescriptionFromColumnLabel <- function(column)
{
  sapply(column, function(x){
    if(is.null(polishColumnsNames[[x]]))
      x
    else
      polishColumnsNames[[x]]
  })
}


loadData <- function(filename)
{
  read.csv(file = filename,
           header = TRUE,
           sep = ',',
           comment.char = "",
           na.strings = "?",
           colClasses = c("integer", "numeric", "numeric", "numeric", 
                          "numeric", "numeric", "numeric", 
                          "numeric", "numeric", "factor", 
                          "numeric", "numeric", "numeric", 
                          "numeric", "integer", "numeric"))
}

transformData <- function(rawData)
{
  columnsToPreserve <- c("length", "cfin1",  "cfin2",  "chel1",  "chel2",
                         "lcop1",  "lcop2",  "fbar",   "recr",   "cumf",
                         "totaln", "sst", "sal", "xmonth", "nao")
  data <- rawData[columnsToPreserve]

  # names(data) <- polishColumnsNames
  
  for(col in names(data))
  {
    if(!any(is.na(data[[col]])))
    {
      # skip columns without NA
      next
    }
    data[is.na(data[[col]]), col] <- mean(data[[col]], na.rm = TRUE)
  }
  
  data
}

addYearsColumns <- function(data)
{
  # add column with year (based on years' estimation)
  numberOfYears <- 60
  numberOfHerringsInOneYear <- nrow(data) %/% (numberOfYears-1)
  dataWithYears <- data %>%
    mutate(year = 1:n() %/% numberOfHerringsInOneYear + 1)
  
  # add column with year (based on recr)
  years_distinct <- getYearsDistinct(data)
  dataWithYears <- mutate(dataWithYears, year_=assignYear(recr, years_distinct))
  
  dataWithYears
}

getYearsDistinct <- function(data)
{
  data %>% distinct(x=recr) %>% mutate(year=seq(1, length(x), 1))
}

assignYear <- function(recr, years)
{
  sapply(recr, function(x){
    val <- years[years["x"] == levels(x)[x], "year"]
    if(length(val) < 1)
      0
    else
    {
      val
    }  
  })
}


drawHistogram <- function(data, name)
{
  ggplot(data, aes(x=name)) +
    geom_histogram()
}

dataSummary <- function(data)
{
  print("Dimensions of dataset:")
  print(dim(data))
  
  summary(data)
  
  ggplot(data, aes(x=length, color = factor(xmonth))) +
    geom_density() +
    scale_x_continuous(breaks = seq(min(data$length), max(data$length), 1)) +
    labs(title="Rozkład długości śledzia w zależności od miesiąca",
         x="Długość śledzia",
         y="Gęstość rozkładu",
         colour = "Miesiąc")
}

variablesAnalysis.lengthByYear_ <- function(data)
{
  meanByYear <- data %>% group_by(year_) %>% summarise_at(vars(length), list(mean, sd))
  ggplot(meanByYear, aes(x=year_, y=fn1)) +
    geom_line(linetype = "dashed") +
    geom_pointrange(aes(min=fn1-fn2, max=fn1+fn2), color="darkseagreen3") +
    geom_point()  +
    labs(title="Rozkład długości śledzia w zależności od roku",
         subtitle = "Agregacja na podstawie atrybutu `recr`",
         x="Rok",
         y="Długość śledzia [cm]")
}

variablesAnalysis.lengthByYear <- function(data)
{
  meanByYear <- data %>% group_by(year) %>% summarise_at(vars(length), list(mean, sd))
  ggplot(meanByYear, aes(x=year, y=fn1)) +
    geom_line(linetype = "dashed") +
    geom_pointrange(aes(min=fn1-fn2, max=fn1+fn2), color="darkseagreen3") +
    geom_point()  +
    labs(title="Rozkład długości śledzia w zależności od roku",
         subtitle = "Agregacja na podstawie założenia o chronologii danych.",
         x="Rok",
         y="Długość śledzia [cm]")
}

variablesAnalysis.lengthAll <- function(data)
{
  ggplot(data, aes(x=seq(1, nrow(data)), y=length)) +
    geom_line(color="darkseagreen3")  +
    labs(title="Długość śledzia",
         subtitle="Bez agregacji, wszystkie dane.",
         x="Nr śledzia",
         y="Długość śledzia [cm]")
}

getUpperMatrix <- function(matrix){
  matrix[lower.tri(matrix)] <- NA
  matrix
}

reorderCorrmat <- function(corrmat){
  # Use correlation between variables as distance
  dd <- as.dist((1-corrmat)/2)
  hc <- hclust(dd)
  corrmat <- corrmat[hc$order, hc$order]
}

variableAnalysis.calculateCorrelation <- function(data)
{
  corrmat <- round(cor(data[columnsForCorrelation]), 2)
  corrmat
}

variableAnalysis.drawCorrelationPlot <- function(data)
{
  corrmat <- variableAnalysis.calculateCorrelation(data)
  
  # reorder corrmat to find some hidden patterns
  # use hclust for hierarchical clustering order
  corrmat <- reorderCorrmat(corrmat)
  
  upperCorrmat <- getUpperMatrix(corrmat)
  melted_cormat <- melt(upperCorrmat, na.rm = TRUE)
  
  ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Korelacja\nPearsona") +
    theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1)) +
    coord_fixed() +
    labs(title = "Macierz korelacji zmiennych",
         x="", y="") +
    geom_text(aes(Var2, Var1, label = value), color = "black", size = 3)
}

variableAnalysis.getTooCorrelatedColumns <- function(data, cutoff = .75)
{
  corrmat <- variableAnalysis.calculateCorrelation(data)
  highlyCorrelated <- findCorrelation(corrmat, cutoff=cutoff)
  columnsForCorrelation[highlyCorrelated]
}

featureSelection.rankImportance <- function(data)
{
  control <- trainControl(method="repeatedcv", number=1, repeats=1, allowParallel = TRUE)
  model <- train(length ~ .,  data=data)
  importance <- varImp(model, scale=FALSE)
  print(importance)
  plot(importance)
  
}

variableAnalysis.drawNumericChart <- function(data, colName, yDesc) {
  ggplot(data, aes(x = length, y = data[[colName]], color=xmonth)) +
    geom_jitter(size = 1.5, stat = "identity") +
    labs(x = "Długość", y = yDesc, color='Miesiąc') + 
    theme_bw()
}

variableAnalysis.drawDifferentCharts <- function(data)
{
  attributes <- c('chel1', 'fbar', 'sst', 'nao')
  list <- lapply(attributes, function(x){
    variableAnalysis.drawNumericChart(data, x, getDescriptionFromColumnLabel(x))
  })
  plot_grid(
    plotlist = list
  )
}

variableAnalysis.removeColumns <- function(data, columnsToRemove)
{
  select(data,-columnsToRemove)
}

predictions.prepareData <- function(data)
{
  set.seed(13)
  trainTestSeries <- createDataPartition(data$length, p=0.7, list=FALSE)
  training <- data[trainTestSeries,]
  testing <- data[-trainTestSeries,]
  
  print("Zbiór treningowy")
  print(dim(training))
  
  print("Zbiór testowy")
  print(dim(testing))
  
  print(class(training))
  x.train = training[, -training$length]
  y.train = training$length
  x.test = testing[, -testing$length]
  y.test = testing$length
  
  list(x.train = x.train,
       y.train = y.train,
       x.test = x.test,
       y.test = y.test)
}

predictions.trainControl <- function(data)
{
  trainControl(method="cv",
               number=5,
               repeats=2,
               allowParallel = TRUE,
               verboseIter = TRUE)
}

predictions.knn <- function(predictionData)
{
  knnGrid <- expand.grid(k = seq(1, 31, by = 2)) 
  knnModel <- train(
    predictionData$x.train[1000:2000, ], predictionData$y.train[1000:2000],
    method = "knn",
    trControl = predictions.trainControl(),
    tuneGrid = knnGrid,
    preProcess = c("center", "scale"),
    verbose=TRUE
  )
}

predictions.linearSVM <- function(predictionData)
{
  naiveBayesModel <- train(
    predictionData$x.train[1000:2000, ], predictionData$y.train[1000:2000],
    method = "svmLinear",
    trControl = predictions.trainControl(),
    verbose=TRUE
  )
}

predictions.randomForest <- function(predictionData)
{
  randomForestModel <- train(
    predictionData$x.train[1000:2000, ], predictionData$y.train[1000:2000],
    method = "ranger",
    trControl = predictions.trainControl(),
    verbose=TRUE
  )
}

predictions.xGradientBoosting <- function(predictionData)
{
  extremeGradientBoostingGrid <- expand.grid(
    nrounds = c(100,200), 
    max_depth = c(10, 15, 20, 25),
    colsample_bytree = seq(0.5, 0.9, length.out = 5),
    eta = 0.1,
    gamma=0,
    min_child_weight = 1,
    subsample = 1
  )
  extremeGradientBoostingModel <- train(
    predictionData$x.train[1000:2000, ], predictionData$y.train[1000:2000],
    trControl = predictions.trainControl(),
    tuneGrid = extremeGradientBoostingGrid,
    method = "xgbTree"
  )
}


predictions.neuralNetwork <- function(predictionData)
{
  neuralNetworkGrid <- expand.grid(
    size = c(32, 64, 32)
  )
  neuralNetworkModel <- train(
    predictionData$x.train[1000:2000, ], predictionData$y.train[1000:2000],
    method = "pcaNNet",
    trControl = predictions.trainControl(),
    tuneGrid = neuralNetworkGrid,
    verbose=TRUE
  )
}
