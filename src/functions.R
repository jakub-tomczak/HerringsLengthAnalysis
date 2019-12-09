library(ggplot2)
library(reshape2)
library(caret)
library(dplyr)
library(cowplot)

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
  #model <- train(length ~ .,  data=data)
  #importance <- varImp(model, scale=FALSE)
  #print(importance)
  #plot(importance)
  
}

variableAnalysis.drawNumericChart <- function(data, colName, yDesc=NULL) {
  yDesc <- if(is.null(yDesc))
            colName
          else
            yDesc
  ggplot(data, aes(x = length, y = data[[colName]], color=xmonth)) +
    geom_jitter(size = 1.5, stat = "identity") +
    labs(x = "Długość", y = yDesc, color='Miesiąc') + 
    theme_bw()
}

variableAnalysis.drawDifferentCharts <- function(data)
{
  attributes <- c('lcop1', 'fbar', 'sst', 'nao')
  plots <- lapply(attributes, function(x){
    if(!is.null(data[[x]]))
      variableAnalysis.drawNumericChart(data, x)
  })
  plot_grid(plotlist=plots)
}

variableAnalysis.drawHistograms <- function(data)
{
  plots <- lapply(colnames(data), function(col)
  {
    if(is.factor(data[[col]]))
    {
      ggplot(data, aes(x=data[[col]])) + 
        geom_histogram(stat="count") +
        labs(title=col, x="wartości")  
    }
    else
    {
      ggplot(data, aes(x=data[[col]])) + 
        geom_histogram() +
        labs(title=col, x="wartości", y="rozkład wartości")
    }
  })
  
  plot_grid(plotlist=plots)
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
  
  list(train = training,
       test = testing)
}


predictions.trainControl <- function(number = 10)
{
  trainControl(method="cv",
               number=number,
               allowParallel = TRUE,
               verboseIter = FALSE)
}

predictions.lm <- function(predictionData)
{
  lmModel <- train(
    length~.,
    data=predictionData$train[1:1000,],
    method="lm"
  )
}

predictions.lasso <- function(predictionData)
{
  lmModel <- train(
    length~.,
    data=predictionData$train[1:1000,],
    method="lasso"
  )
}

predictions.ridge <- function(predictionData)
{
  ridgeGrid <- expand.grid(lambda=.96) 
  lmModel <- train(
    length~.,
    data=predictionData$train[1:1000,],
    method="ridge",
    tuneGrid = ridgeGrid
  )
}

predictions.knn <- function(predictionData)
{
  knnGrid <- expand.grid(k = seq(1, 10, by = 2)) 
  knnModel <- train(
    length~.,
    #data=predictionData$train,
    data=predictionData$train[1:1000,],
    method = "knn",
    trControl = predictions.trainControl(),
    tuneGrid = knnGrid,
    preProcess = c("center", "scale"),
    verbose=FALSE
  )
}

predictions.linearSVM <- function(predictionData)
{
  naiveBayesModel <- train(
    length~.,
    # data=predictionData$train,
    data=predictionData$train[1:1000,],
    method = "svmLinear",
    trControl = predictions.trainControl(),
    verbose=FALSE
  )
}

predictions.randomForest <- function(predictionData)
{
  randomForestModel <- train(
    length~.,
    # data=predictionData$train,
    data=predictionData$train[1:1000,],
    method = "ranger",
    trControl = predictions.trainControl(),
    verbose=FALSE
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
    length~.,
    # data=predictionData$train,
    data=predictionData$train[1:1000,],
    trControl = predictions.trainControl(),
    tuneGrid = extremeGradientBoostingGrid,
    method = "xgbTree"
  )
}


predictions.neuralNetwork <- function(predictionData)
{
  neuralNetworkGrid <- expand.grid(
    size = c(11, 16, 32, 64, 32),
    decay=.95
  )
  neuralNetworkModel <- train(
    length~.,
    # data=predictionData$train,
    data=predictionData$train[1:1000,],
    method = "pcaNNet",
    trControl = predictions.trainControl(),
    tuneGrid = neuralNetworkGrid,
    verbose=FALSE
  )
}

predictions.getStats <- function(predictions, predictionData)
{
  y <- predictions
  y_true <- select(predictionData$test, length)
  y_mean <- mean(y_true$length)
  residuals <- (y_true - y)
  
  rmse <- sqrt(mean(residuals$length**2))
  ss.tot <- sum((y_true$length - y_mean)**2)
  ss.res <- sum(residuals**2)
  r.squared <- 1 - ss.res / ss.tot
  
  list(rmse = rmse,
       r.squared = r.squared)
}

predictions.predictionChart <- function(predictions, model.name, predictionData) {
  x <- select(predictionData$test, -c(length))
  results <- predictionData$test %>%
    select(length) %>%
    rename(y_true = length)
  results$y_predicted <- predictions
  # Plot predictions vs test data
  ggplot(results, aes(y = y_true, x = y_predicted)) +
    geom_point(color = "darkred", alpha = 0.5) + 
    geom_smooth(method=lm) +
    labs(title = paste("model", model.name),
         x="Rzeczywista długość", y="Przewidziana długość")
}

predictions.importanceCharts <- function(method)
{
  ggplot(varImp(method$model)) +
    labs(title=method$name, x="Ważność atrybutu", y="Atrybut")
}

predictions.plotPredictedValues <- function(method, predictionData)
{
  data <- data.frame(select(predictionData$test, length), predictions = method$predictions)
  ggplot(data, aes(y = length, x = predictions)) +
    geom_point(color = "darkgreen", alpha = 0.5) + 
    geom_smooth(method=lm) +
    labs(title = paste("model", method$name),
         x="Rzeczywista długość", y="Przewidziana długość")
}

predictions.getPredictions <- function(model, predictionData)
{
  y_true <- select(predictionData$test, length)
  predict(model, select(predictionData$test, -c(length)))
}

predictions.runMethods <- function(methods, predictionData)
{
  methodsWithPredictions <- lapply(methods, function(method)
  {
    model <- method$fun(predictionData)
    predictions <- predictions.getPredictions(model, predictionData)
    importances <- varImp(model, scale=FALSE)
    list(
      name = method$name,
      model = model,
      predictions = predictions,
      stats = predictions.getStats(predictions, predictionData),
      importances = importances
    )
  })
  methodsWithPredictions
}
