library(ggplot2)

loadData <- function(filename)
{
  read.csv(file = filename,
           header = TRUE,
           sep = ',',
           comment.char = "",
           na.strings = "?",
           colClasses = c("integer", "numeric", "numeric", "numeric", 
                          "numeric", "numeric", "numeric", 
                          "numeric", "numeric", "integer", 
                          "numeric", "numeric", "numeric", 
                          "numeric", "integer", "numeric"))
}

transformData <- function(rawData)
{
  columnsToPreserve <- c("length", "cfin1",  "cfin2",  "chel1",  "chel2",
                         "lcop1",  "lcop2",  "fbar",   "recr",   "cumf",
                         "totaln", "sst", "sal", "xmonth", "nao")
  data <- rawData[columnsToPreserve]
  
  polishColumnsNames <- c("długość śledzia [cm]",
                          "dostępność planktonu [Calanus finmarchicus gat. 1]",
                          "dostępność planktonu [Calanus finmarchicus gat. 2]",
                          "dostępność planktonu [Calanus helgolandicus gat. 1]",
                          "dostępność planktonu [Calanus helgolandicus gat. 2]",
                          "dostępność planktonu [widłonogi gat. 1]",
                          "dostępność planktonu [widłonogi gat. 2]",
                          "natężenie połowów w regionie [ułamek pozostawionego narybku]",
                          "roczny narybek [liczba śledzi]",
                          "łączne roczne natężenie połowów w regionie [ułamek pozostawionego narybku]",
                          "łączna liczba ryb złowionych w ramach połowu [liczba śledzi]",
                          "temperatura przy powierzchni wody [°C]",
                          "poziom zasolenia wody [Knudsen ppt]",
                          "miesiąc połowu [numer miesiąca]",
                          "oscylacja północnoatlantycka [mb]")
  
  names(data) <- polishColumnsNames
  
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

drawHistogram <- function(data, name)
{
  ggplot(data, aes(x=name)) +
    geom_histogram()
}

dataSummary <- function(data)
{
  print("Dimensions of dataset:")
  print(dim(data))
  
  for(col in names(data))
  {
    ggplot(data, aes(sym(col))) +
      geom_histogram() +
      labs(x = "oś x", y="oś y")
  }
}