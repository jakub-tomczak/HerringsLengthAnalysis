library(ggplot2)
library(raster)
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
  
  for(col in names(data))
  {
    if(!any(is.na(data[[col]])))
    {
      # skip columns without NA
      # print(paste("Skipping column", col))
      next
    }
    
    print(paste("Substituting NA in column", col))
    
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
      geom_histogram()
  }
  #ggplot(data, aes(cfin1)) +
  #  geom_histogram(bins = 10) +
  #  labs(x = "oś x", y="oś y")
}