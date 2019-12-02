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
                          "numeric", "numeric", "factor", 
                          "factor", "numeric", "numeric", 
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
  
  # add column with year (based on years' estimation)
  numberOfYears <- 60
  numberOfHerringsInOneYear <- nrow(data) %/% (numberOfYears-1)
  data <- data %>%
    mutate(year = 1:n() %/% numberOfHerringsInOneYear + 1)
  
  # add column with year (based on recr)
  years_distinct <- getYearsDistinct(data)
  data <- mutate(data, year_=assignYear(recr, years_distinct))
  
  data
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