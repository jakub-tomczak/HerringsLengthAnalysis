---
title: "Raport z analizy długości śledzia"
author: "Jakub Tomczak"
date: "1 grudnia 2019"
output: html_document
---



## Cel analizy
Celem analizy jest określenie jakie mogą być główne przyczyny stopniowego zmniejszania się długości śledzi oceanicznych wyławianych w Europie.

## Zbiór danych
Na przestrzeni ostatnich lat zauważono stopniowy spadek rozmiaru śledzia oceanicznego wyławianego w Europie. Do analizy zebrano pomiary śledzi i warunków w jakich żyją z ostatnich 60 lat. Dane były pobierane z połowów komercyjnych jednostek. W ramach połowu jednej jednostki losowo wybierano od 50 do 100 sztuk trzyletnich śledzi. Zbiór danych zawiera 52582 rekordy, każdy z nich posiada 16 atrybutów:
* indeks
* długość złowionego śledzia [cm],
* dostępność planktonu [zagęszczenie Calanus finmarchicus gat. 1],
* dostępność planktonu [zagęszczenie Calanus finmarchicus gat. 2],
* dostępność planktonu [zagęszczenie Calanus helgolandicus gat. 1],
* dostępność planktonu [zagęszczenie Calanus helgolandicus gat. 2],
* dostępność planktonu [zagęszczenie widłonogów gat. 1],
* dostępność planktonu [zagęszczenie widłonogów gat. 2],
* natężenie połowów w regionie [ułamek pozostawionego narybku],
* roczny narybek [liczba śledzi],
* łączne roczne natężenie połowów w regionie [ułamek pozostawionego narybku],
* łączna liczba ryb złowionych w ramach połowu [liczba śledzi],
* temperatura przy powierzchni wody [°C],
* poziom zasolenia wody [Knudsen ppt],
* miesiąc połowu [numer miesiąca],
* oscylacja północnoatlantycka [mb]

Brakujące dane zostały oznaczone jako `?` w oryginalnym zbiorze danych.


## Wykorzystane biblioteki

```r
library(ggplot2)
library(dplyr)
```


## Ładowanie danych

Dane zostały załadowane z podaniem typów kolumn oraz wskazaniem, że brakujące dane są oznaczone przez `?`.

```r
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
herringsFilename <- 'files/herrings.csv'
rawData <- loadData(herringsFilename)
```

## Wstępne przetwarzanie danych

Wstępne przetwarzanie danych obejmowało usunięcie pierwszego atrybutu `indeks` oraz zastąpienie danych brakujących przez średnią. 
Dla czytelności, oryginalne nazwy zostały zastąpione ich polskimi opisami.
Wstępne przetwarzanie danych zostało wykonane za pomocą kodu:


```r
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
herringsData <- transformData(rawData)
```

## Podsumowanie zbioru danych

W celu podsumowania zbioru danych możemy posłużyć się następującą funkcją:

```r
summary(herringsData)
```

```
##  długość śledzia [cm] dostępność planktonu [Calanus finmarchicus gat. 1]
##  Min.   :19.0         Min.   : 0.0000                                   
##  1st Qu.:24.0         1st Qu.: 0.0000                                   
##  Median :25.5         Median : 0.1333                                   
##  Mean   :25.3         Mean   : 0.4458                                   
##  3rd Qu.:26.5         3rd Qu.: 0.3603                                   
##  Max.   :32.5         Max.   :37.6667                                   
##  dostępność planktonu [Calanus finmarchicus gat. 2]
##  Min.   : 0.0000                                   
##  1st Qu.: 0.2778                                   
##  Median : 0.7012                                   
##  Mean   : 2.0248                                   
##  3rd Qu.: 1.9973                                   
##  Max.   :19.3958                                   
##  dostępność planktonu [Calanus helgolandicus gat. 1]
##  Min.   : 0.000                                     
##  1st Qu.: 2.469                                     
##  Median : 6.083                                     
##  Mean   :10.006                                     
##  3rd Qu.:11.500                                     
##  Max.   :75.000                                     
##  dostępność planktonu [Calanus helgolandicus gat. 2]
##  Min.   : 5.238                                     
##  1st Qu.:13.589                                     
##  Median :21.435                                     
##  Mean   :21.221                                     
##  3rd Qu.:27.193                                     
##  Max.   :57.706                                     
##  dostępność planktonu [widłonogi gat. 1]
##  Min.   :  0.3074                       
##  1st Qu.:  2.5479                       
##  Median :  7.1229                       
##  Mean   : 12.8108                       
##  3rd Qu.: 21.2315                       
##  Max.   :115.5833                       
##  dostępność planktonu [widłonogi gat. 2]
##  Min.   : 7.849                         
##  1st Qu.:17.808                         
##  Median :25.338                         
##  Mean   :28.419                         
##  3rd Qu.:37.232                         
##  Max.   :68.736                         
##  natężenie połowów w regionie [ułamek pozostawionego narybku]
##  Min.   :0.0680                                              
##  1st Qu.:0.2270                                              
##  Median :0.3320                                              
##  Mean   :0.3304                                              
##  3rd Qu.:0.4560                                              
##  Max.   :0.8490                                              
##  roczny narybek [liczba śledzi]
##  Min.   : 140515               
##  1st Qu.: 360061               
##  Median : 421391               
##  Mean   : 520367               
##  3rd Qu.: 724151               
##  Max.   :1565890               
##  łączne roczne natężenie połowów w regionie [ułamek pozostawionego narybku]
##  Min.   :0.06833                                                           
##  1st Qu.:0.14809                                                           
##  Median :0.23191                                                           
##  Mean   :0.22981                                                           
##  3rd Qu.:0.29803                                                           
##  Max.   :0.39801                                                           
##  łączna liczba ryb złowionych w ramach połowu [liczba śledzi]
##  Min.   : 144137                                             
##  1st Qu.: 306068                                             
##  Median : 539558                                             
##  Mean   : 514973                                             
##  3rd Qu.: 730351                                             
##  Max.   :1015595                                             
##  temperatura przy powierzchni wody [°C]
##  Min.   :12.77                         
##  1st Qu.:13.63                         
##  Median :13.86                         
##  Mean   :13.87                         
##  3rd Qu.:14.16                         
##  Max.   :14.73                         
##  poziom zasolenia wody [Knudsen ppt] miesiąc połowu [numer miesiąca]
##  Min.   :35.40                       Min.   : 1.000                 
##  1st Qu.:35.51                       1st Qu.: 5.000                 
##  Median :35.51                       Median : 8.000                 
##  Mean   :35.51                       Mean   : 7.258                 
##  3rd Qu.:35.52                       3rd Qu.: 9.000                 
##  Max.   :35.61                       Max.   :12.000                 
##  oscylacja północnoatlantycka [mb]
##  Min.   :-4.89000                 
##  1st Qu.:-1.89000                 
##  Median : 0.20000                 
##  Mean   :-0.09236                 
##  3rd Qu.: 1.63000                 
##  Max.   : 5.08000
```
