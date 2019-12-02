---
title: "Raport z analizy d³ugoœci œledzia"
author: "Jakub Tomczak"
date: "1 grudnia 2019"
output:
    html_notebook
---


# Raport z analizy d³ugoœci œledzia oceanicznego

## Cel analizy
Celem analizy jest okreœlenie jakie mog¹ byæ g³ówne przyczyny stopniowego zmniejszania siê d³ugoœci œledzi oceanicznych wy³awianych w Europie.

## Zbiór danych
Na przestrzeni ostatnich lat zauwa¿ono stopniowy spadek rozmiaru œledzia oceanicznego wy³awianego w Europie. Do analizy zebrano pomiary œledzi i warunków w jakich ¿yj¹ z ostatnich 60 lat. Dane by³y pobierane z po³owów komercyjnych jednostek. W ramach po³owu jednej jednostki losowo wybierano od 50 do 100 sztuk trzyletnich œledzi. Zbiór danych zawiera 52582 rekordy, ka¿dy z nich posiada 16 atrybutów:
* indeks
* d³ugoœæ z³owionego œledzia [cm],
* dostêpnoœæ planktonu [zagêszczenie Calanus finmarchicus gat. 1],
* dostêpnoœæ planktonu [zagêszczenie Calanus finmarchicus gat. 2],
* dostêpnoœæ planktonu [zagêszczenie Calanus helgolandicus gat. 1],
* dostêpnoœæ planktonu [zagêszczenie Calanus helgolandicus gat. 2],
* dostêpnoœæ planktonu [zagêszczenie wid³onogów gat. 1],
* dostêpnoœæ planktonu [zagêszczenie wid³onogów gat. 2],
* natê¿enie po³owów w regionie [u³amek pozostawionego narybku],
* roczny narybek [liczba œledzi],
* ³¹czne roczne natê¿enie po³owów w regionie [u³amek pozostawionego narybku],
* ³¹czna liczba ryb z³owionych w ramach po³owu [liczba œledzi],
* temperatura przy powierzchni wody [°C],
* poziom zasolenia wody [Knudsen ppt],
* miesi¹c po³owu [numer miesi¹ca],
* oscylacja pó³nocnoatlantycka [mb]

Brakuj¹ce dane zosta³y oznaczone jako `?` w oryginalnym zbiorze danych.


## Wykorzystane biblioteki

```r
library(ggplot2)
library(dplyr)
```


## £adowanie danych

Dane zosta³y za³adowane z podaniem typów kolumn oraz wskazaniem, ¿e brakuj¹ce dane s¹ oznaczone przez `?`.

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
                          "numeric", "factor", "numeric"))
}
herringsFilename <- 'files/herrings.csv'
rawData <- loadData(herringsFilename)
```

## Wstêpne przetwarzanie danych

Wstêpne przetwarzanie danych obejmowa³o usuniêcie pierwszego atrybutu `indeks` oraz zast¹pienie danych brakuj¹cych przez œredni¹. 
Dla czytelnoœci, oryginalne nazwy zosta³y zast¹pione ich polskimi opisami.

Oprócz tych przekszta³ceñ zosta³a dodana jeszcze jedna kolumna w której zosta³y zapisany rok, z któego pochodzi dany rekord. Aby wyznaczyæ rok wykorzystano kolumnê `recr`, która wskazuje roczny narybek. W ten sposób wytypowano 52 unikatowe rekordy (licz¹c unikatowe rekordy dla kolumny `cumf` równie¿ wysz³y 52 wartoœci) za pomoc¹ funkcji:


```r
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
```

W ten sposób dodajemy kolumnê o nazwie `year_`. Ze wzglêdu na fakt, i¿ dane mia³y byæ zapisane chronologicznie (co jest ma³o prawdopodobne patrz¹c na dane z kolumn `recr` oraz `cumf`) do ka¿dego rekordu jest dodawany rok na podstawie wartoœci z pola `recr`. Tak wyliczony rok jest dodany jako atrybut `year`.

Ostatecznie, wstêpne przetwarzanie danych zosta³o wykonane za pomoc¹ kodu:










