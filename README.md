---
title: "Raport z analizy d�ugo�ci �ledzia"
author: "Jakub Tomczak"
date: "1 grudnia 2019"
output:
    html_notebook
---


# Raport z analizy d�ugo�ci �ledzia oceanicznego

## Cel analizy
Celem analizy jest okre�lenie jakie mog� by� g��wne przyczyny stopniowego zmniejszania si� d�ugo�ci �ledzi oceanicznych wy�awianych w Europie.

## Zbi�r danych
Na przestrzeni ostatnich lat zauwa�ono stopniowy spadek rozmiaru �ledzia oceanicznego wy�awianego w Europie. Do analizy zebrano pomiary �ledzi i warunk�w w jakich �yj� z ostatnich 60 lat. Dane by�y pobierane z po�ow�w komercyjnych jednostek. W ramach po�owu jednej jednostki losowo wybierano od 50 do 100 sztuk trzyletnich �ledzi. Zbi�r danych zawiera 52582 rekordy, ka�dy z nich posiada 16 atrybut�w:
* indeks
* d�ugo�� z�owionego �ledzia [cm],
* dost�pno�� planktonu [zag�szczenie Calanus finmarchicus gat. 1],
* dost�pno�� planktonu [zag�szczenie Calanus finmarchicus gat. 2],
* dost�pno�� planktonu [zag�szczenie Calanus helgolandicus gat. 1],
* dost�pno�� planktonu [zag�szczenie Calanus helgolandicus gat. 2],
* dost�pno�� planktonu [zag�szczenie wid�onog�w gat. 1],
* dost�pno�� planktonu [zag�szczenie wid�onog�w gat. 2],
* nat�enie po�ow�w w regionie [u�amek pozostawionego narybku],
* roczny narybek [liczba �ledzi],
* ��czne roczne nat�enie po�ow�w w regionie [u�amek pozostawionego narybku],
* ��czna liczba ryb z�owionych w ramach po�owu [liczba �ledzi],
* temperatura przy powierzchni wody [�C],
* poziom zasolenia wody [Knudsen ppt],
* miesi�c po�owu [numer miesi�ca],
* oscylacja p�nocnoatlantycka [mb]

Brakuj�ce dane zosta�y oznaczone jako `?` w oryginalnym zbiorze danych.


## Wykorzystane biblioteki

```r
library(ggplot2)
library(dplyr)
```


## �adowanie danych

Dane zosta�y za�adowane z podaniem typ�w kolumn oraz wskazaniem, �e brakuj�ce dane s� oznaczone przez `?`.

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

## Wst�pne przetwarzanie danych

Wst�pne przetwarzanie danych obejmowa�o usuni�cie pierwszego atrybutu `indeks` oraz zast�pienie danych brakuj�cych przez �redni�. 
Dla czytelno�ci, oryginalne nazwy zosta�y zast�pione ich polskimi opisami.

Opr�cz tych przekszta�ce� zosta�a dodana jeszcze jedna kolumna w kt�rej zosta�y zapisany rok, z kt�ego pochodzi dany rekord. Aby wyznaczy� rok wykorzystano kolumn� `recr`, kt�ra wskazuje roczny narybek. W ten spos�b wytypowano 52 unikatowe rekordy (licz�c unikatowe rekordy dla kolumny `cumf` r�wnie� wysz�y 52 warto�ci) za pomoc� funkcji:


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

W ten spos�b dodajemy kolumn� o nazwie `year_`. Ze wzgl�du na fakt, i� dane mia�y by� zapisane chronologicznie (co jest ma�o prawdopodobne patrz�c na dane z kolumn `recr` oraz `cumf`) do ka�dego rekordu jest dodawany rok na podstawie warto�ci z pola `recr`. Tak wyliczony rok jest dodany jako atrybut `year`.

Ostatecznie, wst�pne przetwarzanie danych zosta�o wykonane za pomoc� kodu:










