library(shiny)
library(dplyr)
source("src/functions.R")

herringsFilename <- 'files/herrings.csv'
herringImageFile <-"img/herring.png"
data <- transformData(loadData(herringsFilename))

meanByYear <- data %>% group_by(year) %>% summarise_at(vars(length), mean)

scaleHerringSizeForUI <- function(size)
{
  size*20
}

getLengthFromYear <- function(year)
{
  scaleHerringSizeForUI(meanByYear[year, 2])
}

createYearPlot <- function(year_point)
{
  ggplot(meanByYear, aes(x=year, y=length)) +
    geom_line(linetype = "dashed") +
    geom_point(aes(x=year_point, y=as.numeric(meanByYear[year_point, 2])), colour="red", size=5)  +
    labs(title="Rozkład długości śledzia w zależności od roku",
         subtitle = "Agregacja na podstawie założenia o chronologii danych.",
         x="Rok",
         y="Długość śledzia [cm]")
}

getExtremeHerringSize <- function(minMaxFunction)
{
  length <- minMaxFunction(meanByYear$length)
  year <- meanByYear[meanByYear$length == length, 1]
  list(year=year, length=round(length, 2))
}

min.size <- getExtremeHerringSize(min)
max.size <- getExtremeHerringSize(max)

ui <-
  navbarPage("Analiza długości śledzia",
    tabPanel("Raport",
      includeMarkdown("README.md")
    ),
    tabPanel("Animacja długości śledzia",
      fluidPage(
        fluidRow(
         column(width=3,
                sliderInput("slider", label = "", min = min(data$year), max = max(data$year), value = min(data$year) + (max(data$year) - min(data$year))/2 )
         ),
         column(width=5,
                p("Aktualny śledź"),
                textOutput("herringYearText"),
                textOutput("herringSizeText"),
                uiOutput('herringImg')
         ),
         column(width=4,
                plotOutput("herringPlot")
         )
        ),
        fluidRow(
         column(offset=3, width=5,
                p(paste("Minimalny rozmiar, rok", min.size$year, ", rozmiar", min.size$length)),
                img(src = herringImageFile, width =  scaleHerringSizeForUI(min.size$length))
         )
        ),
        fluidRow(
         column(offset=3, width=5,
                p(paste("Maksymalny rozmiar, rok", max.size$year, ", rozmiar", max.size$length)),
                img(src = herringImageFile, width =  scaleHerringSizeForUI(max.size$length))
         )
        )
      )
    )
  )

server <- function(input, output, session) {
  year <- reactive(as.integer(input$slider))
  output$herringImg <- renderUI({
    img(src = herringImageFile, width = getLengthFromYear(year()))
  })
  output$herringYearText <- renderText({
    paste("Rok ", year())
  })
  output$herringSizeText <- renderText({
    paste("Rozmiar ", round(meanByYear[year(), 2], 2))
  })
  
  output$herringPlot <- renderPlot(createYearPlot(year()))
}

shinyApp(ui, server)