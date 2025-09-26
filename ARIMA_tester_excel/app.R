#
# ARIMA testsuite
#
# Kleine toolbox voor de workshop op 28 oktober.
# Dit script geeft je een makkelijk overzicht van de verschillende opties bij het modelleren met ARIMA.
# 
# Benodigdheden:
# - R en Rstudio
# - De onderstaande libraries
# - Een Excelsheet met een export vanuit HPZone
#   -> Ga naar Query, kies een infection, klik op Run.
#   -> Klik bovenin op Show export options, selecteer Excel en All core values en klik op Download File
#   -> Plaats dit bestand in dezelfde map als het script, met een herkenbare naam.
#   -> Herhaal deze stappen voor alle gewenste infectieziektes.
#

#
# VANAF HIER NIKS AANPASSEN
#

pkg_nodig = c("shiny", "bslib", "tidyverse", "readxl", "forecast", "this.path")

for (pkg in pkg_nodig) {
  if (system.file(package = pkg) == "") {
    install.packages(pkg)
  }
}

library(shiny)
library(bslib)
library(tidyverse)
library(readxl)
library(forecast)
library(this.path)

setwd(dirname(this.path()))

datafiles = list.files(pattern=".*.xls(x|)")

ui <- fluidPage(
    titlePanel("ARIMA settings tester"),
    sidebarLayout(
        sidebarPanel(
            selectInput("databestand", "Dataset:", datafiles),
            sliderInput("startjaar", "Beginjaar:", min=2015, max=year(Sys.Date()), value=2020),
            sliderInput("p",
                        "AR (p)",
                        min=0,
                        max=10,
                        value=0),
            sliderInput("d",
                        "Differencing (d)",
                        min=0,
                        max=10,
                        value=0),
            sliderInput("q",
                        "MA (q)",
                        min=0,
                        max=10,
                        value=0),
            sliderInput("seasonality",
                        "Seasonality:",
                        min = 0,
                        max = 50,
                        value = 12),
            sliderInput("P",
                        "AR (P)",
                        min=0,
                        max=10,
                        value=0),
            sliderInput("D",
                        "Differencing (D)",
                        min=0,
                        max=10,
                        value=0),
            sliderInput("Q",
                        "MA (Q)",
                        min=0,
                        max=10,
                        value=0),
        ),

        mainPanel(
          navset_tab(nav_panel("Overzicht",
                               textOutput("title"),
                               plotOutput("lineView"),
                               plotOutput("tsOverview"),
                               plotOutput("tsOverviewDiff")),
                     nav_panel("Score",
                               layout_columns(
                                 card(card_header("auto.arima()"),
                                      verbatimTextOutput("ARIMAsummary"),
                                      plotOutput("residualsAuto"),
                                      plotOutput("predictAuto")),
                                 card(card_header("Handmatige selectie"),
                                      verbatimTextOutput("ARIMAsummaryManual"),
                                      plotOutput("residualsManual"),
                                      plotOutput("predictManual"))
                               )),
                     nav_panel("Voorspelling",
                               layout_columns(
                                 card(card_header("auto.arima()"),
                                      plotOutput("forecastAutoTraining"),
                                      plotOutput("forecastAuto")),
                                 card(card_header("Handmatige selectie"),
                                      plotOutput("forecastManualTraining"),
                                      plotOutput("forecastManual"))
                               )),
                     nav_panel("Vergelijking",
                               dataTableOutput("compTable"))
          )
        )
    )
)

server <- function(input, output) {
  
  data = reactive({
    data = read_excel(input$databestand)
    data = data %>%
      mutate(peildatum=coalesce(`Date of Onset`, `Datum melding aan de GGD`, `Time entered`),
             m = month(peildatum), y = year(peildatum)) %>%
      arrange(peildatum) %>%
      group_by(y, m) %>%
      summarize(n=n())
    data = data %>%
      ungroup() %>%
      filter(y >= input$startjaar) %>%
      complete(y, m) %>%
      filter(!is.na(m), !is.na(y),
             !(y >= year(Sys.Date()) & m > month(Sys.Date()))) %>%
      mutate(n=ifelse(is.na(n), 0, n),
             diff=c(NA, diff(n)),
             datum=ymd(paste0(y, "-", m, "-1")))
    
    return(data)
  })
  
  data.training = reactive({
    n = nrow(data())
    splitpoint = floor(n * 0.8)
    return(data()[1:splitpoint,])
  })
  
  data.test = reactive({
    n = nrow(data())
    splitpoint = floor(n * 0.8)
    return(data()[splitpoint:n,])
  })
  
  model.auto = reactive({
    model = auto.arima(data()$n, stepwise=F, approximation=F)
    return(model)
  })
  
  model.auto.training = reactive({
    model = auto.arima(data.training()$n, stepwise=F, approximation=F)
    return(model)
  })
  
  model.manual = reactive({
    if (any(c(input$P, input$D, input$Q) > 0) && input$seasonality > 0) {
      model = Arima(data()$n, order=c(input$p, input$d, input$q), seasonal=list(order=c(input$P, input$D, input$Q), period=input$seasonality))
    } else {
      model = Arima(data()$n, order=c(input$p, input$d, input$q))
    }
    return(model)
  })
  
  model.manual.training = reactive({
    if (any(c(input$P, input$D, input$Q) > 0) && input$seasonality > 0) {
      model = Arima(data.training()$n, order=c(input$p, input$d, input$q), seasonal=list(order=c(input$P, input$D, input$Q), period=input$seasonality))
    } else {
      model = Arima(data.training()$n, order=c(input$p, input$d, input$q))
    }
    return(model)
  })
  
  output$title = renderText({ input$groep })
  
  output$lineView <- renderPlot({
    ggplot(data(), aes(x=datum, y=n)) +
      geom_line()
  })
  
  output$tsOverview = renderPlot({
    ggtsdisplay(data()$n, main="Overzicht timeseries")
  })
  
  output$tsOverviewDiff = renderPlot({
    ggtsdisplay(diff(data()$n), main="Overzicht timeseries (diff)")
  })
  
  output$ARIMAsummary = renderPrint({
    model.auto()
  })
  
  output$residualsAuto = renderPlot({
    residuals(model.auto()) %>% ggtsdisplay(main="Residuals")
  })
  
  output$predictAuto = renderPlot({
    ggplot(data()) +
      geom_point(aes(x=datum, y=n)) +
      geom_line(aes(x=datum, y=model.auto()$fitted)) +
      labs(title="Overlap tussen datapunten en voorspelmodel")
  })
  
  output$predictManual = renderPlot({
    ggplot(data()) +
      geom_point(aes(x=datum, y=n)) +
      geom_line(aes(x=datum, y=model.manual()$fitted)) +
      labs(title="Overlap tussen datapunten en voorspelmodel")
  })
  
  output$forecastAutoTraining = renderPlot({
    autoplot(forecast(model.auto.training(), h=24)) +
      geom_point(aes(x=x, y=y), data=cbind(x=1:nrow(data()), y=data()$n)) +
      labs(title="Voorspellingen met 80% trainingsset")
  })
  
  output$forecastManualTraining = renderPlot({
    autoplot(forecast(model.manual.training(), h=24)) +
      geom_point(aes(x=x, y=y), data=cbind(x=1:nrow(data()), y=data()$n)) +
      labs(title="Voorspellingen met 80% trainingsset")
  })
  
  output$forecastAuto = renderPlot({
    autoplot(forecast(model.auto(), h=12)) +
      geom_point(aes(x=x, y=y), data=cbind(x=1:nrow(data()), y=data()$n)) +
      labs(title="Voorspellingen met alle data")
  })
  
  output$forecastManual = renderPlot({
    autoplot(forecast(model.manual(), h=12)) +
      geom_point(aes(x=x, y=y), data=cbind(x=1:nrow(data()), y=data()$n)) +
      labs(title="Voorspellingen met alle data")
  })
  
  output$ARIMAsummaryManual = renderPrint({
    model.manual()
  })
  
  output$residualsManual = renderPlot({
    residuals(model.manual()) %>% ggtsdisplay(main="Residuals")
  })
  
  output$compTable = renderDataTable({
    base_order = c(input$p, input$d, input$q)
    season_order = c(input$P, input$D, input$Q)
    orders = list(base_order)
    for (i in c(-1, 1, 2)) {
      if (all(base_order + c(i, 0, 0) >= 0))
        orders[[length(orders)+1]] = base_order + c(i, 0, 0)
      if (all(base_order + c(0, i, 0) >= 0))
        orders[[length(orders)+1]] = base_order + c(0, i, 0)
      if (all(base_order + c(0, 0, i) >= 0))
        orders[[length(orders)+1]] = base_order + c(0, 0, i)
    }
    orders = unique(orders) 
    
    results = tibble()
    for (o in orders) {
      model = Arima(data()$n, order=o, season=list(order=season_order, period=input$seasonality))
      
      acc = accuracy(model)
      results = bind_rows(results,
                          data.frame(model=paste0("(", str_c(o, collapse=", "), ") (", str_c(season_order, collapse=", "), ")"),
                                     AICc=round(model$aicc, digits=3),
                                     MAE=round(acc[,"MAE"], digits=3),
                                     RMSE=round(acc[,"RMSE"], digits=3)))
    }
    
    return(results)
  })
}

shinyApp(ui = ui, server = server)
