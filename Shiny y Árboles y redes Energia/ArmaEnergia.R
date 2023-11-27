library(shiny)
library(TSstudio)
library(readxl)
library(dplyr)
library(lubridate)
library(astsa)
library(feasts)
library(fable)
library(timetk)
library(tsibble)
library(zoo)
library(xts)
library(readxl)
library(tidyverse)
library(nonlinearTseries)
library(tseriesChaos) 
library(forecast)
library(plotly)
library(dplyr)
library(lmtest)
library(parsnip)
library(rsample)
library(timetk)
library(modeltime)
library(tsibble)
library(tidymodels)
library(greybox)

# Define la ruta al archivo CSV
file_path <- "C:/Users/LENOVO/Desktop/Universidad/Series de tiempo/Series_de_tiempo/energia_esta.csv"  # Cambia esto a la ruta de tu archivo

# Interfaz de usuario
ui <- fluidPage(
  titlePanel("Modelo de la familia ARMA para la serie de tiempo de Energía"),
  br(),
  p("Esta aplicación permite modelar la serie de tiempo de la Energía usando modelos de la familia ARMA. Por favor selecciona el orden autorregresivo y de promedios móviles que consideras apropiado, puedes guiarte de la función de autocorrelación y autocorrelación parcial de la serie."),
  sidebarLayout(
    sidebarPanel(
      numericInput("p", "AR Orden (p)", min = 0, value = 1),
      numericInput("q", "MA Orden (q)", min = 0, value = 1),
      actionButton("go", "Modelar")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Acf y Pacf", 
                 p("Gráfico de la función de autocorrelación y autocorrelación parcial."),
                 plotOutput("plotACF"), plotOutput("plotPACF")),
        tabPanel("Residuales del modelo", 
                 p("Gráfico de los residuales del modelo y sus funciones de autocorrelación."),
                 plotOutput("residPlot"), plotOutput("residACF"), plotOutput("residPACF")),
        tabPanel("Cusum y Cusumsq",
                 p("Gráficos de CUSUM y CUSUM de cuadrados para evaluar la estabilidad de los parámetros del modelo."),
                 plotOutput("cusumPlot"), plotOutput("cusumsqPlot")),
        tabPanel("Coeficientes",
                 p("Tabla de coeficientes estimados para el modelo ARMA seleccionado."),
                 verbatimTextOutput("modelCoef"))
      )
    )
  )
)

# Servidor
server <- function(input, output) {
  # Carga el archivo CSV una sola vez al iniciar la aplicación
  series <- as.numeric(read.csv(file_path, header = TRUE)[,1])
  
  # Este reactive se activará cuando el usuario presione el botón 'Model'
  model <- eventReactive(input$go, {
    Arima(series, order = c(input$p, 0, input$q), include.mean = TRUE)
  })
  
  # Output para los coeficientes del modelo
  output$modelCoef <- renderPrint({
    req(model())  # Asegúrese de que el modelo se ha ejecutado
    coeftest(model())
  })
  
  output$plotACF <- renderPlot({
    acf(series,main="Acf de la serie de tiempo")
  })
  
  output$plotPACF <- renderPlot({
    pacf(series,main="Pacf de la serie de tiempo")
  })
  
  output$residPlot <- renderPlot({
    plot(model()$residuals,main="Gráfico de los residuales del modelo",
         ylab="Residuales",xlab="Tiempo")
  })
  
  output$residACF <- renderPlot({
    acf(model()$residuals,main="Acf de los residuales del modelo")
  })
  
  output$residPACF <- renderPlot({
    pacf(model()$residuals,main="Pacf de los residuales del modelo")
  })
  
  output$cusumPlot <- renderPlot({
    res <- model()$residuals
    cum <- cumsum(res) / sd(res)
    N <- length(res)
    Af <- 0.948 
    co <- 0.10997
    LS <- Af * sqrt(N) + 2 * Af * (1:N) / sqrt(N)
    LI <- -LS
    plot(cum, type="l", ylim=c(min(LI),max(LS)), xlab="t", ylab="", main="CUSUM")
    lines(LS, type="S", col="red")
    lines(LI, type="S", col="red")
  })
  
  output$cusumsqPlot <- renderPlot({
    res <- model()$residuals
    cumq <- cumsum(res^2) / sum(res^2)
    N <- length(res)
    co <- 0.10997
    LQS <- co + (1:N) / N
    LQI <- -co + (1:N) / N
    plot(cumq, type="l", xlab="t", ylab="", main="CUSUMSQ")
    lines(LQS, type="S", col="red")
    lines(LQI, type="S", col="red")
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)



