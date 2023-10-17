library(shiny)
library(ggplot2)
library(MASS)
library(forecast)
library(gridExtra)
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

# Carga de la base de datos
AEP_hourly<-read.csv("AEP_hourly.csv")
AEP_hourly$Datetime<-as.POSIXct(AEP_hourly$Datetime, format = "%Y-%m-%d %H:%M:%S")
AEP_hourly$fecha<-as.Date(AEP_hourly$Datetime)

energia <- AEP_hourly %>%
  group_by(fecha) %>%
  summarise(Energia = sum(AEP_MW))
energia<-energia[-5055,]
energia2<-ts(energia$Energia,start=c(2004,10,01),frequency=365.25)

# Define la interfaz de usuario

ui <- fluidPage(
  titlePanel(HTML("<h1 style='color: blue;'>Transformación Box-Cox de la Serie Energía.</h1>")),
  sidebarLayout(
    sidebarPanel(
      sliderInput("lambda", "Valor de Lambda:",
                  min = -2, max = 2, value = 0, step = 0.1)
    ),
    mainPanel(
      plotOutput("time_series_plot"),
      plotOutput("time_series_plot_original")
      
    )
  )
)

# Define la función para generar la serie de tiempo transformada
transformed_series <- function(lambda) {
  # Aplicar la transformación Box-Cox
  transformed_data <- forecast::BoxCox(energia2,lambda=lambda)
  
  return(transformed_data)
}

# Define la función para generar el gráfico
server <- function(input, output) {
  output$time_series_plot <- renderPlot({
    lambda <- input$lambda
    transformed_data <- transformed_series(lambda)
    
    plot1<-ggplot() +
      geom_line(aes(x = time(transformed_data), y = transformed_data),color = "purple") +
      labs(x = "Año", y = "Serie de Tiempo Transformada") +
      theme_minimal()
    
    plot2 <- ggplot() +
      geom_line(aes(x = time(energia2), y = energia2), color = "blue") +
      labs(x = "Año", y = "Serie de Tiempo Original") +
      theme_minimal()
    combined_plot <- grid.arrange(plot1, plot2, heights = c(2, 1))
    print(combined_plot)
  })
}

# Ejecutar la aplicación Shiny
shinyApp(ui, server)
