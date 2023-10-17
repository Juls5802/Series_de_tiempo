library(shiny)
library(TSstudio)
library(shinyjs)
library(ggplot2)
library(MASS)
library(readxl)
library(dplyr)
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

energia_1=energia %>% map_df(rev)
Fechas=as.Date(energia_1$fecha)
energia_xts=xts(x = energia_1$Energia,frequency = 365.25,order.by = Fechas)

# Creación objeto tssible a partir del objeto tibble
df_energia=data.frame(Energia=energia_1$Energia,fecha=energia_1$fecha)
tbl_energia=tibble(df_energia)
tbl_energia_format_fecha=tbl_energia
tsbl_energia=as_tsibble(tbl_energia_format_fecha,index=fecha)
# Define la interfaz de usuario
ui <- fluidPage(
  titlePanel(HTML("<h1 style='color: blue;'>Estimación de Tendencia por diferentes metodologías.</h1>")),
  sidebarLayout(
    sidebarPanel(
      h3("Para estimar la tendencia es posible utilizar diferentes estrategias
         tanto paramétricas como no paramétricas, presiona cada uno de los botones 
         para observar el gráfico que se obtiene con cada metodología."),
      actionButton("btn1", "Estimacion paramétrica"),
      actionButton("btn2", "Filtro de promedios móviles"),
      actionButton("btn3", "Diferenciación.")
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define la función para generar los gráficos
server <- function(input, output) {
  selected_option <- reactiveVal(NULL)
  
  observeEvent(input$btn1, {
    selected_option("option1")
  })
  
  observeEvent(input$btn2, {
    selected_option("option2")
  })
  
  observeEvent(input$btn3, {
    selected_option("option3")
  })
  output$plot <- renderPlot({
    selected <- selected_option()
    
    if (is.null(selected)) {
      return(NULL)
    }
    
    if (selected == "option1") {
      # Generar el gráfico para la opción 1 
      summary(fit_e<-lm(energia2~time(energia2),na.action=NULL))
      ElimiTendenerg<-energia2-predict(fit_e)
      plot(ElimiTendenerg,main="Serie energía sin tendencia",
           cex.main=1.3,
           xlab="Tiempo",
           ylab="Consumo de energía",
           cex.lab=0.4)
    } else if (selected == "option2") {
      # Generar el gráfico para la opción 2 (reemplaza esto)
      energia_decompo=decompose(energia2)
      
      plot(energia2-energia_decompo$trend,main="Serie energía sin tendencia")
    } else if (selected == "option3"){
      plot(diff(energia2), type="l", main="Primera Diferencia") 
      }
  })
}

# Ejecutar la aplicación Shiny
shinyApp(ui, server)
