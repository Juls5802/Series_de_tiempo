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


ui <- fluidPage(
  titlePanel(HTML("<h1 style='color: blue;'>Exploración de la componente estacional serie Energía (descriptivas).</h1>")),
  h3("Existen mutliples maneras de explorar la componente estacional de nuestra serie de tiempo original,
     en la siguiente lista desplegable puedes encontrar varias metodologias para observar la presencia de dicha componente."),
  sidebarLayout(
    sidebarPanel(
      # Crea una lista desplegable con elementos de ejemplo
      selectInput("opcion", "Selecciona una opción:",
                  choices = c("Gráfico de retardos", "Gráfico de subseries mensuales", "Gráfico de subseries diarias"))
    ),
    mainPanel(
      # Crea un espacio para los gráficos
      uiOutput("grafica_output"),
      plotOutput("grafico_base")
    )
  )
)

# Define el servidor
server <- function(input, output) {
  
  # Función para generar el gráfico de TSstudio
  output$grafica_output <- renderUI({
    opcion_seleccionada <- input$opcion
    
    # Puedes personalizar esta parte para generar el gráfico de TSstudio según la opción seleccionada
    if (opcion_seleccionada == "Gráfico de subseries diarias") {
      # Código para el gráfico de TSstudio de la opción 1
      plot <- gg_subseries(tsbl_energia, y = Energia, period = 7)
      return(plotly::ggplotly(plot))
    } else if (opcion_seleccionada == "Gráfico de retardos") {
      ts_lags(energia2,lags=1:7) %>% layout(title = "Gráfico de retardos de la serie original", 
              titlefont = list(size = 16))
    } else if (opcion_seleccionada == "Gráfico de subseries mensuales") {
      # Código para el gráfico R base de la opción 2
      plot1 <- gg_subseries(tsbl_energia, y = Energia, period = 12)
      return(plotly::ggplotly(plot1))
    }
    
    # Retorna el gráfico de TSstudio como una salida UI
    #return(HTML(plot$code))
  })
  
}

# Crea la aplicación Shiny
shinyApp(ui, server)
