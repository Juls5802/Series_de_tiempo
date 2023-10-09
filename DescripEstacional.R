library(shiny)
library(TSstudio)
library(shinyjs)
library(ggplot2)
library(MASS)
library(readxl)
library(dplyr)
library(forecast)
library(gridExtra)


ui <- fluidPage(
  titlePanel(HTML("<h1 style='color: blue;'>Exploración de la componente estacional serie DIAN (descriptivas).</h1>")),
  h3("Existen mutliples maneras de explorar la componente estacional de nuestra serie de tiempo,
     en la siguiente lista desplegable puedes encontrar varias metodologias para observar la presencia de dicha componente.
     Recuerda que cada una de las siguientes gráficas se da para la serie sin componente estacional"),
  sidebarLayout(
    sidebarPanel(
      # Crea una lista desplegable con elementos de ejemplo
      selectInput("opcion", "Selecciona una opción:",
                  choices = c("Graficos de retardos", "Graficos de subseries", "Promedio mensual",
                              "Heatmap"))
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
    if (opcion_seleccionada == "Heatmap") {
      # Código para el gráfico de TSstudio de la opción 1
      plot <- ts_heatmap(ElimiTenddian_STL, title = "Mapa de Calor - Impuestos Dian sin tendencia (STL)")
    } else if (opcion_seleccionada == "Promedio mensual") {
      # Código para el gráfico de TSstudio de la opción 2
      dian2sint_df <- data.frame(year = floor(time(ElimiTenddian_STL)), month = cycle(ElimiTenddian_STL),ElimiTenddian_STL = as.numeric(ElimiTenddian_STL))
      dian2sint_df$month <- factor(month.abb[dian2sint_df$month], levels = month.abb)
      dian2sint_summary <- dian2sint_df %>%group_by(month) %>%summarise(mean= mean(ElimiTenddian_STL),sd = sd(ElimiTenddian_STL))
      dian2sint_summary
      plot_ly (data = dian2sint_summary, x = ~ month, y = ~ mean, type = "bar", name   = "Mean") %>%
        layout (title = "dian2sint - Monthly Average", yaxis =list(title = "Mean",   range = c(min(dian2sint_summary$mean), max(dian2sint_summary$mean))))
    } 
    
    # Retorna el gráfico de TSstudio como una salida UI
    #return(HTML(plot$code))
  })
  
  # Función para generar el gráfico R base
  output$grafico_base <- renderPlot({
    opcion_seleccionada <- input$opcion
    
    # Puedes personalizar esta parte para generar el gráfico R base según la opción seleccionada
    if (opcion_seleccionada == "Graficos de retardos") {
      # Código para el gráfico R base de la opción 1
      par(mar = c(3,2,3,2))
      astsa::lag1.plot(ElimiTenddian_STL, 12,corr=F,main="Gráfico de retardos")
    } else if (opcion_seleccionada == "Graficos de subseries") {
      # Código para el gráfico R base de la opción 2
      dian2_tsbl_notend=as_tsibble(ElimiTenddian_STL)
      dian2_tsbl_notend%>%gg_subseries(value)
    } 
  })
}

# Crea la aplicación Shiny
shinyApp(ui, server)
