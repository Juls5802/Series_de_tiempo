library(shiny)
library(ggplot2)
library(MASS)
library(readxl)
library(dplyr)
library(forecast)
library(gridExtra)
# Cargar tus datos

dian<-read_excel("dian.xlsx", range="A7:C313", sheet = "Rec mensual a junio 2023" )
años<-2000:2023
dian<-dplyr::filter(dian,Año %in% años)
colnames(dian)<-c("Año","Mes","Impuestos")
dian$fecha<-as.Date(paste(dian$Año, dian$Mes, "1", sep = "-"), format = "%Y-%B-%d")
dian<-dian[,3:4]

# Serie de tiempo de la DIAN 
dian2<-ts(dian$Impuestos,start=c(2000,01),frequency=12)

# Define la interfaz de usuario

ui <- fluidPage(
  titlePanel(HTML("<h1 style='color: blue;'>Transformación Box-Cox de la Serie DIAN.</h1>")),
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
  transformed_data <- forecast::BoxCox(dian2,lambda=lambda)
  
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
      geom_line(aes(x = time(dian2), y = dian2), color = "blue") +
      labs(x = "Año", y = "Serie de Tiempo Original") +
      theme_minimal()
    combined_plot <- grid.arrange(plot1, plot2, heights = c(2, 1))
    print(combined_plot)
  })
  #output$time_series_plot_original <- renderPlot({
   # plot(dian2, main="Serie de tiempo del recaudo mensual interno",
    #      cex.main=1.3,
     #     xlab="Tiempo",
      #    ylab="Recaudo interno",
       #   cex.lab=0.4)
  #})
}

# Ejecutar la aplicación Shiny
shinyApp(ui, server)
