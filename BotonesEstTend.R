library(shiny)
library(ggplot2)
library(MASS)
library(readxl)
library(dplyr)
library(forecast)
library(gridExtra)
library(feasts)
library(fable)
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
  titlePanel(HTML("<h1 style='color: blue;'>Estimación de Tendencia por diferentes metodologías.</h1>")),
  sidebarLayout(
    sidebarPanel(
      h3("Para estimar la tendencia es posible utilizar diferentes estrategias
         tanto paramétricas como no paramétricas, presiona cada uno de los botones 
         para observar el gráfico que se obtiene con cada metodología."),
      actionButton("btn1", "Estimación paramétrica"),
      actionButton("btn2", "Filtro de promedio móviles"),
      actionButton("btn3", "Descomposición STL"),
      actionButton("btn4", "Diferenciación")
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
  
  observeEvent(input$btn4, {
    selected_option("option4")
  })
  output$plot <- renderPlot({
    selected <- selected_option()
    
    if (is.null(selected)) {
      return(NULL)
    }
    
    if (selected == "option1") {
      # Generar el gráfico para la opción 1 
      summary(fit<-lm(ldian2~time(ldian2),na.action=NULL))
      ElimiTenddian<-ldian2-predict(fit)
      plot(ElimiTenddian,main="Serie Dian sin tendencia y con varianza estable (lm)",col="purple",
           cex.main=1.3,
           xlab="Tiempo",
           ylab="Recaudo interno",
           cex.lab=0.4)
    } else if (selected == "option2") {
      # Generar el gráfico para la opción 2 (reemplaza esto)
      dian_decompo=decompose(ldian2)
      
      plot(ldian2-dian_decompo$trend,main="Serie Dian sin tendencia y con varianza estable(PFM)",col="purple")
    } else if (selected == "option3") {
      # Generar el gráfico para la opción 3 (reemplaza esto)
      tsibble_dian<-as_tsibble(ldian2)
      #str(tsibble_dian)
      modelo_stl <- tsibble_dian %>%
        model(
          STL(value ~ trend() +
                season(window = "periodic"),
              robust = TRUE)
        ) %>%
        components()
      ElimiTenddian_STL<-ldian2-modelo_stl$trend
      plot(ElimiTenddian_STL,main="Serie Dian sin tendencia y con varianza estable (Por STL)",col="purple",
           cex.main=1.3,
           xlab="Tiempo",
           ylab="Recaudo interno",
           cex.lab=0.4)
    }else if (selected == "option4"){
      plot(diff(ldian2), type="l", main="Serie Dian sin tendencia y con varianza estable (diff)",col="purple")
    }
  })
}

# Ejecutar la aplicación Shiny
shinyApp(ui, server)
