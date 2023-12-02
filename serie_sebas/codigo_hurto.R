setwd("C:/Users/LENOVO/Desktop/Universidad/Series de tiempo")
load("series.Rdata")
ls()
hurto_ts

library(shiny)
library(forecast)
library(tseries)

# UI
ui <- fluidPage(
  titlePanel("Análisis de Series Temporales"),
  sidebarLayout(
    sidebarPanel(
      selectInput("modelo", "Selecciona un modelo:", 
                  choices = c("ARIMA", "ARMA")),
      uiOutput("parametrosInput")
    ),
    mainPanel(
      plotOutput("plotSerie"),
      verbatimTextOutput("resultadoTest"),
      plotOutput("plotResiduales")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  datos <- reactive({
    # Cargar tus datos aquí. Por ejemplo, puedes cargar "hurto_ts" directamente si ya está en tu entorno
    hurto_ts
  })
  
  output$plotSerie <- renderPlot({
    plot(datos(), main = "Serie de Tiempo", ylab = "Valor", xlab = "Tiempo")
  })
  
  observeEvent(input$modelo, {
    if(input$modelo == "ARIMA") {
      output$parametrosInput <- renderUI({
        tagList(
          numericInput("p", "p:", value = 1, min = 0),
          numericInput("q", "q:", value = 1, min = 0)
        )
      })
    } else {
      output$parametrosInput <- renderUI({
        tagList(
          numericInput("p", "p:", value = 1, min = 0),
          numericInput("q", "q:", value = 1, min = 0)
        )
      })
    }
  })
  
  output$resultadoTest <- renderPrint({
    if(input$modelo == "ARIMA") {
      adf.test(datos(), k = 13)
      summary(ur.df(datos(), type = "trend", lags = 13))
    }
  })
  
  output$plotResiduales <- renderPlot({
    if(input$modelo == "ARIMA") {
      if(adf.test(datos(), k = 13)$p.value < 0.05) {
        ajuste <- forecast::Arima(datos(), order = c(input$p, 1, input$q), lambda = 1, include.constant = TRUE)
        plot(resid(ajuste),main="Gráfico de los residuales")
      } else {
        return("No se puede realizar ARIMA debido a que no hay presencia de raíz unitaria")
      }
    } else {
      ajuste <- Arima(datos(), order = c(input$p, 0, input$q), include.mean = TRUE)
      plot(resid(ajuste),main="Gráfico de los residuales")
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
