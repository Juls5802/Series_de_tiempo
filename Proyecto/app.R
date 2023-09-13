library(dplyr)
library(rsconnect)
library(devtools)
library(httr)
library(readxl)
library(readr)
library(tsibble)
library(ggplot2)
library(forecast)
library(feasts)
library(data.table)
library(fpp3)
library(seasonal)
library(stringr)
library(shinydashboard)
library(flexdashboard)
library(knitr)
library(DT)
library(yfR)
library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyWidgets)

ui<-fluidPage(title="Proyecto Series de Tiempo",
              theme=shinytheme("journal"),
              navbarPage(img(src="logo.png",height=20,width=50,align="right"),
              tabPanel("Introducción",sidebarLayout(position="right",
              sidebarPanel(width=4,
                           h3("Trabajo de series de tiempo",align='left'),
                           p("Hecho por ",strong("Sofia Bello Dueñas, Andrés García Lopez, Julieta Ruiz Castiblanco"),
                           style = "font-family: 'Bahnschrift'; font-si20pt"),
                           hr(),
                           p("Este trabajo realiza un análisis de dos bases de datos: Recaudo de impuestos 
                              internos por la DIAN para los años 2000 a 2023 y consumo de energía por horas de la 
                              organización regional de transmisión PJM Interconnection para los años 2004 a 2018.
                              El análisis se desarrolla de forma estadística, realizando la parte descriptiva con su
                              correspondiente interpretación.",
                           style = "font-family: 'Bahnschrift'; font-si20pt"),
                           h3("Fuente de los datos",  align = "left"),
                           p("Los datos del recaudo de impuestos internos de la DIAN se obtuvieron de la siguiente fuente",
                           tags$a(href="https://www.dian.gov.co/dian/cifras/Paginas/EstadisticasRecaudo.aspx", 
                                  "Estadísticas de recaudo mensual por Tipo de impuesto 2000 - 2023."),
                                  br(),
                                  "Los datos del consumo de energía de la empresa PJM se obtuvieron de la siguiente fuente",
                           tags$a(href="https://www.kaggle.com/datasets/robikscube/hourly-energy-consumption?resource=download", "Hourly Energy Consumption."),
                           style = "font-family: 'Bahnschrift'; font-si20pt"),
                           img(src = "logo.png", height=35,width=80, align="right"),
                           br(),
                           br(),
                           br(),
                           br(),
                           ),
                                          
                           mainPanel(width=8,
                                     br(),
                                     h1("Análisis del recaudo de impuestos internos por la DIAN",  align = "center"),
                                     h2("Motivación"),
                                     p("La DIAN es la entidad encargada de administrar y recaudar los impuestos 
                                        internos y aduaneros en el país. El recaudo de impuestos internos que 
                                        realiza la DIAN cada mes se refiere a la suma total de los impuestos 
                                        nacionales recaudados dentro del territorio colombiano durante ese período
                                        mensual. Los impuestos internos son aquellos que se aplican a las actividades 
                                        económicas y transacciones que ocurren dentro del país, los impuestos internos en
                                        Colombia pueden incluir: IVA, impuesto de renta y complementarios, impuesto de 
                                        timbre, impuesto de consumo, impuesto a la riqueza, impuesto predial, ICA, entre otros.",
                                      style = "font-family: 'Bahnschrift'; font-si20pt"),
                                     br(),
                                     p("El recaudo de estos impuestos internos es esencial para financiar las actividades
                                        gubernamentales, incluyendo la provisión de servicios públicos, la inversión en
                                        infraestructura y el funcionamiento del gobierno. El seguimiento y la gestión 
                                        eficiente del recaudo de impuestos internos es fundamental para mantener la 
                                        estabilidad económica y el desarrollo del país.",
                                      style = "font-family: 'Bahnschrift'; font-si20pt"),
                                     h3("Serie de tiempo"),
                                     plotOutput("serie_impuesto"),
                                     h1("Análisis del consumo de energía de la empresa PJM",  align = "center"),
                                     h2("Motivación"),
                                     p("PJM es una organización de transmisión regional que coordina el movimiento
                                        de electricidad mayorista en la totalidad o parte de 13 estados y el Distrito de 
                                        Columbia.",
                                      style = "font-family: 'Bahnschrift'; font-si20pt"),
                                     br(),
                                     p("El análisis del consumo de energía es esencial para mejorar la eficiencia 
                                        operativa, reducir costos, cumplir con regulaciones, promover la sostenibilidad
                                        y mantener la competitividad en un mundo en constante cambio.",
                                      style = "font-family: 'Bahnschrift'; font-si20pt"),
                                     h3("Serie de tiempo"),
                                     plotOutput("serie_energia"),
              ),
),
)))

server<-function(input,output){
  
  output$serie_impuesto<-renderPlot({
    dian<-read_excel("dian.xlsx", range="A7:C313", sheet = "Rec mensual a junio 2023" )
    años<-2000:2023
    dian<-dplyr::filter(dian,Año %in% años)
    colnames(dian)<-c("Año","Mes","Impuestos")
    dian$fecha<-as.Date(paste(dian$Año, dian$Mes, "1", sep = "-"), format = "%Y-%B-%d")
    dian<-dian[,3:4]
    dian2<-ts(dian$Impuestos,start=c(2000,01),frequency=12)
    plot(dian2, main="Serie de tiempo del recaudo mensual interno",
         cex.main=1.3,
         xlab="Tiempo",
         ylab="Recaudo interno",
         cex.lab=0.4)
  })
  
  output$serie_energia<-renderPlot({
    AEP_hourly<-read.csv("AEP_hourly.csv")
    AEP_hourly$Datetime<-as.POSIXct(AEP_hourly$Datetime, format = "%Y-%m-%d %H:%M:%S")
    AEP_hourly$fecha<-as.Date(AEP_hourly$Datetime)
    energia <- AEP_hourly %>%
      group_by(fecha) %>%
      summarise(Energia = sum(AEP_MW))
    energia2<-ts(energia$Energia,start=c(2004,10),frequency=365)
    plot(energia2, main="Serie de tiempo de la energía diaria de una empresa estadounidense",
         cex.main=1.2,
         xlab="Tiempo ",
         ylab="Energía consumida",
         cex.lab=0.4)
  })

}
# Run the application 
shinyApp(ui = ui, server = server)


#GitHUB access
#rsconnect::deployApp()
#options(rsconnect.max.bundle.size = 3145728000)
#runGitHub( "SeriesTemporaisUnicamp", "JuanPabloMonDi")



