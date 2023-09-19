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
                           p("Hecho por: ",strong("Sofia Bello Dueñas, Andrés García Lopez, Julieta Ruiz Castiblanco."),
                           style = "font-family: 'Bahnschrift'; font-si20pt"),
                           hr(),
                           p("Este trabajo realiza un análisis de dos bases de datos: Recaudo de impuestos 
                              internos por la DIAN para los años 2000 a 2023 y consumo de energía por horas de la 
                              organización regional de transmisión PJM Interconnection para los años 2004 a 2018.
                              El análisis se desarrolla de forma estadística, realizando la parte descriptiva con su
                              correspondiente interpretación.",
                           style = "font-family: 'Bahnschrift'; font-si20pt"),
                           h3("Fuente de los datos",  align = "left"),
                           p("Los datos del recaudo de impuestos internos de la DIAN se obtuvieron de la siguiente fuente:",
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
                                     HTML('<h2 style="color: blue; text-align: center;">Análisis del recaudo de impuestos internos por la DIAN</h2>'),
                                     
                                     #h1("Análisis del recaudo de impuestos internos por la DIAN",  align = "center"),
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
                                     br(),
                                     p("A Continuación se presenta la manera en la cual se realizó la carga de los datos y 
                                       un vistaso preliminar de la serie de tiempo, a grandes razgos es posible notar que la varianza no 
                                       es constante a lo largo del tiempo, además podemos ver que la serie presenta tendencia creciente y algunos posibles ciclos
                                       estacionales.",
                                       style = "font-family: 'Bahnschrift'; font-si20pt"),
                                     verbatimTextOutput("codigo1"),
                                     h3("Serie de tiempo"),
                                     plotOutput("serie_impuesto1"),
                                     p("",
                                       style = "font-family: 'Bahnschrift'; font-si20pt"),
                                     h2("Transformación Box-cox"),
                                     p("Como fue posible observar en la gráfica de la serie de tiempo
                                       Es necesario realizar una estabilización de la varianza para continuar con el análisis descriptivo
                                       de la serie para esto se utilizó una trasnformación de Box-cox.",
                                       style = "font-family: 'Bahnschrift'; font-si20pt"),
                                     verbatimTextOutput("boxcox1_text"),
                                     plotOutput("boxcox1_plot_1"),
                                     p("Notese que en este caso no se contiene al valor lambda 1, por lo tanto
                                       es necesario realizar la transformación Box cox a los datos.",
                                       style = "font-family: 'Bahnschrift'; font-si20pt"),
                                     textOutput("boxcox1_plot_2"),br(),
                                     p("Se obtuvo un valor de 0.1 para lambda por lo tanto se procede a realizar una transformación
                                       logaritmica de los datos y se obtienen lo siguiente:",
                                       style = "font-family: 'Bahnschrift'; font-si20pt"),
                                     plotOutput("boxcox1_plot_3"),
                                     
                                     p("Se puede observar que ahora si captura al valor de lambda=1, lo cual nos indica que
                                       no es necesario realizar ninguna transformacion extra, en la gráfica que se presenta a continuación
                                       es posible observar la serie Dian sin y con la trasnformación Box-cox y podemos
                                       observar que se reduce considerablemente su variabilidad.",
                                       style = "font-family: 'Bahnschrift'; font-si20pt"),
                                     plotOutput("boxcox1_plot_4"),
                                     ########################Segunda Serie##################################
                                     HTML('<h2 style="color: blue; text-align: center;">Análisis del consumo de energía de la empresa PJM</h2>'),
                                     #h1("Análisis del consumo de energía de la empresa PJM",  align = "center"),
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
                                     plotOutput("serie_energia1"),
              ),
),
)))

server<-function(input,output){
  output$codigo1 <- renderText({
    "dian<-read_excel(\"dian.xlsx\", range=\"A7:C313\", sheet = \"Rec mensual a junio 2023\" )
    años<-2000:2023
    dian<-dplyr::filter(dian,Año %in% años)
    colnames(dian)<-c(\"Año\",\"Mes\",\"Impuestos\")
    dian$fecha<-as.Date(paste(dian$Año, dian$Mes, \"1\", sep = \"-\"), format = \"%Y-%B-%d\")
    dian<-dian[,3:4]
    dian2<-ts(dian$Impuestos,start=c(2000,01),frequency=12)
    plot(dian2, main=\"Serie de tiempo del recaudo mensual interno\",
         cex.main=1.3,
         xlab=\"Tiempo\",
         ylab=\"Recaudo interno\",
         cex.lab=0.4)"
  })
  output$serie_impuesto1<-renderPlot({
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
  ####Boxcox#####
  output$boxcox1_text<-renderText({
    library(forecast)
    "MASS::boxcox(lm(dian2 ~ 1),seq(-5, 5, length = 50)) ##Notese que no acputra al 1
    forecast::BoxCox.lambda(dian2, method =\"loglik\",
                            lower = -1, upper = 3)#Entrega el valor de lambda (0.1).
    plot(forecast::BoxCox(dian2,lambda=0.1))
    par(mar = c(1,1,1,1))
    ldian2=log(dian2)
    
    MASS::boxcox(lm(ldian2 ~ 1),seq(-5, 5, length = 50)) #Si captura al 1
    par(mfrow=c(2,1))
    plot(dian2,main=\"Serie Dian sin Transformar\")
    plot(ldian2,main=\"Series Dian con Transformación BoxCox\")"
  })
  output$boxcox1_plot_1<-renderPlot({
    MASS::boxcox(lm(dian2 ~ 1),seq(-5, 5, length = 50)) ##Notese que no acputra al 1
  })
  output$boxcox1_plot_2<-renderPrint({
    forecast::BoxCox.lambda(dian2, method ="loglik",
                            lower = -1, upper = 3)#Entrega el valor de lambda (0.1).
  })
  output$boxcox1_plot_3<-renderPlot({
    MASS::boxcox(lm(ldian2 ~ 1),seq(-5, 5, length = 50)) #Si captura al 1
    abline(v = 1, col = "red", lty = 2)
  })
  output$boxcox1_plot_4<-renderPlot({
    par(mfrow=c(2,1))
    plot(dian2,main="Serie Dian sin Transformar")
    plot(ldian2,main="Series Dian con Transformación BoxCox")
    
  })
  ###################Serie de Energia ##############
  output$serie_energia1<-renderPlot({
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



