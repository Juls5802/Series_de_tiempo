library(tsibble)
library(fable)
library(plotly)
library(dygraphs)
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
library(dygraphs)

ui<-fluidPage(title="Proyecto Series de Tiempo",
              theme=shinytheme("journal"),
              navbarPage(img(src="logo.png",height=20,width=50,align="right"),
              tabPanel("Introducción",sidebarLayout(position="right",
              sidebarPanel(
                           h3("Trabajo de series de tiempo",align='left'),
                           p("Elaborado por: ",strong("Sofia Bello Dueñas, Andrés García López, Julieta Ruiz Castiblanco."),
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
                                          
                           mainPanel(
                             fluidRow(column(width=6,
                                     br(),
                                     HTML('<h2 style="color: blue; text-align: center;">Análisis del recaudo de impuestos internos por la DIAN</h2>'),
                                     h2("Motivación"),
                                     p("La DIAN es la entidad encargada de administrar y recaudar los impuestos 
                                        internos y aduaneros en el país. El recaudo de impuestos internos que 
                                        realiza la DIAN cada mes se refiere a la suma total de los impuestos 
                                        nacionales recaudados dentro del territorio colombiano durante ese período
                                        mensual. Los impuestos internos son aquellos que se aplican a las actividades 
                                        económicas y transacciones que ocurren dentro del país, los cuales pueden incluir: IVA, impuesto de renta y complementarios, impuesto de 
                                        timbre, impuesto de consumo, impuesto a la riqueza, impuesto predial, ICA, entre otros.",
                                      style = "font-family: 'Bahnschrift'; font-si20pt"),
                                     br(),
                                     p("Con el proyecto se busca estudiar esta serie de tiempo para ver como es el comportamiento 
                                        de los impuestos internos de Colombia a lo largo de los años, por ejemplo, encontrar patrones
                                        y observar qué tanto han aumentado dichos impuestos.",
                                      style = "font-family: 'Bahnschrift'; font-si20pt"),
                                     br(),
                                     p("A continuación se presenta la manera en que se realiza la carga de los datos y 
                                       un vistazo preliminar de la serie de tiempo.",
                                       style = "font-family: 'Bahnschrift'; font-si20pt"),
                                     verbatimTextOutput("codigo1"),
                                     h3("Serie de tiempo"),
                                     plotOutput("serie_impuesto1"),
                                     p("",
                                       style = "font-family: 'Bahnschrift'; font-si20pt"),),
                                     column(width=6,
                                            br(),
                                     ########################Segunda Serie##################################
                                     HTML('<h2 style="color: blue; text-align: center;">Análisis del consumo de energía de la empresa PJM</h2>'),
                                     #h1("Análisis del consumo de energía de la empresa PJM",  align = "center"),
                                     h2("Motivación"),
                                     p("La empresa PJM es una organización de transmisión regional que coordina el movimiento
                                        de electricidad mayorista en la totalidad, o parte, de 13 estados y el Distrito de 
                                        Columbia.",
                                      style = "font-family: 'Bahnschrift'; font-si20pt"),
                                     br(),
                                     p("El análisis del consumo de energía es esencial para mejorar la eficiencia 
                                        operativa, reducir costos, cumplir con regulaciones y promover la sostenibilidad, por lo
                                        cual, este proyecto analiza la serie de tiempo con el fin de encontrar variaciones 
                                        en el consumo de energía de los 13 estados y el Distrito de Columbia a lo largo del 
                                        tiempo, así como también descubrir posibles patrones.",
                                      style = "font-family: 'Bahnschrift'; font-si20pt"),
                                     verbatimTextOutput("codigo2"),
                                     h3("Serie de tiempo"),
                                     plotOutput("serie_energia1"),),
                                     
              ),),
),),
              tabPanel("Análisis descriptivo DIAN",
                       mainPanel(width=12,
                                 br(),
                                 HTML('<h2 style="color: blue; text-align: center;">Análisis descriptivo del recaudo de impuestos internos por la DIAN</h2>'),
                                 #--------------------------------------------------------------------
                                 p("Según lo observado en la serie de tiempo, visualmente se tiene: ",
                                   style = "font-family: 'Bahnschrift'; font-si20pt"),
                                 tags$ul(
                                   tags$li(HTML("<b>Heterocedasticidad marginal:</b> Se puede observar que la varianza de cada instante en
                                                la serie no es la misma, a medida que pasa el tiempo esta va aumentanto, por lo tanto surge la 
                                                necesidad de realizar una transformación Box-cox.")),
                                   tags$li(HTML("<b>Tendencia:</b> A simple vista se observa que, a medida que pasa el 
                                                tiempo, la serie oscila al rededor de valores cada vez más grandes, por lo tanto, es 
                                                necesario estimar la tendencia y posteriormente eliminarla para poder continuar con el 
                                                análisis de la serie.")),
                                   tags$li(HTML("<b>Componente estacional:</b> Se observan algunos patrones que se repiten con cierta 
                                                periodicidad (posiblemente cada año), lo cual hace que sea necesario estimar posibles 
                                                comportamientos estacionales."))
                                 ),
                                 h2("Estabilización de la varianza marginal"),
                                 p("Como se observa en la gráfica de la serie de tiempo es necesario realizar una estabilización
                                   de la varianza para continuar con el análisis descriptivo,  para esto se utilizó una 
                                   trasnformación de Box-cox.",
                                   style = "font-family: 'Bahnschrift'; font-si20pt"),
                                 verbatimTextOutput("boxcox1_text"),
                                 plotOutput("boxcox1_plot_1"),
                                 p("Nótese que en este caso no se contiene al valor lambda 1, por lo tanto
                                       es necesario realizar la transformación Box-cox a los datos.",
                                   style = "font-family: 'Bahnschrift'; font-si20pt"),
                                 textOutput("boxcox1_plot_2"),br(),
                                 p("Se obtuvo un valor de 0.1 para lambda por lo tanto se procede a obtener el logaritmo 
                                   de los datos y se obtiene lo siguiente:",
                                   style = "font-family: 'Bahnschrift'; font-si20pt"),
                                 plotOutput("boxcox1_plot_3"),
                                 
                                 p("Se puede observar que el valor de lambda ahora sí captura a 1, lo cual nos indica que
                                    no es necesario realizar ninguna transformacion extra. En la gráfica que se presenta 
                                    a continuación es posible observar la serie Dian sin y con la trasnformación Box-cox, 
                                    y podemos observar que se reduce considerablemente su variabilidad.",
                                   style = "font-family: 'Bahnschrift'; font-si20pt"),
                                 plotOutput("boxcox1_plot_4"),
                                 #--------------------------------------------------------------------
                                 verbatimTextOutput("Serienotend_1"),
                                 dygraphOutput("Serienovar_plot_1"),
                                 h2("Estimación de la tendencia."),
                                 p("Al observa el gráfico de la serie de tiempo con su varianza marginal estable,
                                   es posible observar que esta presenta tendencia es decir que su valor medio no fluctua al rededor de un valor,
                                   por lo tanto procederemos a estimarla
                                   para poder eliminarla y asi continuar realizando nuestro análisis, en primer lugar se presentará
                                   un gráfico que estima de modo preliminar la tendencia de nuestra serie.",
                                   style = "font-family: 'Bahnschrift'; font-si20pt"),
                                 
                                 verbatimTextOutput("tsplot_varest"),
                                 plotlyOutput("esttendnovar_plot_1"),
                                 p("Para realizar la Estimación de la tendencia se utilizarón 3 metodos, los cuales fueron:
                                   Estimación por regresión lineal simple, descomposicion utilizando filtro de promedios moviles y descomposición
                                   STL, se hablara puntualmente de lo obtenido en cada uno de estos metodos y se mostraran sus ajustes.
                                   .",
                                   style = "font-family: 'Bahnschrift'; font-si20pt"),
                                 h3("Estimacion de la tendencia usando regresion lineal simple"),
                                 p("Como se mencionó anteriormente se utilizó la regresion lineal simple para hacer la primera estimación
                                 de la tendencia de nuestra serie, ya que se observaba que esta parecia crecer de manera lineal,
                                 Con esto se tuvo lo siguiente.
                                   ",
                                   style = "font-family: 'Bahnschrift'; font-si20pt"),
                                 verbatimTextOutput("diansinten_rs_1"),
                                 plotOutput("diansinten_rs_2"),
                                 plotOutput("diansinten_rs_3"),
                                 plotOutput("diansinten_rs_4"),
                                 p("Como se pude observar en los anteriore gráficos, al ajustar la tendencia por regresión lineal simple
                                 se elimina correctamente la tendencia de la serie, pero en el acf de la serie es posible 
                                 observar su lento decaimiento a 0, lo cual nos puede sugerir la presencia de algún componente estacional.
                                   ",
                                   style = "font-family: 'Bahnschrift'; font-si20pt"),br(),
                                 p("Ahora procederemos a realizar la estimación de la tendencia utilizando el filtro de promedios moviles
                                 y mediante STL, es importante resaltar que como aun no se ha trabajado sobre la componente estacional, estas herramientas
                                 nos dan una estimación preliminar de la tendencia.
                                   ",
                                   style = "font-family: 'Bahnschrift'; font-si20pt"),
                                 verbatimTextOutput("diansinten_rs_5"),
                                 plotOutput("diansinten_rs_6"),
                                 plotOutput("diansinten_rs_7"),
                                 p("Es posible observar que tanto la descomposición elaborada utilizando
                                 promedios moviles, como la elaborada por STL, nos dan información similar, además la estimación 
                                 de la tendencia en ambos casos es aproximadamente lineal y se captura de buena manera
                                 la posible componente estacional de la serie. Otro mecanismo eficaz para la 
                                 eliminación de la tendencia es utilizar la diferenciación, este metodo además puede ayudar a eliminar
                                 las componentes estacionales presentes en la serie, por lo tanto presentaremos a continuación una comparación
                                 entre la eliminacion de la tendencia utilizando diferenciación y la eliminación de la tendencia utilizando
                                 regresion lineal.
                                   ",
                                   style = "font-family: 'Bahnschrift'; font-si20pt"),
                                 verbatimTextOutput("diansinten_rs_8"),
                                 plotOutput("diansinten_rs_9"),
                                 plotOutput("diansinten_rs_10"),
                                 p("Es notorio que ambas formas de estimar y eliminar la tendencia nos generan resultados optimos
                                 y además es posible observar que los graficos ACF descienden rapidamente a 0, pero en ambos casos
                                 es posible observar la presencia de componentes estacionales que posiblemente podrian
                                 tener un perido de 12, pero esto lo abordaremos más a detalle más adelante.
                                   ",
                                   style = "font-family: 'Bahnschrift'; font-si20pt"),
                                 h2("Graficos de retardos (falta hacer la interpretación)"),
                                 verbatimTextOutput("grafretardos_1"),
                                 plotOutput("grafretardos_2"),
                                 p("En los anteriores graficos de dispersion es posible observar la posible
                                 relación entre las variables al igual que una estimación de la autocorrelación entre los retardos,
                                 para esta serie en particular, se puede evidenciar una alta relacion lineal positiva en rezagos con h=12
                                 mientras que en los demas rezagos no se evidencia una relacion lineal tan fuerte e incluso en algunas situaciones
                                 puede considerarse no lineales o no significativa.
                                 .
                                   ",
                                   style = "font-family: 'Bahnschrift'; font-si20pt"),
                                 h2("Detección de estacionalidad."),
                                 p("Como se mencionó anteriormente es posible observar en nuestra serie de tiempo la posible presencia de una componente estacional
                                 por lo tanto surge la necesidad de confirmar la presencia de la misma y posteriormente realizar su modelado y eliminación,
                                 en esta primera etapa nos enfocaremos en utilizar algunas herramientas visuales que nos ayuden a observar la presencia de dichas componentes
                                 estacionales, estas pueden ser, grafico de perfiles, boxplot, mapas de calor entre otros, si iniciará utilizando un grafico de perfiles,
                                 el cual se presenta a continuación.
                                 .
                                   ",
                                   style = "font-family: 'Bahnschrift'; font-si20pt"),
                                 verbatimTextOutput("grafperfiles_0"),
                                 plotOutput("grafperfiles_1"),
                                 p("Interpretación gráfico de perfiles.
                                 .
                                   ",
                                   style = "font-family: 'Bahnschrift'; font-si20pt"),
                                 verbatimTextOutput("indiceAMI_0"),
                                 plotOutput("indiceAMI_1"),
                                 p("Interpretación Indice AMI.
                                 .
                                   ",
                                   style = "font-family: 'Bahnschrift'; font-si20pt"),
                                 
                                 verbatimTextOutput("monthlyaverage_1"),
                                 plotlyOutput("monthlyaverage_2"),
                                 p("Interpretación Montly Average.
                                 .
                                   ",
                                   style = "font-family: 'Bahnschrift'; font-si20pt"),
                                 
                                 verbatimTextOutput("heatmap_0"),
                                 plotlyOutput("heatmap_1"),
                                 plotlyOutput("heatmap_2"),
                                 p("Interpretación Heat Map.
                                 .
                                   ",
                                   style = "font-family: 'Bahnschrift'; font-si20pt"),

                                 verbatimTextOutput("subseries_0"),
                                 plotOutput("subseries_1"),
                                 p("Interpretación Gráfico de subseries.
                                 .
                                   ",
                                   style = "font-family: 'Bahnschrift'; font-si20pt"),
                                 
                                 verbatimTextOutput("periodograma_0"),
                                 plotOutput("periodograma_1"),
                                 textOutput("periodograma_2"),
                                 textOutput("periodograma_3"),
                                 p("Interpretación periodograma.
                                 .
                                   ",
                                   style = "font-family: 'Bahnschrift'; font-si20pt"),
                                 h2("Eliminación de la componente estacional.")
                                 )#
                       ),        
                                  
              tabPanel("Análisis descriptivo Energía",
                       mainPanel(width=12,
                                 br(),
                                 HTML('<h2 style="color: blue; text-align: center;">Análisis descriptivo del consumo de energía de la empresa PJM</h2>'),
                                 #--------------------------------------------------------------------)
                                 p("Según lo observado en la serie de tiempo, visualmente se tiene: ",
                                   style = "font-family: 'Bahnschrift'; font-si20pt"),
                                 tags$ul(
                                   tags$li(HTML("<b>Heterocedasticidad marginal:</b> Se puede observar que la varianza de cada instante en la 
                                                serie no es la misma, a medida que pasa el tiempo esta va aumentanto, por lo tanto surge la 
                                                necesidad de realizar una transformación Box-cox.")),
                                   tags$li(HTML("<b>Tendencia:</b> Aunque no sea evidencie que a medida que pasa el tiempo, la serie oscila 
                                                al rededor de valores cada vez más grandes o más pequeños, se estima la tendencia
                                                y posteriormente se  elimina para poder continuar con el análisis de la serie. ")),
                                   tags$li(HTML("<b>Componente Estacional:</b> Se observan algunos patrones que se repiten con cierta periodicidad,
                                                (la cual no es tan evidente), lo cual hace que sea necesario estimar posibles comportamientos estacionales.")),
                                  ),
                                 h2("Estabilización de la varianza marginal"),
                                 p("Como se observa en la gráfica de la serie de tiempo es necesario realizar una estabilización
                                    de la varianza para continuar con el análisis descriptivo,  para esto se utilizó una 
                                    trasnformación de Box-cox.",
                                   style = "font-family: 'Bahnschrift'; font-si20pt"),
                                 verbatimTextOutput("boxcox_ener_text"),
                                 plotOutput("boxcox_ener_plot_1"),
                                 p("Notese que en este caso no se contiene al valor lambda 1, por lo tanto es necesario 
                                    realizar la transformación Box-cox a los datos.",
                                   style = "font-family: 'Bahnschrift'; font-si20pt"),
                                 textOutput("boxcox_ener_plot_2"),br(),
                                 p("Se obtuvo un valor de -0.25 para lambda por lo tanto se procede a obtener el logaritmo 
                                   de los datos y se obtiene lo siguiente:",
                                   style = "font-family: 'Bahnschrift'; font-si20pt"),
                                 plotOutput("boxcox_ener_plot_3"),
                                 p("Se puede observar que el valor de lambda ahora sí captura a 1, lo cual nos indica que
                                    no es necesario realizar ninguna transformacion extra. En la gráfica que se presenta 
                                    a continuación es posible observar la serie Dian sin y con la trasnformación Box-cox, 
                                    y podemos observar que se reduce considerablemente su variabilidad. revisar si si contiene a 1xq nunca pude ver el grafico yy queda un titulo sobre otro horrible",
                                   style = "font-family: 'Bahnschrift'; font-si20pt"),
                                 plotOutput("boxcox1_plot_4"),
                                 #--------------------------------------------------------------------
                                 verbatimTextOutput("Serienotend_1_ener"),
                                 dygraphOutput("Serienovar_plot_1_ener"),
                                 h2("Estimación de la tendencia."),
                                 p("Al observa el gráfico de la serie de tiempo con su varianza marginal estable,
                                   es posible observar que esta presenta tendencia es decir que su valor medio no fluctua al rededor de un valor,
                                   por lo tanto procederemos a estimarla
                                   para poder eliminarla y asi continuar realizando nuestro análisis, en primer lugar se presentará
                                   un gráfico que estima de modo preliminar la tendencia de nuestra serie.",
                                   style = "font-family: 'Bahnschrift'; font-si20pt"),
                                 
))))

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
         cex.main=1,
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
         cex.main=1,
         xlab="Tiempo",
         ylab="Recaudo interno",
         cex.lab=0.4)
  })
  ####Boxcox#####
  output$boxcox1_text<-renderText({
    "library(forecast)
    MASS::boxcox(lm(dian2 ~ 1),seq(-5, 5, length = 50)) 
    forecast::BoxCox.lambda(dian2, method =\"loglik\",
                            lower = -1, upper = 3)
    plot(forecast::BoxCox(dian2,lambda=0.1))
    par(mar = c(1,1,1,1))
    ldian2=log(dian2)
    
    MASS::boxcox(lm(ldian2 ~ 1),seq(-5, 5, length = 50)) 
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
  
  output$Serienotend_1 <- renderText({
    "dian3<-window(ldian2, start = c(2000,1))
ts_plot(dian3,title=\"Serie de tiempo del recaudo mensual interno\",
        Ytitle=\"Recaudo interno\",
        Xtitle=\"Tiempo\",
        Xgrid=TRUE,
        Ygrid=TRUE)
dygraph(dian3,main=\"Serie de tiempo del recaudo mensual interno\",
            ylab=\"Recaudo interno\",
            xlab=\"Tiempo\")"
  })
  output$Serienovar_plot_1<-renderDygraph({
    dian3<-window(ldian2, start = c(2000,1))
    dygraph(dian3,main="Serie de tiempo del recaudo mensual interno",
            ylab="Recaudo interno",
            xlab="Tiempo")
    #ts_plot(dian3,title="Serie de tiempo del recaudo mensual interno",
        #Ytitle="Recaudo interno",
        #Xtitle="Tiempo",
        #Xgrid=TRUE,
        #Ygrid=TRUE)
    
  })
  output$tsplot_varest <- renderText({
    "dian$Impuestos<-log(dian$Impuestos)
Dian_1=dian %>% map_df(rev)

Fechas=as.yearmon(Dian_1$fecha)
Dian_xts=xts(x = Dian_1$Impuestos,frequency = 12,order.by = Fechas)
ts_info(Dian_xts)
plot(Dian_xts)

###Creación objeto tssible a partir de un objeto tibble
df_dian=data.frame(impuesto=Dian_1$Impuestos,fecha=Dian_1$fecha)
tbl_dian=tibble(df_dian)
tbl_dian_format_fecha=tbl_dian
tbl_dian_format_fecha$fecha=yearmonth(tbl_dian_format_fecha$fecha)
tsbl_dian=as_tsibble(tbl_dian_format_fecha,index=fecha)

tbl_dian$fecha<-as.Date(zoo::as.yearmon(tbl_dian$fecha))
tbl_dian
tbl_dian%>%plot_time_series(.value=impuesto,.date_var=fecha)"
  })
  output$esttendnovar_plot_1<-renderPlotly({
    p<-tbl_dian%>%plot_time_series(.value=impuesto,.date_var=fecha)})
  plotly::plotly(p)
  
output$diansinten_rs_1 <- renderText({
    "summary(fit<-lm(ldian2~time(ldian2),na.action=NULL))
plot(ldian2,ylab=\"Recaudo interno\") 
abline(fit,col=\"darkcyan\",lwd=2)
#Eliminamos la tendencia con la predicción de la recta
ElimiTenddian<-ldian2-predict(fit)
plot(ElimiTenddian,main=\"Serie Dian sin tendencia y con varianza estable\",
     cex.main=1.3,
     xlab=\"Tiempo\",
     ylab=\"Recaudo interno\",
     cex.lab=0.4)
acf(ElimiTenddian,lag.max=179,main=\"Acf Serie Dian sin tendencia\")"
  })
output$diansinten_rs_2<-renderPlot({
  plot(ldian2,ylab="Recaudo interno",main="Serie Dian con tendencia y varianza estable") 
  abline(fit,col="darkcyan",lwd=2)
})
output$diansinten_rs_3<-renderPlot({
  plot(ElimiTenddian,main="Serie Dian sin tendencia y con varianza estable",
     cex.main=1.3,
     xlab="Tiempo",
     ylab="Recaudo interno",
     cex.lab=0.4)
})
output$diansinten_rs_4<-renderPlot({
  acf(ElimiTenddian,lag.max=179,main="Acf Serie Dian sin tendencia y con varianza estable")
})
output$diansinten_rs_5 <- renderText({
  "### Descomposición con filtro de promedios moviles ####
dian_decompo=decompose(ldian2)
plot(dian_decompo)
dian_decompo$trend
### Descomposición STL ####
##Algunas librerias
library(feasts)
library(fable)
### Gráfico ##
tsibble_dian<-as_tsibble(ldian2)
str(tsibble_dian)
tsibble_dian %>%
  model(
    STL(value ~ trend() +
          season(window = \"periodic\"),
        robust = TRUE)) %>%
  components() %>%
  autoplot()
tsibble_dian_notendstl<-tsibble_dian$Log"
})
output$diansinten_rs_6<-renderPlot({
  plot(dian_decompo)
})
output$diansinten_rs_7<-renderPlot({
  tsibble_dian<-as_tsibble(ldian2)
  str(tsibble_dian)
  tsibble_dian %>%
    model(
      STL(value ~ trend() +
            season(window = "periodic"),
        robust = TRUE)) %>%
  components() %>%
  autoplot()})
#tsibble_dian_notendstl<-tsibble_dian$Log
output$diansinten_rs_8 <- renderText({
  "par(mar = c(2,2,2,2))
fitdian = lm(ldian2~time(ldian2), na.action=NULL) # Regresión sobre el tiempo igual que el objeto 
par(mfrow=c(2,1))
plot(resid(fitdian), type=\"l\", main=\"sin tendencia\") #Sin tendencia quitandola con regresión
plot(diff(dian2), type=\"l\", main=\"Primera Diferencia\") #Primera diferencia ordinaria
## Gráfico de los acf
par(mar = c(3,2,3,2))
par(mfrow=c(3,1)) # plot ACFs
acf(ldian2, 60, main=\"ACF Dian objeto ts varianza estable por boxcox\")
acf(resid(fitdian), 60, main=\"ACF Sin tendencia (resid(fitdian))\") 
acf(diff(dian2), 60, main=\"ACF Primera Diferencia\")"
})
output$diansinten_rs_9<-renderPlot({
  par(mfrow=c(2,1))
  plot(resid(fitdian), type="l", main="Serie Dian sin tendencia (por lm)") #Sin tendencia quitandola con regresión
  plot(diff(ldian2), type="l", main="Serie Dian sin tendencia por primera Diferencia") #Primera diferencia ordinaria
})
output$diansinten_rs_10<-renderPlot({
  par(mfrow=c(3,1)) # plot ACFs
  acf(ldian2, 60, main="ACF Dian con tendencia varianza estable por boxcox")
  acf(resid(fitdian), 60, main="ACF Sin tendencia por lm") 
  acf(diff(ldian2), 60, main="ACF Primera Diferencia")
})
output$grafretardos_1 <- renderText({
  "# Grafica retardos sin tendencia y varianza estable
  ts_info(ElimiTenddian)
  par(mar = c(3,2,3,2))
  astsa::lag1.plot(ElimiTenddian, 12,corr=F)
  ts_lags(ElimiTenddian,lags=1:12)"
})
output$grafretardos_2<-renderPlot({
  par(mar = c(3,2,3,2))
  astsa::lag1.plot(ElimiTenddian, 12,corr=F)
})
output$grafperfiles_0 <- renderText({
  "  ggseasonplot(ElimiTenddian) +
    labs(title = \"Gráfico de perfiles\")
"
})
output$grafperfiles_1<-renderPlot({
  ggseasonplot(ElimiTenddian) +
    labs(title = "Gráfico de perfiles")
})
output$indiceAMI_0 <- renderText({
  "  tseriesChaos::mutual(ElimiTenddian, partitions = 50, lag.max = 10, plot=TRUE,main=\"AMI sin tendencia y varianza estable \")
"
})
output$indiceAMI_1<-renderPlot({
  tseriesChaos::mutual(ElimiTenddian, partitions = 50, lag.max = 10, plot=TRUE,main="AMI sin tendencia y varianza estable ")
})
output$monthlyaverage_1 <- renderText({
"dian2sint_df <- data.frame(year = floor(time(ElimiTenddian)), month = cycle(ElimiTenddian),ElimiTenddian = as.numeric(ElimiTenddian))
dian2sint_df$month <- factor(month.abb[dian2sint_df$month], levels = month.abb)
dian2sint_summary <- dian2sint_df %>%group_by(month) %>%summarise(mean= mean(ElimiTenddian),sd = sd(ElimiTenddian))
dian2sint_summary
plot_ly (data = dian2sint_summary, x = ~ month, y = ~ mean, type = \"bar\", name   = \"Mean\") %>%
  layout (title = \"dian2sint - Monthly Average\", yaxis =list(title = \"Mean\",   range = c(min(dian2sint_summary$mean), max(dian2sint_summary$mean))))
"
})
output$monthlyaverage_2<-renderPlotly({
  plot_ly (data = dian2sint_summary, x = ~ month, y = ~ mean, type = "bar", name   = "Mean") %>%
    layout (title = "dian2sint - Monthly Average", yaxis =list(title = "Mean",   range = c(min(dian2sint_summary$mean), max(dian2sint_summary$mean))))
})
output$heatmap_0 <- renderText({
  "TSstudio::ts_heatmap(ElimiTenddian,title = \"Mapa de Calor - Impuestos Dian sin tendencia\")
  TSstudio::ts_heatmap(diff(ldian2),title = \"Mapa de Calor - Impuestos Dian sin tendencia\")"
})
output$heatmap_1<-renderPlotly({
  TSstudio::ts_heatmap(ElimiTenddian,title = "Mapa de Calor - Impuestos Dian sin tendencia (por lm)")
})
output$heatmap_2<-renderPlotly({
  TSstudio::ts_heatmap(diff(ldian2),title = "Mapa de Calor - Impuestos Dian sin tendencia (primera diferenciación")
})
output$subseries_0 <- renderText({
  "dian2_tsbl_notend=as_tsibble(ElimiTenddian)
  dian2_tsbl_notend%>%gg_subseries(value)"
})
output$subseries_1<-renderPlot({
  dian2_tsbl_notend%>%gg_subseries(value)
})
output$periodograma_0 <- renderText({
  "spectrum(ElimiTenddian,log='no')
abline(v=1/12, lty=2,col=\"red\")
spectrum(ElimiTenddian,log='no',span=5)
spectrum(ElimiTenddian,log='no',span=c(5,5))
spectrum(ElimiTenddian,log='no',span=c(2,2))

Periodgramadldian2_sintendencia=spectrum(as.numeric(ElimiTenddian),log='no')
ubicacionlogdian=which.max(Periodgramadldian2_sintendencia$spec)
sprintf(\"El valor de la frecuencia donde se máximiza el periodograma para la serie es: %s\",Periodgramadldian2_sintendencia$freq[ubicacionlogdian])

sprintf(\"El periodo correspondiente es aproximadamente: %s\",1/Periodgramadldian2_sintendencia$freq[ubicacionlogdian])"
})
output$periodograma_1<-renderPlot({
  spectrum_result <- spectrum(ElimiTenddian, log = 'no', span = 5)
  
  # Crea una gráfica de espectro interactiva con plotly
  spectrum_plot <- plot_ly(x = spectrum_result$freq, y = spectrum_result$spec, type = 'scatter', mode = 'lines')
  
  # Agrega etiquetas y título
  spectrum_plot <- spectrum_plot %>%
    layout(title = "Análisis de Espectro",
           xaxis = list(title = "Frecuencia"),
           yaxis = list(title = "Energía"))
})
output$periodograma_2<-renderPrint({
sprintf("El valor de la frecuencia donde se máximiza el periodograma para la serie es: %s",Periodgramadldian2_sintendencia$freq[ubicacionlogdian])
})
output$periodograma_3<-renderPrint({
sprintf("El periodo correspondiente es aproximadamente: %s",1/Periodgramadldian2_sintendencia$freq[ubicacionlogdian])
})
  ###################Serie de Energia ##############
  output$codigo2<-renderText({
   "AEP_hourly<-read.csv(\"AEP_hourly.csv\")
    AEP_hourly$Datetime<-as.POSIXct(AEP_hourly$Datetime, format = \"%Y-%m-%d %H:%M:%S\")
    AEP_hourly$fecha<-as.Date(AEP_hourly$Datetime)
    energia <- AEP_hourly %>%
      group_by(fecha) %>%
      summarise(Energia = sum(AEP_MW))
    energia<-energia[-5055,]
    energia2<-ts(energia$Energia,start=c(2004,10),frequency=365)
    plot(energia2, main=\"Serie de tiempo de la energía diaria consumida\",
         cex.main=1,
         xlab=\"Tiempo\",
         ylab=\"Energía consumida\",
         cex.lab=0.4)"
  })
  output$serie_energia1<-renderPlot({
    AEP_hourly<-read.csv("AEP_hourly.csv")
    AEP_hourly$Datetime<-as.POSIXct(AEP_hourly$Datetime, format = "%Y-%m-%d %H:%M:%S")
    AEP_hourly$fecha<-as.Date(AEP_hourly$Datetime)
    energia <- AEP_hourly %>%
      group_by(fecha) %>%
      summarise(Energia = sum(AEP_MW))
    energia<-energia[-5055,]
    energia2<-ts(energia$Energia,start=c(2004,10),frequency=365)
    plot(energia2, main="Serie de tiempo de la energía diaria consumida",
         cex.main=1,
         xlab="Tiempo ",
         ylab="Energía consumida",
         cex.lab=0.4)
  })
  
  output$boxcox_ener_text<-renderText({
    "library(forecast)
    MASS::boxcox(lm(energia2 ~ 1),seq(-5, 5, length = 50))
    forecast::BoxCox.lambda(energia2, method =\"loglik\",
                        lower = -1, upper = 3)
    plot(forecast::BoxCox(energia2,lambda=-0.25))  
    par(mar = c(1,1,1,1))
    lenergia2=log(energia2)
    MASS::boxcox(lm(lenergia2 ~ 1),seq(-5, 5, length = 50))
    abline(v = 1, col = \"red\", lty = 2)
    par(mfrow=c(2,1))
    plot(energia2,main=\"Serie energia sin Transformar\")
    plot(lenergia2,main=\"Series energia con Transformación BoxCox\")"
  })
  output$boxcox_ener_plot_1<-renderPlot({
    MASS::boxcox(lm(energia2 ~ 1),seq(-5, 5, length = 50))
  })
  output$boxcox_ener_plot_2<-renderPrint({
    forecast::BoxCox.lambda(energia2, method ="loglik",
                            lower = -1, upper = 3)
  })
  output$boxcox_ener_plot_3<-renderPlot({
    MASS::boxcox(lm(lenergia2 ~ 1),seq(-5, 5, length = 50))
    abline(v = 1, col = "red", lty = 2)
  })
  output$boxcox_ener_plot_4<-renderPlot({
    par(mfrow=c(2,1))
    plot(energia2,main="Serie energia sin Transformar")
    plot(lenergia2,main="Series energia con Transformación BoxCox")
  })
  
  output$Serienotend_1 <- renderText({
    "dian3<-window(ldian2, start = c(2000,1))
ts_plot(dian3,title=\"Serie de tiempo del recaudo mensual interno\",
        Ytitle=\"Recaudo interno\",
        Xtitle=\"Tiempo\",
        Xgrid=TRUE,
        Ygrid=TRUE)
dygraph(dian3,main=\"Serie de tiempo del recaudo mensual interno\",
            ylab=\"Recaudo interno\",
            xlab=\"Tiempo\")"
  })
  output$Serienovar_plot_1<-renderDygraph({
    dian3<-window(ldian2, start = c(2000,1))
    dygraph(dian3,main="Serie de tiempo del recaudo mensual interno",
            ylab="Recaudo interno",
            xlab="Tiempo")
    #ts_plot(dian3,title="Serie de tiempo del recaudo mensual interno",
    #Ytitle="Recaudo interno",
    #Xtitle="Tiempo",
    #Xgrid=TRUE,
    #Ygrid=TRUE)
    
  })
}


shinyApp(ui = ui, server = server)

# Preguntas ####
# Si en la serie de energia el lambda del boxcox dio 0.35 podemos aproximar a 0, y usar log?


