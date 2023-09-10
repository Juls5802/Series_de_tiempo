## Instalación de paquetes ####
library(feasts)
library(fable)
library(timetk)
library(tsibble)
library(zoo)
library(xts)
library(readxl)
library(tidyverse)
#### Analisis descriptivo de la base de datos DIAN ######
class(energia2)

# La serie es medida mensual, es decir, 
# presenta una frecuencia de 12. 
# Presenta  tendencia (aumenta), ciclos estacionales 
# (cada año mas o menos), varianza marginal 
# no constante (aumenta).

energia3<-window(energia2, start = c(2004,10))
ts_plot(energia3,title="Serie de tiempo del recaudo mensual interno",
        Ytitle="Recaudo interno",
        Xtitle="Tiempo",
        Xgrid=TRUE,
        Ygrid=TRUE)
### Estimación preliminar de la tendencia####
##Algunos paquetes 
require(feasts)
require(fable)
require(timetk)
require(tsibble)
require(lubridate)
#Objeto tibble
energia_1=energia %>% map_df(rev)

#Fechas=as.yearmon(energia_1$fecha)
Fechas=as.Date(energia_1$fecha)
energia_xts=xts(x = energia_1$Energia,frequency = 365.25,order.by = Fechas)
ts_info(energia_xts)
plot(energia_xts)

###Creación objeto tssible a partir de un objeto tibble
df_energia=data.frame(Energia=energia_1$Energia,fecha=energia_1$fecha)
tbl_energia=tibble(df_energia)
tbl_energia_format_fecha=tbl_energia
#tbl_energia_format_fecha$fecha=yearmonth(tbl_energia_format_fecha$fecha)
tsbl_energia=as_tsibble(tbl_energia_format_fecha,index=fecha)

tbl_energia$fecha<-as.Date(zoo::as.yearmon(tbl_energia$fecha))

tbl_energia%>%plot_time_series(.value=Energia,.date_var=fecha)#gráfica timetk


### Analisis de tendencia con regresion simple ####
summary(fit_e<-lm(energia2~time(energia2),na.action=NULL))
plot(energia2,ylab="Consumo de energia total.") 
abline(fit_e,col="darkcyan",lwd=2)
#Eliminamos la tendencia con la predicción la recta
ElimiTendenerg<-energia2-predict(fit_e)
plot(ElimiTendenerg,main="Serie energia sin tendencia",
     cex.main=1.3,
     xlab="Tiempo",
     ylab="Consumo de energia total",
     cex.lab=0.4)
acf(ElimiTendenerg,lag.max=179)

###Loess para hacer el ajuste de la tendencia ####

### Descomposición ####
energia_decompo=decompose(energia2)
plot(energia_decompo)
energia_decompo$trend
### DEscomposición STL ####
##Algunas librerias
library(feasts)
library(fable)
### Gráfico ##
tsibble_energia<-as_tsibble(energia2)
str(tsibble_energia)
tsibble_energia %>%
  model(
    STL(value ~ trend() +
          season(window = "periodic"),
        robust = TRUE)) %>%
  components() %>%
  autoplot()
### Aun no se que es esto pero quita tendencia xd ####
par(mar = c(2,2,2,2))
fit_e = lm(energia2~time(energia2), na.action=NULL) # Regresión sobre el tiempo
par(mfrow=c(2,1))
plot(resid(fit_e), type="l", main="sin tendencia") #Sin tendencia quitandola con regresión
plot(diff(energia2), type="l", main="Primera Diferencia") #Primera diferencia ordinaria
## Gráfico de los acf
par(mar = c(3,2,3,2))
par(mfrow=c(3,1)) # plot ACFs
acf(energia2, 60, main="ACF energia")
acf(resid(fit_e), 60, main="ACF Sin tendencia") 
acf(diff(energia2), 60, main="ACF Primera Diferencia")
#Notese que no bajan tan rapido sino que lo hacen lentico 
#y tiene un ciclo estacional
### Transformacion de box cox ####
library(forecast)
forecast::BoxCox.lambda(energia2, method ="loglik",
                        lower = -1, upper = 3)#Entrega el valor de lambda (0.1).
plot(forecast::BoxCox(energia2,lambda=0.1))
par(mar = c(1,1,1,1))
par(mfrow=c(2,1))

lenergia2=log(energia2)
plot(energia2,main="Serie de energia sin transformar")
plot(lenergia2,main="Series con Transformación BoxCox")

## Gráfica de retardos #####
ts_info(energia2)
par(mar = c(3, 2, 3, 2))
astsa::lag1.plot(energia2, 7,corr=F)

ts_lags(soi,lags=1:7)
## Preguntarle al profe cual es el valor que debe ir en la funcion astsa
## Preguntar si estos graficos se hacen sin tendencia.
##ACF y PACF #####
par(mfrow=c(3,1))
par(mar = c(2.7,2,2.7,2))
acf(ElimiTendenerg, 48, main="Energia tendencia eliminada con lm")
acf(resid(fit_e ), 48, main="Energia la otra forma de quitar tendencia")
acf(diff(energia2),48,main="Energia diferenciación")
## Indice AMI 
library(nonlinearTseries)
library(tseriesChaos)
tseriesChaos::mutual(energia2, partitions = 50, lag.max = 10, plot=TRUE)
tseriesChaos::mutual(ElimiTendenerg, partitions = 50, lag.max = 10, plot=TRUE)

## Detección de cíclos y estacionalidades ####
energia3<-ts(energia$Energia,start=c(2004,10,01),frequency=365)
TSstudio::ts_heatmap(energia3,title = "Mapa de Calor - Impuestos Dian con tendencia")
#TSstudio::ts_heatmap(ElimiTendenerg,title = "Mapa de Calor - Impuestos Dian sin tendencia")#Lo dejo comentado porque aun me falta quitar la tendencia de este modo para energia3
TSstudio::ts_heatmap(diff(energia3),title = "Mapa de Calor - Impuestos Dian sin tendencia")
##Creo que no se ve nada, preguntar al profe.

## Explorando multiples estacionalidades ######
library(UKgrid)
require(TSstudio)
require(timetk)
require(feasts)
require(tsibble)
require(plotly)

view(energia2)

## Usando regresión para descubrir un ciclo (periodograma) ####
##Periodograma 
#con tendencia 
spectrum(energia2,log='no')
abline(v=1/365, lty=2,col="red")
spectrum(energia2,log='no',span=5)
spectrum(energia2,log='no',span=c(5,5))
spectrum(energia2,log='no',span=c(2,2))
#Sin tendencia 
spectrum(ElimiTendenerg,log='no')
abline(v=1/12, lty=2,col="red")
spectrum(ElimiTendenerg,log='no',span=5)
spectrum(ElimiTendenerg,log='no',span=c(5,5))
spectrum(ElimiTendenerg,log='no',span=c(2,2))
