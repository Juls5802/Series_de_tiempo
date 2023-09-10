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
class(dian2)

# La serie es medida mensual, es decir, 
# presenta una frecuencia de 12. 
# Presenta  tendencia (aumenta), ciclos estacionales 
# (cada año mas o menos), varianza marginal 
# no constante (aumenta).

dian3<-window(dian2, start = c(2000,1))
ts_plot(dian3,title="Serie de tiempo del recaudo mensual interno",
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
tbl_dian%>%plot_time_series(.value=impuesto,.date_var=fecha)#gráfica timetk


### Analisis de tendencia con regresion simple ####
summary(fit<-lm(dian2~time(dian2),na.action=NULL))
plot(dian2,ylab="Recaudo interno") 
abline(fit,col="darkcyan",lwd=2)
#Eliminamos la tendencia con la predicción de la recta
ElimiTenddian<-dian2-predict(fit)
plot(ElimiTenddian,main="Serie Dian sin tendencia",
     cex.main=1.3,
     xlab="Tiempo",
     ylab="Recaudo interno",
     cex.lab=0.4)
acf(ElimiTenddian,lag.max=179)

###Loess para hacer el ajuste de la tendencia ####

### Descomposición ####
dian_decompo=decompose(dian2)
plot(dian_decompo)
dian_decompo$trend
### DEscomposición STL ####
##Algunas librerias
library(feasts)
library(fable)
### Gráfico ##
tsibble_dian<-as_tsibble(dian2)
str(tsibble_dian)
tsibble_dian %>%
  model(
    STL(value ~ trend() +
          season(window = "periodic"),
        robust = TRUE)) %>%
  components() %>%
  autoplot()


### Aun no se que es esto pero quita tendencia xd ####
par(mar = c(2,2,2,2))
fitdian = lm(dian2~time(dian2), na.action=NULL) # Regresión sobre el tiempo
par(mfrow=c(2,1))
plot(resid(fitdian), type="l", main="sin tendencia") #Sin tendencia quitandola con regresión
plot(diff(dian2), type="l", main="Primera Diferencia") #Primera diferencia ordinaria
## Gráfico de los acf
par(mar = c(3,2,3,2))
par(mfrow=c(3,1)) # plot ACFs
acf(dian2, 60, main="ACF Dian")
acf(resid(fitdian), 60, main="ACF Sin tendencia") 
acf(diff(dian2), 60, main="ACF Primera Diferencia")
#Notese que no bajan tan rapido sino que lo hacen lentico 
#y tiene un ciclo estacional
### Transformacion de box cox ####
library(forecast)
forecast::BoxCox.lambda(dian2, method ="loglik",
                        lower = -1, upper = 3)#Entrega el valor de lambda (0.1).
plot(forecast::BoxCox(dian2,lambda=0.1))
par(mar = c(1,1,1,1))
par(mfrow=c(2,1))
ldian2=log(dian2)
plot(dian2,main="Serie Dian sin Transformar")
plot(ldian2,main="Series con Transformación BoxCox")

## Gráfica de retardos #####
ts_info(dian2)
par(mar = c(3,2,3,2))
astsa::lag1.plot(dian2, 12,corr=F)

ts_lags(soi,lags=1:12)
## Preguntar si estos graficos se hacen sin tendencia.
##ACF y PACF #####
par(mfrow=c(3,1))
par(mar = c(2.7,2,2.7,2))
acf(ElimiTenddian, 48, main="Impuestos Dian tendencia eliminada con lm")
acf(resid(fitdian), 48, main="Impuestos Dian la otra forma de quitar tendencia")
acf(diff(dian2),48,main="Impuestos Dian diferenciación")

## Indice AMI 
library(nonlinearTseries)
library(tseriesChaos)
tseriesChaos::mutual(dian2, partitions = 50, lag.max = 10, plot=TRUE)
tseriesChaos::mutual(ElimiTenddian, partitions = 50, lag.max = 10, plot=TRUE)

## Detección de cíclos y estacionalidades ####
TSstudio::ts_heatmap(dian2,title = "Mapa de Calor - Impuestos Dian con tendencia")
TSstudio::ts_heatmap(ElimiTenddian,title = "Mapa de Calor - Impuestos Dian sin tendencia")
TSstudio::ts_heatmap(diff(dian2),title = "Mapa de Calor - Impuestos Dian sin tendencia")

## Otras medidas para observar la estacionalidad.
## Con la serie con tendencia 
library(dplyr)
library(plotly)
dian2_df <- data.frame(year = floor(time(dian2)), month = cycle(dian2),dian2 = as.numeric(dian2))
dian2_df$month <- factor(month.abb[dian2_df$month], levels = month.abb)
dian2_summary <- dian2_df %>%group_by(month) %>%summarise(mean= mean(dian2),sd = sd(dian2))
dian2_summary
plot_ly (data = dian2_summary, x = ~ month, y = ~ mean, type = "bar", name   = "Mean") %>%
  layout (title = "dian2 - Monthly Average", yaxis =list(title = "Mean",   range = c(4136400, 9148475)))
## Falta hacer el mismo analisis anterior pero sin tendencia ####
dian2sint_df <- data.frame(year = floor(time(ElimiTenddian)), month = cycle(ElimiTenddian),ElimiTenddian = as.numeric(ElimiTenddian))
dian2sint_df$month <- factor(month.abb[dian2sint_df$month], levels = month.abb)
dian2sint_summary <- dian2sint_df %>%group_by(month) %>%summarise(mean= mean(ElimiTenddian),sd = sd(ElimiTenddian))
dian2sint_summary
plot_ly (data = dian2sint_summary, x = ~ month, y = ~ mean, type = "bar", name   = "Mean") %>%
  layout (title = "dian2sint - Monthly Average", yaxis =list(title = "Mean",   range = c(min(dian2sint_summary$mean), max(dian2sint_summary$mean))))

## Gráfico de subseries ####
dian2_tsbl=as_tsibble(dian2)
require(feasts)
dian2_tsbl=as_tsibble(dian2)
dian2_tsbl%>%gg_subseries(value)

## Usando regresión para descubrir un ciclo (periodograma) ####
##Periodograma 
#con tendencia 
spectrum(dian2,log='no')
abline(v=1/12, lty=2,col="red")
spectrum(dian2,log='no',span=5)
spectrum(dian2,log='no',span=c(5,5))
spectrum(dian2,log='no',span=c(2,2))
#Sin tendencia 
spectrum(ElimiTenddian,log='no')
abline(v=1/12, lty=2,col="red")
spectrum(ElimiTenddian,log='no',span=5)
spectrum(ElimiTenddian,log='no',span=c(5,5))
spectrum(ElimiTenddian,log='no',span=c(2,2))

