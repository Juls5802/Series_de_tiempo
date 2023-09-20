## Instalación de paquetes ####
library(feasts)
library(fable)
library(timetk)
library(tsibble)
library(zoo)
library(xts)
library(readxl)
library(tidyverse)

### Transformacion de box cox ####
library(forecast)
MASS::boxcox(lm(dian2 ~ 1),seq(-5, 5, length = 50)) ##Notese que no captura al 1
forecast::BoxCox.lambda(dian2, method ="loglik",
                        lower = -1, upper = 3)#Entrega el valor de lambda (0.1), cercano a cero o sea que vale la pena pensar en una transformación logaritmica.
plot(forecast::BoxCox(dian2,lambda=0.1)) 
par(mar = c(1,1,1,1))
ldian2=log(dian2)

MASS::boxcox(lm(ldian2 ~ 1),seq(-5, 5, length = 50)) #Si captura al 1
par(mfrow=c(2,1))
plot(dian2,main="Serie Dian sin Transformar")
plot(ldian2,main="Series Dian con Transformación BoxCox")

#### Analisis descriptivo de la base de datos DIAN ######
class(ldian2)

dian3<-window(ldian2, start = c(2000,1))
# Gráfico interactivo
ts_plot(dian3,title="Serie de tiempo del recaudo mensual interno",
        Ytitle="Recaudo interno",
        Xtitle="Tiempo",
        Xgrid=TRUE,
        Ygrid=TRUE)
### Estimación preliminar de la tendencia con objeto tibble (gráfico más bonito)####
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
par(mfrow=c(1,1))
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
summary(fit<-lm(ldian2~time(ldian2),na.action=NULL))
plot(ldian2,ylab="Recaudo interno") 
abline(fit,col="darkcyan",lwd=2)
#Eliminamos la tendencia con la predicción de la recta
ElimiTenddian<-ldian2-predict(fit)
plot(ElimiTenddian,main="Serie Dian sin tendencia y con varianza estable",
     cex.main=1.3,
     xlab="Tiempo",
     ylab="Recaudo interno",
     cex.lab=0.4)
acf(ElimiTenddian,lag.max=179)

###Loess para hacer el ajuste de la tendencia ####

### Descomposición con filtro de promedios moviles ####
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
          season(window = "periodic"),
        robust = TRUE)) %>%
  components() %>%
  autoplot()
tsibble_dian_notendstl<-tsibble_dian$Log
##
## Aqui podriamos colocar lo que sale en el archivo ejemplodescriptiva.qmd
## Para eliminar la tendencia con este metodo.
##

### Aun no se que es esto pero quita tendencia (creo que hace lo mismo que al quitarla con lm X2) xd ####
par(mar = c(2,2,2,2))
fitdian = lm(ldian2~time(ldian2), na.action=NULL) # Regresión sobre el tiempo igual que el objeto 
par(mfrow=c(2,1))
plot(resid(fitdian), type="l", main="sin tendencia") #Sin tendencia quitandola con regresión
plot(diff(dian2), type="l", main="Primera Diferencia") #Primera diferencia ordinaria
## Gráfico de los acf
par(mar = c(3,2,3,2))
par(mfrow=c(3,1)) # plot ACFs
acf(ldian2, 60, main="ACF Dian objeto ts varianza estable por boxcox") # la tendencia confunde resto
acf(resid(fitdian), 60, main="ACF Sin tendencia (resid(fitdian))") 
acf(diff(dian2), 60, main="ACF Primera Diferencia")
#Notese que no bajan tan rapido sino que lo hacen lentico 
#y tiene un ciclo estacional
## Gráfica de retardos con varianza estable #####
ts_info(ldian2)
par(mar = c(3,2,3,2))
astsa::lag1.plot(ldian2, 12,corr=F)
ts_lags(ldian2,lags=1:12)
# Grafica retardos sin tendencia y varianza estable
ts_info(ElimiTenddian)
par(mar = c(3,2,3,2))
astsa::lag1.plot(ElimiTenddian, 12,corr=F)
ts_lags(ElimiTenddian,lags=1:12)
#### Gráfico  de perfiles 
ggseasonplot(ElimiTenddian)
##ACF y PACF #####
par(mfrow=c(3,1))
par(mar = c(2.7,2,2.7,2))
acf(ElimiTenddian, 48, main="Impuestos Dian tendencia eliminada con lm y varianza estable")
acf(resid(fitdian), 48, main="Impuestos Dian la otra forma de quitar tendencia y varianza estable")
acf(diff(ldian2),48,main="Impuestos Dian diferenciación variazna estable")

## Indice AMI 
library(nonlinearTseries)
library(tseriesChaos)
tseriesChaos::mutual(ldian2, partitions = 50, lag.max = 10, plot=TRUE,main="AMI con tendencia y varianza estable")
tseriesChaos::mutual(ElimiTenddian, partitions = 50, lag.max = 10, plot=TRUE,main="AMI sin tendencia y varianza estable ")

## Detección de cíclos y estacionalidades ####
TSstudio::ts_heatmap(ldian2,title = "Mapa de Calor - Impuestos Dian con tendencia") # no se nota la estacionalidad
TSstudio::ts_heatmap(ElimiTenddian,title = "Mapa de Calor - Impuestos Dian sin tendencia") # si se nota la estacionalidad
TSstudio::ts_heatmap(diff(ldian2),title = "Mapa de Calor - Impuestos Dian sin tendencia")

## Otras medidas para observar la estacionalidad.
## Con la serie con tendencia y varianza estable
library(dplyr)
library(plotly)
dian2_df <- data.frame(year = floor(time(ldian2)), month = cycle(ldian2),ldian2 = as.numeric(ldian2))
dian2_df$month <- factor(month.abb[dian2_df$month], levels = month.abb)
dian2_summary <- dian2_df %>%group_by(month) %>%summarise(mean= mean(ldian2),sd = sd(ldian2))
dian2_summary
plot_ly (data = dian2_summary, x = ~ month, y = ~ mean, type = "bar", name   = "Mean") %>%
  layout (title = "ldian2 - Monthly Average", yaxis =list(title = "Mean",   range = c(15, 16)))
## Con la serie sin tendencia eliminada por lm y con la varianza estable ####
dian2sint_df <- data.frame(year = floor(time(ElimiTenddian)), month = cycle(ElimiTenddian),ElimiTenddian = as.numeric(ElimiTenddian))
dian2sint_df$month <- factor(month.abb[dian2sint_df$month], levels = month.abb)
dian2sint_summary <- dian2sint_df %>%group_by(month) %>%summarise(mean= mean(ElimiTenddian),sd = sd(ElimiTenddian))
dian2sint_summary
plot_ly (data = dian2sint_summary, x = ~ month, y = ~ mean, type = "bar", name   = "Mean") %>%
  layout (title = "dian2sint - Monthly Average", yaxis =list(title = "Mean",   range = c(min(dian2sint_summary$mean), max(dian2sint_summary$mean))))

## Gráfico de subseries con varianza estabilizada ####
require(feasts)
dian2_tsbl=as_tsibble(ldian2)
dian2_tsbl%>%gg_subseries(value)
## Gráfico de subseries sin tendencia y con varianza estabilizada
require(feasts)
dian2_tsbl_notend=as_tsibble(ElimiTenddian)
dian2_tsbl_notend%>%gg_subseries(value)
## Usando regresión para descubrir un ciclo (periodograma) ####
##Periodograma 
#con tendencia y varianza estabilizada
spectrum(ldian2,log='no')
abline(v=1/12, lty=2,col="red")
spectrum(ldian2,log='no',span=5)
spectrum(ldian2,log='no',span=c(5,5))
spectrum(ldian2,log='no',span=c(2,2))
#Sin tendencia 
spectrum(ElimiTenddian,log='no')
abline(v=1/12, lty=2,col="red")
spectrum(ElimiTenddian,log='no',span=5)
spectrum(ElimiTenddian,log='no',span=c(5,5))
spectrum(ElimiTenddian,log='no',span=c(2,2))

Periodgramadldian2_sintendencia=spectrum(as.numeric(ElimiTenddian),log='no')
ubicacionlogdian=which.max(Periodgramadldian2_sintendencia$spec)
sprintf("El valor de la frecuencia donde se máximiza el periodograma para la serie es: %s",Periodgramadldian2_sintendencia$freq[ubicacionlogdian])

sprintf("El periodo correspondiente es aproximadamente: %s",1/Periodgramadldian2_sintendencia$freq[ubicacionlogdian])
### Modelamiento de la estacionalidad #####
