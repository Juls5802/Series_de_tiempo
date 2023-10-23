# Librerias ####
library(TSstudio)
library(readxl)
library(dplyr)
library(lubridate)
library(astsa)
library(feasts)
library(fable)
library(timetk)
library(tsibble)
library(zoo)
library(xts)
library(readxl)
library(tidyverse)
library(nonlinearTseries)
library(tseriesChaos) 
library(forecast)
library(plotly)

# Base de datos ####
setwd("C:\\Users\\soffy\\OneDrive\\Escritorio\\Universidad\\Series")

# Carga de la base de datos
AEP_hourly<-read.csv("AEP_hourly.csv")
AEP_hourly$Datetime<-as.POSIXct(AEP_hourly$Datetime, format = "%Y-%m-%d %H:%M:%S")
AEP_hourly$fecha<-as.Date(AEP_hourly$Datetime)

energia <- AEP_hourly %>%
  group_by(fecha) %>%
  summarise(Energia = sum(AEP_MW))
energia<-energia[-5055,]
energia2<-ts(energia$Energia,start=c(2004,10,01),frequency=365.25)

# Estabilización de la varianza ####

### Transformacion de box cox con objeto ts ####
# Miramos a ver si 1 está en el intervalo para ver si transformamos o no
MASS::boxcox(lm(energia2 ~ 1),seq(-5, 5, length = 50))
abline(v = 1, col = "red", lty = 2)

# Dice que lambda es -0.25, o sea 0, o sea logaritmo
forecast::BoxCox.lambda(energia2, method ="loglik",lower = -1, upper = 3)
lenergia2=log(energia2)

# El 1 sigue sin estar en el intervalo entonces trabajamos con la serie original
MASS::boxcox(lm(lenergia2 ~ 1),seq(-5, 5, length =  50))
abline(v = 1, col = "red", lty = 2)

# En las gráficas vemos que realmente no cambia mucho entonces vamos a dejar la serie original
par(mar = c(1,1,1,1))
par(mfrow=c(2,1),mar=c(3,3,3,3))
plot(energia2,main="Serie energía sin Transformar",cex.main=1)
plot(lenergia2,main="Serie energía con Transformación BoxCox",cex.main=1)

# Analisis descriptivo de la base de datos energia sin varianza
energia3<-window(energia2, start = c(2004,10))
ts_plot(energia3,title="Serie de tiempo del recaudo mensual interno",
        Ytitle="Recaudo interno",
        Xtitle="Tiempo",
        Xgrid=TRUE,
        Ygrid=TRUE)

# Estimación preliminar de la tendencia ####

# Creación del objeto tibble
energia_1=energia %>% map_df(rev)
Fechas=as.Date(energia_1$fecha)
energia_xts=xts(x = energia_1$Energia,frequency = 365.25,order.by = Fechas)

# Creación objeto tssible a partir del objeto tibble
df_energia=data.frame(Energia=energia_1$Energia,fecha=energia_1$fecha)
tbl_energia=tibble(df_energia)
tbl_energia_format_fecha=tbl_energia
tsbl_energia=as_tsibble(tbl_energia_format_fecha,index=fecha)

### Regresion lineal simple ####
# Análisis de tendencia con regresion simple
summary(fit_e<-lm(energia2~time(energia2),na.action=NULL))

# Como que sí hay tendencia y la estimación lineal parece ser buena
plot(energia2, main="Serie de tiempo de la energía diaria de una empresa estadounidense",
     cex.main=1,
     xlab="Tiempo ",
     ylab="Energía consumida",
     cex.lab=0.4)
abline(fit_e,col="darkcyan",lwd=2)

# Eliminación de la tendencia con la predicción la recta
ElimiTendenerg<-energia2-predict(fit_e)
plot(ElimiTendenerg,main="Serie energía sin tendencia",
     cex.main=1.3,
     xlab="Tiempo",
     ylab="Consumo de energía",
     cex.lab=0.4)

### Filtro Promedios móviles####
energia_decompo=decompose(energia2)
plot(energia_decompo)
energia_decompo$trend # Hay NA's :(

### STL (no está funcionando)####
#tsibble_energia<-as_tsibble(energia2)
#tsibble_energia <- tsibble_energia %>%
#  fill_gaps()
#str(tsibble_energia)
#tsibble_energia %>%
#  model(
#    STL(value ~ trend() +
#          season(window = "periodic"),
#        robust = TRUE)) %>%
#  components() %>%
#  autoplot()

### Primera diferenciación ####
tsibble_energia<-as_tsibble(energia2)
par(mar = c(2,2,2,2))
par(mfrow=c(2,1))

plot(resid(fit_e), type="l", main="Sin tendencia lineal") 
plot(diff(energia2), type="l", main="Primera Diferencia") 

### Comparación ACF ####
# Gráficos de los ACF
par(mar = c(3,2,3,2))
par(mfrow=c(3,1))
acf(energia2, 60, main="ACF energia")
acf(resid(fit_e), 60, main="ACF Sin tendencia lineal") 
acf(diff(energia2), 60, main="ACF Primera Diferencia") # parece que es mejor trabajar la serie con la primera diferencia pq acf oscila y cae rapidamente

# Gráficos de los PACF
par(mar = c(3,2,3,2))
par(mfrow=c(3,1))
pacf(energia2, 60, main="ACF energia") # baja lento, tendencia
pacf(resid(fit_e), 60, main="ACF Sin tendencia lineal") # baja lento, posible tendencia
pacf(diff(energia2), 60, main="ACF Primera Diferencia") # parece que es mejor trabajar la serie con la primera diferencia pq acf oscila y cae rapidamente

# Gráfica de retardos #####

# Original
par(mar = c(3, 2, 3, 2))
astsa::lag1.plot(energia2, 7,corr=F)

# Sin tendencia lineal
ts_info(ElimiTendenerg)
par(mar = c(3, 2, 3, 2))
astsa::lag1.plot(ElimiTendenerg, 7,corr=F)

# Sin tendencia Primera diferencia
par(mar = c(3, 2, 3, 2))
astsa::lag1.plot(diff(energia2), 7,corr=F) # como que se ve una posible relación lineal con el retardo 7

#ts_lags(ElimiTendenerg,lags=1:7)

# Indice AMI #### 
tseriesChaos::mutual(energia2, partitions = 50, lag.max = 10, plot=TRUE) # AMI serie con tendencia
tseriesChaos::mutual(ElimiTendenerg, partitions = 50, lag.max = 10, plot=TRUE) # AMI serie sin tendencia lineal
tseriesChaos::mutual(diff(energia2), partitions = 50, lag.max = 10, plot=TRUE) # AMI serie sin tendencia primera diferencia, el septimo rezago es el que me da más info

# Detección de cíclos y estacionalidades ####

### Subserie semanal ####
##### Originales ####
gg_subseries(tsbl_energia,y=Energia,period=7) # hay un cambio los sabados, domingos y lunes

##### Tendencia lineal ####
lineal_1<-cbind(as.matrix(ElimiTendenerg),as.character(energia$fecha))
lineal_1<-as.data.frame(lineal_1)
names(lineal_1)<-c("Energia","fecha")

lineal_1$Energia<-as.numeric(lineal_1$Energia)
lineal_1$fecha<-as.Date(lineal_1$fecha)

df_lineal=data.frame(Energia=lineal_1$Energia,fecha=lineal_1$fecha)
tbl_lineal=tibble(df_lineal)
tbl_lineal_format_fecha=tbl_lineal
tsbl_lineal=as_tsibble(tbl_lineal_format_fecha,index=fecha)
gg_subseries(tsbl_lineal,y=Energia,period=7) # hay un cambio los sabados, domingos y lunes

##### Diferenciacion ####
diferencia<-diff(energia2)
diferencia_1<-cbind(as.matrix(diferencia),as.character(energia$fecha[-1]))
diferencia_1<-as.data.frame(diferencia_1)
names(diferencia_1)<-c("Energia","fecha")

diferencia_1$Energia<-as.numeric(diferencia_1$Energia)
diferencia_1$fecha<-as.Date(diferencia_1$fecha)

df_diferencia=data.frame(Energia=diferencia_1$Energia,fecha=diferencia_1$fecha)
tbl_diferencia=tibble(df_diferencia)
tbl_diferencia_format_fecha=tbl_diferencia
tsbl_diferencia=as_tsibble(tbl_diferencia_format_fecha,index=fecha)
gg_subseries(tsbl_diferencia,y=Energia,period=7) # hay un cambio los sabados, domingos y lunes

### Subserie semanal ####
##### Original ####
gg_subseries(tsbl_energia,y=Energia,period=12) # no se ve nada pero puede ser por la multiple estacionalidad

##### Tendencia lineal ####
gg_subseries(tsbl_lineal,y=Energia,period=12)

##### Diferenciacion ####
gg_subseries(tsbl_diferencia,y=Energia,period=12)


## Explorando multiples estacionalidades ######

##### Diferenciada ####
diferencia<-diff(energia2)
energia_df<-cbind(as.matrix(diferencia),as.character(energia$fecha[-1]))
energia_df<-as.data.frame(energia_df)
names(energia_df)<-c("Energia","Fecha")

energia_df$Fecha<-as.Date(energia_df$Fecha)
energia_df$time = as.POSIXct(energia_df$Fecha, "%Y-%m-%d")
energia_df$weekday <- wday(energia_df$time, label = TRUE, abbr = TRUE)
energia_df$month <- factor(month.abb[month(energia_df$time)], levels =   month.abb)
head(energia_df)

# Agrupamos por mes y día
energia_df$Energia<-as.numeric(energia_df$Energia)
energia_mensual <- energia_df %>%
  dplyr::filter(weekday == "dom\\." | weekday == "mar\\." ) %>% # martes se parece al comportamiento de lunes-viernes, domingo se parece a sabado
  dplyr::group_by(weekday, month) %>%
  dplyr::summarise(mean = mean(Energia, na.rm = TRUE),
                   sd = sd(Energia, na.rm = TRUE))

# Grafico consumo (diferenciado) de energia mensual por dia
plot_ly(data = energia_mensual, x = ~ month, y = ~ mean, type =
          "bar",color = ~ weekday) %>%
  layout(title = "Promedio diario de energía por día de la semana",
         yaxis = list(title = "Media"),
         xaxis = list(title = "Mes"))

##### Tendencia lineal (sin) ####
energia_df<-cbind(as.matrix(ElimiTendenerg),as.character(energia$fecha))
energia_df<-as.data.frame(energia_df)
names(energia_df)<-c("Energia","Fecha")

energia_df$Fecha<-as.Date(energia_df$Fecha)
energia_df$time = as.POSIXct(energia_df$Fecha, "%Y-%m-%d")
energia_df$weekday <- wday(energia_df$time, label = TRUE, abbr = TRUE)
energia_df$month <- factor(month.abb[month(energia_df$time)], levels =   month.abb)
head(energia_df)

# Agrupamos por mes y día
energia_df$Energia<-as.numeric(energia_df$Energia)
energia_mensual <- energia_df %>%
  dplyr::filter(weekday == "dom\\." | weekday == "mar\\." ) %>% # martes se parece al comportamiento de lunes-viernes, domingo se parece a sabado
  dplyr::group_by(weekday, month) %>%
  dplyr::summarise(mean = mean(Energia, na.rm = TRUE),
                   sd = sd(Energia, na.rm = TRUE))

# Grafico consumo (diferenciado) de energia mensual por dia
plot_ly(data = energia_mensual, x = ~ month, y = ~ mean, type =
          "bar",color = ~ weekday) %>%
  layout(title = "Promedio diario de energía por día de la semana",
         yaxis = list(title = "Media"),
         xaxis = list(title = "Mes"))

##### Datos originales ####
energia_df<-energia

energia_df$time = as.POSIXct(energia_df$fecha, "%Y-%m-%d")
energia_df$weekday <- wday(energia_df$time, label = TRUE, abbr = TRUE)
energia_df$month <- factor(month.abb[month(energia_df$time)], levels =   month.abb)
head(energia_df)

# Agrupamos por mes y día
energia_df$Energia<-as.numeric(energia_df$Energia)
energia_mensual <- energia_df %>%
  dplyr::filter(weekday == "dom\\." | weekday == "mar\\." ) %>%
  dplyr::group_by(weekday, month) %>%
  dplyr::summarise(mean = mean(Energia, na.rm = TRUE),
                   sd = sd(Energia, na.rm = TRUE))

# Grafico consumo (original) de energia mensual por dia
plot_ly(data = energia_mensual, x = ~ month, y = ~ mean, type =
          "bar",color = ~ weekday) %>%
  layout(title = "Promedio diario de energía por día de la semana",
         yaxis = list(title = "Media"),
         xaxis = list(title = "Mes"))

# Estimación de la estacionalidad ####

### Periodograma ####

##### Original ####
spectrum(as.numeric(energia2),log='no')

PeriodogramaEnergia2=spectrum(as.numeric(energia2),log='no')
ubicacionenergia=which.max(PeriodogramaEnergia2$spec)

sprintf("El valor de la frecuencia donde se maximiza el periodograma para la serie es: %s",PeriodogramaEnergia2$freq[ubicacionenergia])

sprintf("El periodo correspondiente es aproximadamente: %s",1/PeriodogramaEnergia2$freq[ubicacionenergia])
# El periodo es casi 182 que es multiplo de 7, lo que daría indicios de que el periodo es de 7 días
# También puede ser indicio de que el periodo es 365 días (1 año) lo que confirma la múltiple estacionalidad

##### Tendencia lineal ####
spectrum(as.numeric(ElimiTendenerg),log='no')

PeriodogramaEnergia2_lineal=spectrum(as.numeric(ElimiTendenerg),log='no')
ubicacionlogenergia=which.max(PeriodogramaEnergia2_lineal$spec)

sprintf("El valor de la frecuencia donde se maximiza el periodograma para la serie es: %s",PeriodogramaEnergia2_lineal$freq[ubicacionlogenergia])

sprintf("El periodo correspondiente es aproximadamente: %s",1/PeriodogramaEnergia2_lineal$freq[ubicacionlogenergia])
# El periodo es casi 182 que es multiplo de 7, lo que daría indicios de que el periodo es de 7 días
# También puede ser indicio de que el periodo es 365 días (1 año) lo que confirma la múltiple estacionalida

##### Diferenciada ####
spectrum(as.numeric(diff(energia2)),log='no')

PeriodogramaEnergia2_dif=spectrum(as.numeric(diff(energia2)),log='no')
ubicacionlogenergia=which.max(PeriodogramaEnergia2_dif$spec)

sprintf("El valor de la frecuencia donde se maximiza el periodograma para la serie es: %s",PeriodogramaEnergia2_dif$freq[ubicacionlogenergia])

sprintf("El periodo correspondiente es aproximadamente: %s",1/PeriodogramaEnergia2_dif$freq[ubicacionlogenergia])
# El periodo es casi 3.5 que es multiplo de 7, lo que daría indicios de que el periodo es de 7 días
# ????? también daría indicio de que el periodo es de un año ?????

## Estimación ####
# Usando regresión para descubrir un ciclo (periodograma)
frec_ang=(2*pi/7) #w=2*pi/s, tomamos s=7 pq dijimos que 182 es multiplo de 7 y tiene más sentido un periodo de 7

##### Original ####
energia_copia=energia
str(energia_copia)

#Fourier k=1 
energia_copia$sin = sin(c(1:5054)*(1*frec_ang))
energia_copia$cos = cos(c(1:5054)*(1*frec_ang))

#Fourier k=2 
energia_copia$sin2 = sin(c(1:5054)*(2*frec_ang))
energia_copia$cos2 = cos(c(1:5054)*(2*frec_ang))

#Fourier k=3 
energia_copia$sin3 = sin(c(1:5054)*(3*frec_ang))
energia_copia$cos3 = cos(c(1:5054)*(3*frec_ang))

X<-cbind(energia_copia$sin,energia_copia$cos,energia_copia$sin2,energia_copia$cos2,energia_copia$sin3,energia_copia$cos3)
Y=energia_copia$Energia

linmodel_ciclo<-lm(Energia~1+sin+cos+sin2+cos2+sin3+cos3,data=energia_copia)
summary(linmodel_ciclo)

results_ciclo=linmodel_ciclo$fitted.values
results_ciclo<-as.data.frame(results_ciclo)
str(results_ciclo)
results_ciclo_ts<-ts(results_ciclo,start=c(2004,10,01),frequency=365.25)

plot(energia2, main="Serie de tiempo de la energía diaria de una empresa estadounidense",
     cex.main=1.3,
     xlab="Tiempo ",
     ylab="Energía consumida",
     cex.lab=0.4)
lines(results_ciclo_ts,col="red")

plot(energia2-results_ciclo_ts)

##### Tendencia Lineal (sin) ####
energia_copia<-cbind(as.matrix(ElimiTendenerg),as.character(energia$fecha))
energia_copia<-as.data.frame(energia_copia)
names(energia_copia)<-c("Energia","fecha")

energia_copia$fecha<-as.Date(energia_copia$fecha)
head(energia_copia)

#Fourier k=1 
energia_copia$sin = sin(c(1:5054)*(1*frec_ang))
energia_copia$cos = cos(c(1:5054)*(1*frec_ang))

#Fourier k=2 
energia_copia$sin2 = sin(c(1:5054)*(2*frec_ang))
energia_copia$cos2 = cos(c(1:5054)*(2*frec_ang))

#Fourier k=3 
energia_copia$sin3 = sin(c(1:5054)*(3*frec_ang))
energia_copia$cos3 = cos(c(1:5054)*(3*frec_ang))

X<-cbind(energia_copia$sin,energia_copia$cos,energia_copia$sin2,energia_copia$cos2,energia_copia$sin3,energia_copia$cos3)
Y=energia_copia$Energia

linmodel_ciclo<-lm(Energia~1+sin+cos+sin2+cos2+sin3+cos3,data=energia_copia)
summary(linmodel_ciclo)

results_ciclo=linmodel_ciclo$fitted.values
results_ciclo<-as.data.frame(results_ciclo)
str(results_ciclo)
results_ciclo_ts<-ts(results_ciclo,start=c(2004,10,01),frequency=365.25)

plot(ElimiTendenerg, main="Serie de tiempo de la energía diaria de una empresa estadounidense",
     cex.main=1.3,
     xlab="Tiempo ",
     ylab="Energía consumida",
     cex.lab=0.4)
lines(results_ciclo_ts,col="red")

plot(energia2-results_ciclo_ts)

##### Diferenciada ####
energia_copia<-cbind(as.matrix(diferencia),as.character(energia$fecha[-1]))
energia_copia<-as.data.frame(energia_copia)
names(energia_copia)<-c("Energia","fecha")

energia_copia$fecha<-as.Date(energia_copia$fecha)
head(energia_copia)

#Fourier k=1 
energia_copia$sin = sin(c(1:5053)*(1*frec_ang))
energia_copia$cos = cos(c(1:5053)*(1*frec_ang))

#Fourier k=2 
energia_copia$sin2 = sin(c(1:5053)*(2*frec_ang))
energia_copia$cos2 = cos(c(1:5053)*(2*frec_ang))

#Fourier k=3 
energia_copia$sin3 = sin(c(1:5053)*(3*frec_ang))
energia_copia$cos3 = cos(c(1:5053)*(3*frec_ang))

X<-cbind(energia_copia$sin,energia_copia$cos,energia_copia$sin2,energia_copia$cos2,energia_copia$sin3,energia_copia$cos3)
Y=energia_copia$Energia

linmodel_ciclo<-lm(Energia~1+sin+cos+sin2+cos2+sin3+cos3,data=energia_copia)
summary(linmodel_ciclo)

results_ciclo=linmodel_ciclo$fitted.values
results_ciclo<-as.data.frame(results_ciclo)
str(results_ciclo)
results_ciclo_ts<-ts(results_ciclo,start=c(2004,10,01),frequency=365.25)

plot(diferencia, main="Serie de tiempo de la energía diaria de una empresa estadounidense",
     cex.main=1.3,
     xlab="Tiempo ",
     ylab="Energía consumida",
     cex.lab=0.4)
lines(results_ciclo_ts,col="red")

plot(energia2-results_ciclo_ts)


# PREGUNTAS PROFE ####
# 1. En la serie diferenciada se ve que el ACF baja mucho más rápido que el de  la original y el de la estimación lineal de la tendencia, nos quedamos con la diferenciada pq indica que sí quitamos la tendencia?
# 1.1. Si escogemos la diferenciada F por la estimación de la estacionalidad, entonces cuantos rezagos para árboles y eso?
# 2. El PACF de las tres series no cambia mucho, entonces?
# 3. En la estimación de la estacionalidad mezclamos los dos periodos (7 y 365 días) o por aparte o solo 7?
# 4. Cómo mezclamos esos dos periodos en árboles y redes, qué retardos tomo? los ultimos 7 (o 365 pero es que ya sería mucho)?