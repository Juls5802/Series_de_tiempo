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
library(forecast)
library(plotly)


#### Base de datos de la DIAN #####
dian<-read_excel("dian.xlsx", range="A7:C313", sheet = "Rec mensual a junio 2023" )
años<-2000:2023
dian<-dplyr::filter(dian,Año %in% años)
colnames(dian)<-c("Año","Mes","Impuestos")
dian$fecha<-as.Date(paste(dian$Año, dian$Mes, "1", sep = "-"), format = "%Y-%B-%d")
dian<-dian[,3:4]

## Serie de tiempo de la DIAN ##
dian2<-ts(dian$Impuestos,start=c(2000,01),frequency=12)
plot(dian2, main="Serie de tiempo del recaudo mensual interno",
     cex.main=1.3,
     xlab="Tiempo",
     ylab="Recaudo interno",
     cex.lab=0.4)


#### Base de datos de energia ####
AEP_hourly<-read.csv("AEP_hourly.csv")
AEP_hourly$Datetime<-as.POSIXct(AEP_hourly$Datetime, format = "%Y-%m-%d %H:%M:%S")
AEP_hourly<-AEP_hourly %>% arrange(Datetime)
AEP_hourly$fecha<-as.Date(AEP_hourly$Datetime)

energia <- AEP_hourly %>%
  group_by(fecha) %>%
  summarise(Energia = sum(AEP_MW))
energia<-energia[-5055,]

write_csv(energia, "energia.csv")

## Serie de tiempo de la energia ##
energia2<-ts(energia$Energia,start=c(2004,10,01),frequency=365.25)
plot(energia2, main="Serie de tiempo de la energía diaria de una empresa estadounidense",
     cex.main=1.3,
     xlab="Tiempo ",
     ylab="Energía consumida",
     cex.lab=0.4)

plot(as.ts(energia$Energia))