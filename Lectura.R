#Librerías necesarias
library(readxl)
library(sqldf)
library(ggplot2)
library(car)
library(rgl)
library(dplyr)
library(RColorBrewer)

#Carga de datos
setwd("C:/Users/Irene/Desktop/Irene/Trabajos/Master/TFM/datos y EDA/datos")  #directorio 
OU <- read.csv(file = 'traficoUIC_OU.csv', header = TRUE, sep = ";")
OV <- read.csv(file = 'traficoUIC_OV.csv', header = TRUE, sep = ";")
TRK_enejun_2019 <- read.csv(file = 'TRK_enero2019 a junio2019.csv', header = TRUE, sep = ",")
TRK_julabr_1921 <- read.csv(file = 'TRK_julio2019 a abril 2021.csv', header = TRUE, sep = ",")
fb_ads <- read_excel("Display-brand-facebook-ads.xlsx")
google_ads <- read_excel("Google ads campañas de marca display.xlsx") 

ddComp <- read.csv(file = 'ddCompilado.csv', header = TRUE, sep = ";")
gads2 <- read.csv(file = 'google_ads2.csv', header = TRUE, sep = ";")
ddTOT <- read.csv(file = 'ddTOT.csv', header = TRUE, sep = ";")

#Transformación de las variables mal codificadas
OU$date<- lubridate::ymd(OU$date)
OV$date<- lubridate::ymd(OV$date)
google_ads$Día<- lubridate::ymd(google_ads$Día)
gads2$Día<- lubridate::ymd(gads2$Día)
ddTOT$date<- lubridate::ymd(ddTOT$date)

OU$sessionsPerUser<-as.numeric(gsub(",",".",OU$sessionsPerUser))
OU$pageviewsPerSession<-as.numeric(gsub(",",".",OU$pageviewsPerSession))
OU$avgTimeOnPage<-as.numeric(gsub(",",".",OU$avgTimeOnPage))
OV$sessionsPerUser<-as.numeric(gsub(",",".",OV$sessionsPerUser))
OV$pageviewsPerSession<-as.numeric(gsub(",",".",OV$pageviewsPerSession))
OV$avgTimeOnPage<-as.numeric(gsub(",",".",OV$avgTimeOnPage))

#cogemos solo españa
OU<-OU[OU$country=="Spain",]
OV<-OV[OV$country=="Spain",]

#Unión de los dos datasets OU y OV
ddTOT<-merge(OU,OV, all=TRUE)

#eliminar los registros que channelGrouping sea Afiliados y (other) equivale a un 8'9%
ddTOT$region = ifelse(ddTOT$region == "Community of Madrid", "Madrid", ddTOT$region)
ddTOT<-sqldf('SELECT * FROM ddTOT
         WHERE channelGrouping NOT IN ("Afiliados", "(Other)")')
ddTOT<-ddTOT[,2:16] #quito la primera variable X que nos sirve pa na

#compilar bien las regiones
ddTOT$region = ifelse(ddTOT$region == "Community of Madrid", "Madrid", ddTOT$region)
ddTOT$region = ifelse(ddTOT$region == "Valencian Community", "Valencia", ddTOT$region)
ddTOT$region = ifelse(ddTOT$region == "Pontevedra", "Galicia", ddTOT$region)
ddTOT$region = ifelse(ddTOT$region == "Barcelona", "Catalonia", ddTOT$region)
ddTOT$region = ifelse(ddTOT$region == "Tarragona", "Catalonia", ddTOT$region)
ddTOT$region = ifelse(ddTOT$region == "Santa Cruz de Tenerife", "Canary Islands", ddTOT$region)
ddTOT$region = ifelse(ddTOT$region == "Alicante", "Valencia", ddTOT$region)
ddTOT$region = ifelse(ddTOT$region == "Zaragoza", "Aragon", ddTOT$region)
ddTOT$region = ifelse(ddTOT$region == "Alava", "Basque Country", ddTOT$region)
ddTOT$region = ifelse(ddTOT$region == "Biscay", "Basque Country", ddTOT$region)
ddTOT$region = ifelse(ddTOT$region == "Cadiz", "Andalusia", ddTOT$region)
ddTOT$region = ifelse(ddTOT$region == "Gipuzkoa", "Basque Country", ddTOT$region)
ddTOT$region = ifelse(ddTOT$region == "Salamanca", "Castile and Leon", ddTOT$region)
ddTOT$region = ifelse(ddTOT$region == "Segovia", "Castile and Leon", ddTOT$region)
ddTOT$region = ifelse(ddTOT$region == "Region of Murcia", "Murcia", ddTOT$region)

ddComp$channelGrouping = ifelse(ddComp$channelGrouping == "Branded Paid Search", "IE", ddComp$channelGrouping)
ddComp$channelGrouping = ifelse(ddComp$channelGrouping == "Organic Search", "IE", ddComp$channelGrouping)
ddComp$channelGrouping = ifelse(ddComp$channelGrouping == "Email", "CRM", ddComp$channelGrouping)
ddComp$channelGrouping = ifelse(ddComp$channelGrouping == "Push", "CRM", ddComp$channelGrouping)

#si necesitamos exportar los csv
write.csv2(ddComp, "ddCompilado2.csv")

