#Carga dataset
df.merged <- read.csv(file = 'Output.csv', header = TRUE, sep = ";")

#Transformación variables
df.merged$date<- lubridate::ymd(df.merged$date)

df.merged$sessionsPerUser<-as.numeric(gsub(",",".",df.merged$sessionsPerUser))
df.merged$pageviewsPerSession<-as.numeric(gsub(",",".",df.merged$pageviewsPerSession))
df.merged$avgTimeOnPage<-as.numeric(gsub(",",".",df.merged$avgTimeOnPage))

#División del dataset para columnas deseadas
Month<-df.merged[,1]
b<-df.merged[,4:7]
c<-df.merged[,9:21]
CTR<-df.merged[,23]
e<-df.merged[,30:31]
df.merged2<-cbind(Month,b,c,CTR,e)
df.merged2$CTR<-as.numeric(gsub(",",".",df.merged2$CTR))
df.merged2$Link.Clicks<-as.integer(gsub(",",".",df.merged2$Link.Clicks))


#coef Pearson
cor.test(df.merged$CTR..Link.Click.Through.Rate., df.merged$Link.Clicks)

#Gráfico de correlaciones
y<-df.merged2[,9:16]
CTR<-df.merged2[,19]
Impressions<-df.merged2[,21]
correlaciones<-cbind(y,CTR,Impressions)
correlaciones<- correlaciones[!is.na(correlaciones$CTR),]

correl.r2<-cor(correlaciones)
correl.pval<-cor.mtest(correlaciones, conf.level = .95)
corrplot(correl.r2, p.mat=correl.pval$p, type="upper", title=all_title, insig = "label_sig", sig.level = c(.001, .01, .05), pch.cex = .9, pch.col = "white", mar=c(0,0,1,0), tl.cex=0.7)

#Tabla de correlaciones variable por variable
z<-df.merged2[,8:15] 
Impressions<-df.merged2[,20]
x<-cbddTOT <- read.csv(file = 'ddTOT.csv', header = TRUE, sep = ";")ind(z,Impressions)#selecciona las variables numericas
cor(x)
corrplot(cor(x),method='circle')