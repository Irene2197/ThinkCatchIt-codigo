
##TEST AB CON T.TEST (IE vs CRM)
library(openintro)
library(tidyverse)
library(agricolae)

df.merged2$channelGrouping = ifelse(df.merged2$channelGrouping == "Email", "CRM", df.merged2$channelGrouping )
df.merged2$channelGrouping = ifelse(df.merged2$channelGrouping == "Branded Paid Search", "IE", df.merged2$channelGrouping)
df.merged2$channelGrouping = ifelse(df.merged2$channelGrouping == "Push", "CRM", df.merged2$channelGrouping)
df.merged2$channelGrouping = ifelse(df.merged2$channelGrouping == "Organic Search", "IE", df.merged2$channelGrouping)

#1.Cálculo del estadístico
CTR_CRM<-subset(df.merged2, channelGrouping == "CRM", select = c(date, channelGrouping, CTR))
as<-sqldf('SELECT date, channelGrouping, avg(CTR) as CTR FROM CTR_CRM GROUP BY date')
CTR_IE<-subset(df.merged2, channelGrouping == "IE", select = c(date, channelGrouping, CTR))
bs<-sqldf('SELECT date, channelGrouping, avg(CTR) as CTR FROM CTR_IE GROUP BY date')
abtest<-rbind(as,bs)


CRM    <- df.merged2 %>% filter(channelGrouping == "CRM") %>% pull(CTR)
IE <- df.merged2 %>% filter(channelGrouping == "IE") %>% pull(CTR) 
mean(CRM) - mean(IE)


#3.Condiciones para aplicar un t.test

##Normalidad
par(mar = c(2, 2, 2, 2))
par(mfrow = c(1, 2))
qqnorm(CRM, xlab = "", ylab = "",
       main = "CRM", col = "firebrick")
qqline(CRM)
qqnorm(IE, xlab = "", ylab = "",
       main = "IE", col = "springgreen4")
qqline(IE)


shapiro.test(as$CTR)
shapiro.test(bs$CTR)  


##Igualdad de varianza 
require(car)
fligner.test(CTR ~ channelGrouping, data = abtest)

leveneTest(CTR ~ channelGrouping, data = abtest, center = "median")


#4. Nivel de signigicaciÃ³n
alpha = 0.05

#5.Calculo p-valor
t.test(
  x           = CRM,
  y           = IE,
  alternative = "two.sided",
  mu          = 0,
  var.equal   = TRUE,
  conf.level  = 0.95
)

library(onewaytests)
welch.test(CTR ~ channelGrouping, abtest, rate = 0, alpha = 0.05, na.rm = TRUE, verbose = TRUE)

ab_experiment_results <- t.test(CTR_CRM ,CTR_IE, data = abtest)
ab_experiment_results



##TUKEY TODOS LOS CANALES
CTR_tukey<-subset(df.merged2, select = c(date, channelGrouping, CTR))
d<-sqldf('SELECT date, channelGrouping, sum(CTR) as CTR FROM CTR_tukey GROUP BY date, channelGrouping')

T_tukey<-subset(df.merged2, select = c(date, channelGrouping, Impressions))
d2<-sqldf('SELECT date, channelGrouping, sum(Impressions) as Impressions FROM T_tukey GROUP BY date, channelGrouping')

T2_tukey<-subset(df.merged2, select = c(date, channelGrouping, Link.Clicks))
d3<-sqldf('SELECT date, channelGrouping, sum("Link.Clicks") as Link FROM T2_tukey GROUP BY date, channelGrouping')


model <- aov(CTR ~ channelGrouping, data=d)
summary(model)

TukeyHSD(model, conf.level=.95) 

tukey.test2 <- HSD.test(model, trt = 'channelGrouping')
tukey.test2
