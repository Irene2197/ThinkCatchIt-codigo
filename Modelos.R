memory.limit(size=100000)
library(data.table) 

##MODELO 1: REGRESIÓN LINEAL
setwd("C:/Users/daniel/Desktop/Irene") 

dd <- read.csv(file = 'FINAL.csv', header = TRUE, sep = ";")

#Transformacion variables
out_final$CTR.rescaled<-as.numeric(gsub(",",".",out_final$CTR.rescaled))
out_final$pageviews.rescaled<-as.numeric(gsub(",",".",out_final$pageviews.rescaled))
out_final$avgTimeOnPage.rescaled<-as.numeric(gsub(",",".",out_final$avgTimeOnPage.rescaled))
out_final$engagement_score<-as.numeric(gsub(",",".",out_final$engagement_score))

dd_final$date<- lubridate::ymd(dd_final$date)
dd_final$sessionsPerUser<-as.numeric(gsub(",",".",dd_final$sessionsPerUser))
dd_final$pageviewsPerSession<-as.numeric(gsub(",",".",dd_final$pageviewsPerSession))
dd_final$avgTimeOnPage<-as.numeric(gsub(",",".",dd_final$avgTimeOnPage))
dd_final$CTR<-as.numeric(gsub(",",".",dd_final$CTR))
dd_final$Amount.Spent..EUR.<-as.numeric(gsub(",",".",dd_final$Amount.Spent..EUR.))

dd<-dd[,2:26]
dd$date<- lubridate::ymd(dd$date)
dd$sessionsPerUser<-as.numeric(gsub(",",".",dd$sessionsPerUser))
dd$pageviewsPerSession<-as.numeric(gsub(",",".",dd$pageviewsPerSession))
dd$avgTimeOnPage<-as.numeric(gsub(",",".",dd$avgTimeOnPage))
dd$CTR<-as.numeric(gsub(",",".",dd$CTR))
dd$Amount.Spent..EUR.<-as.numeric(gsub(",",".",dd$Amount.Spent..EUR.))
dd$engagement_score<-as.numeric(gsub(",",".",dd$engagement_score))


#Librerias
library(dplyr)
library(caret)

#Base de datos con la que implementar el modelo
dd_final<-dd_final[,4:22]
out_final<-out_final[,2:5]
dd<-cbind(dd_final,out_final)

#Crear variable numerica para cond (1=no covid, 2=covid)
dd <- dd %>% mutate(cond_num = case_when(cond == "NO COVID"~ 1, cond == "COVID"~ 2))
write.csv2(dd, "FINAL.csv")

# Dividir los datos en los conjuntos de entrenamiento y Prueba (70,30)
split_dummy <- sample(c(rep(0, 0.7 * nrow(dd)), rep(1, 0.3 * nrow(dd))))
table(split_dummy) 
train <- dd[split_dummy == 0, ]
test <- dd[split_dummy == 1, ]

# Realizar un ajuste mediante regresion lineal 
fit=lm(engagement_score~segment+channelGrouping+userType+sessions+transactions+users+newUsers+sessionsPerUser+
                        pageviewsPerSession+Impressions+cond+camp_num+Amount.Spent..EUR.,data=train)

fit2=lm(engagement_score~segment+channelGrouping+userType+cond+camp_num+Amount.Spent..EUR.,data=train)

# Mostrar detalles o resumen
summary(fit2)
y_predict <- predict(fit, train)
y_test_predict <- predict(fit, test)
cor(train$engagement_score, y_predict)
cor(test$engagement_score, y_test_predict) #0.69442

library(Metrics)
mae(test$engagement_score, y_test_predict)
mae(train$engagement_score, y_predict)


#MODELO 2:RANDOM FOREST
library(ranger)

set.seed(1234)
split_dummy <- sample(c(rep(0, 0.7 * nrow(dd)), rep(1, 0.3 * nrow(dd))))
table(split_dummy) 
train <- dd[split_dummy == 0, ]
test <- dd[split_dummy == 1, ]

modelo<-ranger(
        formula=engagement_score~transactions,
        data=train,
        num.trees=10,
        seed=1234
)
print(modelo)


predicciones <- predict(
  modelo,
  data = test
)

predicciones <- predicciones$predictions
test_rmse    <- sqrt(mean((predicciones - test$engagement_score)^2))
paste("Error de test (rmse) del modelo:", round(test_rmse,2))


# Valores evaluados
num_trees_range <- seq(1, 400, 20)

# Bucle para entrenar un modelo con cada valor de num_trees y extraer su error
# de entrenamiento y de Out-of-Bag.

train_errors <- rep(NA, times = length(num_trees_range))
oob_errors   <- rep(NA, times = length(num_trees_range))

for (i in seq_along(num_trees_range)){
  modelo  <- ranger(
    formula   = engagement_score ~ transactions,
    data      = train,
    num.trees = num_trees_range[i],
    oob.error = TRUE,
    seed      = 1234
  )
  
  predicciones_train <- predict(
    modelo,
    data = train
  )
  predicciones_train <- predicciones_train$predictions
  
  train_error <- mean((predicciones_train - train$engagement_score)^2)
  oob_error   <- modelo$prediction.error
  
  train_errors[i] <- sqrt(train_error)
  oob_errors[i]   <- sqrt(oob_error)
  
}

# Gráfico con la evolución de los errores
df_resulados <- data.frame(n_arboles = num_trees_range, train_errors, oob_errors)
ggplot(data = df_resulados) +
  geom_line(aes(x = num_trees_range, y = train_errors, color = "train rmse")) + 
  geom_line(aes(x = num_trees_range, y = oob_errors, color = "oob rmse")) +
  geom_vline(xintercept = num_trees_range[which.min(oob_errors)],
             color = "firebrick",
             linetype = "dashed") +
  labs(
    title = "Evolución del out-of-bag-error vs número árboles",
    x     = "número de árboles",
    y     = "out-of-bag-error (rmse)",
    color = ""
  ) +
  theme_bw() +
  theme(legend.position = "bottom")
