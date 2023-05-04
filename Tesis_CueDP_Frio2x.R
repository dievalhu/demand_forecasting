library(readr)
library(stats)
library(tidyverse)
library(tidyr)
library(Metrics)
library(ggplot2)
library(dplyr)
library(smooth)
library(quantmod)
library(forecast)
library(fpp2)
library(TTR)
library(reshape2)
library(caTools)
library(randomForest)
library(caret)
library(MASS)
library(tsfgrnn)

Data1 <- library(readr)
#Data1 <- read_delim("DataF.csv", ";", escape_double = FALSE,trim_ws = TRUE)
Data1 <- read_delim("C:/Users/Ma Fernanda/Dropbox/Tesis Master - Mafer y Liss/Programa/DataF.csv",delim = ";", escape_double = FALSE, trim_ws = TRUE)
#Data1 <- read_delim("DataF.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

data_A = filter(Data1, Line == "Cold")
data_A = as.data.frame(data_A)
summary(data_A)
Sales = data_A$Quantity
Weeks = c(1:length(Sales)) #vector de semanas
df1 = as.data.frame(cbind(Weeks,Sales))

#Division de Datos
# Random Cross Validation
set.seed(2)
split = sample.split(df1$Weeks, SplitRatio = .7)
summary(split)

# Training
train = subset(df1, split == TRUE)
# Test
test = subset(df1, split == FALSE)

#Grafico Serie de Tiempo - Train
df2 = train[,1:2]
cols <- c("69E8C8", "#FF6600", "#00A600", "#FF0000")
pl <- ggplot(df2 , aes(x = Weeks, color = Variable))
pl <- pl + geom_line(aes(y = Sales, color = "Sales"), group = 1)
pl <- pl + theme(panel.background = element_rect(fill = 'white'),
                 panel.border = element_rect(color = "black", fill=NA, size = 0.3),
                 legend.position = c(0.07, 0.9))
pl <- pl + scale_color_manual(values = cols)
pl

#Grafico Serie de Tiempo - Test
df3 = test[,1:2]
cols <- c("69E8C8", "#FF6600", "#00A600", "#FF0000")
pl <- ggplot(df3 , aes(x = Weeks, color = Variable))
pl <- pl + geom_line(aes(y = Sales, color = "Sales"), group = 1)
pl <- pl + theme(panel.background = element_rect(fill = 'white'),
                 panel.border = element_rect(color = "black", fill=NA, size = 0.3),
                 legend.position = c(0.07, 0.9))
pl <- pl + scale_color_manual(values = cols)
pl

##########################################
#### Primer Metodo: Promedios Móviles ####

##########################################
####             Train                ####
##########################################

Sales = df2$Sales
Weeks = df2$Weeks

# Funciones para calculo del Rendimiento
y = Sales
nc = length(y) # Posibles tamanos de ventanas
RMSE0 = c()
for (i in 2:(nc-1)) 
{
  m = SMA(y, i)
  pr_next0 = m[length(m)] #pronostico a t+1
  df2$SMA = c(NA,head(m,-1))
  m1 <- head(m[!is.na(m)],-1)
  RMSE0[i] = rmse(y[i+1:(length(y)-i)],m1)
}
med_RMSE0 = min(RMSE0[2:length(RMSE0)])
RMSE0_NA <- RMSE0[2:length(RMSE0)]
n = which(abs(RMSE0_NA - med_RMSE0) == min(abs(RMSE0_NA - med_RMSE0))) #Tamano de la ventana con error promedio

m = SMA(y, n)
pr_next0 = m[length(m)] #pronostico a t+1
df2$SMA = c(NA,head(m,-1))
m1 <- head(m[!is.na(m)],-1)
MSE0 = mse(y[n+1:(length(y)-n)],m1)
MAE0 = mae(y[n+1:(length(y)-n)],m1)
RMSE0 = rmse(y[n+1:(length(y)-n)],m1)
MASE0 = mase(y[n+1:(length(y)-n)],m1)

cols <- c("69E8C8", "#FF6600", "#00A600", "#FF0000")
pl <- ggplot(df2 , aes(x = Weeks, color = Variable))
pl <- pl + geom_line(aes(y = Sales, color = "1 Sales"), group = 1)
pl <- pl + geom_line(aes(y = SMA, color = "2 SMA"),group = 1)
pl <- pl + theme(panel.background = element_rect(fill = 'white'),
                 panel.border = element_rect(color = "black", fill=NA, size = 0.3),
                 legend.position = c(0.07, 0.85))
pl <- pl + scale_color_manual(values = cols)
pl

##########################################
####             Test                 ####
##########################################

Salest = df3$Sales
Weekst = df3$Weeks
yt = Salest

mt = SMA(yt, n)
pr_next0t = mt[length(mt)] #pronostico a t+1
df3$SMA = c(NA,head(mt,-1))
m1t <- head(mt[!is.na(mt)],-1)
MSE0t = mse(yt[n+1:(length(yt)-n)],m1t)
MAE0t = mae(yt[n+1:(length(yt)-n)],m1t)
RMSE0t = rmse(yt[n+1:(length(yt)-n)],m1t)
MASE0t = mase(yt[n+1:(length(yt)-n)],m1t)

cols <- c("69E8C8", "#FF6600", "#00A600", "#FF0000")
pl <- ggplot(df3 , aes(x = Weeks, color = Variable))
pl <- pl + geom_line(aes(y = Sales, color = "1 Sales"), group = 1)
pl <- pl + geom_line(aes(y = SMA, color = "2 SMA"),group = 1)
pl <- pl + theme(panel.background = element_rect(fill = 'white'),
                 panel.border = element_rect(color = "black", fill=NA, size = 0.3),
                 legend.position = c(0.07, 0.85))
pl <- pl + scale_color_manual(values = cols)
pl

###################################################
#### Segundo Metodo: Suavizamiento Exponencial ####

##########################################
####             Train                ####
##########################################

a = c()
RMSE1v = c()
for (i in 1:9)
{
  a[i] = i/10
  m2 = HoltWinters(y, a[i], beta=FALSE, gamma=FALSE) #suavizamiento exponencial
  pr_next1 = m2$coefficients[1] #pronostico a t+1
  m3 = m2$fitted[,1] 
  df2$SES = c(NA,m3)
  RMSE1v[i] = rmse(y[2:(length(y))],m3)
}

med_RMSE1 = min(RMSE1v)
alpha = (which(abs(RMSE1v - med_RMSE1) == min(abs(RMSE1v - med_RMSE1))))/10 #valor de la constante de suavizamiento promedio
m2 = HoltWinters(y, alpha, beta=FALSE, gamma=FALSE) #suavizamiento exponencial
pr_next1 = m2$coefficients[1] #pronostico a t+1
m3 = m2$fitted[,1] 
df2$SES = c(NA,m3)
MSE1 = mse(y[2:(length(y))],m3)
MAE1 = mae(y[2:(length(y))],m3)
RMSE1 = rmse(y[2:(length(y))],m3)
MASE1 = mase(y[2:(length(y))],m3)

cols <- c("69E8C8", "#FF6600", "#00A600", "#FF0000")
pl <- ggplot(df2 , aes(x = Weeks, color = Variable))
pl <- pl + geom_line(aes(y = Sales, color = "1 Sales"), group = 1)
pl <- pl + geom_line(aes(y = SMA, color = "2 SMA"),group = 1)
pl <- pl + geom_line(aes(y = SES, color = "3 SES"),group = 1)
pl <- pl + theme(panel.background = element_rect(fill = 'white'),
                 panel.border = element_rect(color = "black", fill=NA, size = 0.3),
                 legend.position = c(0.07, 0.83))
pl <- pl + scale_color_manual(values = cols)
pl

##########################################
####             Test                 ####
##########################################

m2t = HoltWinters(yt, alpha, beta=FALSE, gamma=FALSE) #suavizamiento exponencial
pr_next1t = m2t$coefficients[1] #pronostico a t+1
m3t = m2t$fitted[,1] 
df3$SES = c(NA,m3t)
MSE1t = mse(yt[2:(length(yt))],m3t)
MAE1t = mae(yt[2:(length(yt))],m3t)
RMSE1t = rmse(yt[2:(length(yt))],m3t)
MASE1t = mase(yt[2:(length(yt))],m3t)

cols <- c("69E8C8", "#FF6600", "#00A600", "#FF0000")
pl <- ggplot(df3 , aes(x = Weeks, color = Variable))
pl <- pl + geom_line(aes(y = Sales, color = "1 Sales"), group = 1)
#pl <- pl + geom_line(aes(y = SMA, color = "2 SMA"),group = 1)
pl <- pl + geom_line(aes(y = SES, color = "3 SES"),group = 1)
pl <- pl + theme(panel.background = element_rect(fill = 'white'),
                 panel.border = element_rect(color = "black", fill=NA, size = 0.3),
                 legend.position = c(0.07, 0.83))
pl <- pl + scale_color_manual(values = cols)
pl

##############################
#### Tercer Metodo: ARIMA ####

##########################################
####             Train                ####
##########################################

m4 = auto.arima(y,ic= "aic",trace = TRUE)
m5 = forecast(m4,1,level=99)
pr_next2 = m5$mean[1]
m6 = m4$fitted
df2$ARM = c(m6)
MSE2 = mse(m4$x,m4$fitted)
MAE2 = mae(m4$x,m4$fitted)
RMSE2 = rmse(m4$x,m4$fitted)
MASE2 = mase(m4$x,m4$fitted)

cols <- c("69E8C8", "#FF6600", "#00A600", "#FF0000")
pl <- ggplot(df2 , aes(x = Weeks, color = Variable))
pl <- pl + geom_line(aes(y = Sales, color = "1 Sales"), group = 1)
pl <- pl + geom_line(aes(y = SMA, color = "2 SMA"),group = 1)
pl <- pl + geom_line(aes(y = SES, color = "3 SES"),group = 1)
pl <- pl + geom_line(aes(y = ARM, color = "4 ARM"),group = 1)
pl <- pl + theme(panel.background = element_rect(fill = 'white'),
                 panel.border = element_rect(color = "black", fill=NA, size = 0.3),
                 legend.position = c(0.07, 0.8))
pl <- pl + scale_color_manual(values = cols)
pl

##########################################
####             Test                 ####
##########################################

m4t = arima(yt, order = c(1, 0, 4))
m5t = forecast(m4t,1,level=99)
pr_next2t = m5t$mean[1]
m6t = m5t$fitted
df3$ARM = c(m6t)
MSE2t = mse(m5t$x,m5t$fitted)
MAE2t = mae(m5t$x,m5t$fitted)
RMSE2t = rmse(m5t$x,m5t$fitted)
MASE2t = mase(m5t$x,m5t$fitted)

cols <- c("69E8C8", "#FF6600", "#00A600", "#FF0000")
pl <- ggplot(df3 , aes(x = Weeks, color = Variable))
pl <- pl + geom_line(aes(y = Sales, color = "1 Sales"), group = 1)
#pl <- pl + geom_line(aes(y = SMA, color = "2 SMA"),group = 1)
pl <- pl + geom_line(aes(y = SES, color = "3 SES"),group = 1)
pl <- pl + geom_line(aes(y = ARM, color = "4 ARM"),group = 1)
pl <- pl + theme(panel.background = element_rect(fill = 'white'),
                 panel.border = element_rect(color = "black", fill=NA, size = 0.3),
                 legend.position = c(0.07, 0.8))
pl <- pl + scale_color_manual(values = cols)
pl

######################################
#### Cuarto Metodo: Random Forest ####

##########################################
####             Train                ####
##########################################

set.seed(2)
m7 = randomForest(formula = Sales ~ Weeks, 
                  data = df2,
                  ntree = 500,
                  nodesize = 5,
                  importance = TRUE)
m8 = predict(m7, newdata=df2$Weeks)
df2$RF = c(m8)
pr_next3 = predict(m7, newdata=data.frame(Weeks=dim(df2)[1]+1))
MSE3 = mse(df2$Sales,df2$RF)
MAE3 = mae(df2$Sales,df2$RF)
RMSE3 = rmse(df2$Sales,df2$RF)
MASE3 = mase(df2$Sales,df2$RF)

cols <- c("69E8C8", "#FFCC00", "#0000FF", "#FF00CC")
pl <- ggplot(df2 , aes(x = Weeks, color = Variable))
pl <- pl + geom_line(aes(y = Sales, color = "1 Sales"), group = 1)
pl <- pl + geom_line(aes(y = RF, color = "5 RF"),group = 1)
pl <- pl + theme(panel.background = element_rect(fill = 'white'),
                 panel.border = element_rect(color = "black", fill=NA, size = 0.3),
                 legend.position = c(0.07, 0.85))
pl <- pl + scale_color_manual(values = cols)
pl

##########################################
####             Test                 ####
##########################################

set.seed(2)
m7t = randomForest(formula = Salest ~ Weekst, 
                  data = df3,
                  ntree = 500,
                  nodesize = 5,
                  importance = TRUE)
m8t = predict(m7t, newdata=df3$Weeks)
df3$RF = c(m8t)
pr_next3t = predict(m7t, newdata=data.frame(Weekst=dim(df3)[1]+1))
MSE3t = mse(df3$Sales,df3$RF)
MAE3t = mae(df3$Sales,df3$RF)
RMSE3t = rmse(df3$Sales,df3$RF)
MASE3t = mase(df3$Sales,df3$RF)

cols <- c("69E8C8", "#FFCC00", "#0000FF", "#FF00CC")
pl <- ggplot(df3 , aes(x = Weeks, color = Variable))
pl <- pl + geom_line(aes(y = Sales, color = "1 Sales"), group = 1)
pl <- pl + geom_line(aes(y = RF, color = "5 RF"),group = 1)
pl <- pl + theme(panel.background = element_rect(fill = 'white'),
                 panel.border = element_rect(color = "black", fill=NA, size = 0.3),
                 legend.position = c(0.07, 0.85))
pl <- pl + scale_color_manual(values = cols)
pl

############################
#### Quinto Metodo: KNN ####

##########################################
####             Train                ####
##########################################

m9 = knnreg(Sales~Weeks, data = df2)
m10 = predict(m9, newdata = as.data.frame(df2$Weeks))
df2$KNN = c(m10)
pr_next4 = predict(m9, newdata=data.frame(Weeks=dim(df2)[1]+1))
MSE4 = mse(df2$Sales,df2$KNN)
MAE4 = mae(df2$Sales,df2$KNN)
RMSE4 = rmse(df2$Sales,df2$KNN)
MASE4 = mase(df2$Sales,df2$KNN)

cols <- c("69E8C8", "#FFCC00", "#0000FF", "#FF00CC")
pl <- ggplot(df2 , aes(x = Weeks, color = Variable))
pl <- pl + geom_line(aes(y = Sales, color = "1 Sales"), group = 1)
pl <- pl + geom_line(aes(y = RF, color = "5 RF"),group = 1)
pl <- pl + geom_line(aes(y = KNN, color = "6 KNN"),group = 1)
pl <- pl + theme(panel.background = element_rect(fill = 'white'),
                 panel.border = element_rect(color = "black", fill=NA, size = 0.3),
                 legend.position = c(0.07, 0.83))
pl <- pl + scale_color_manual(values = cols)
pl

##########################################
####             Test                 ####
##########################################

m9t = knnreg(Salest~Weekst, data = df3, k = 5)
m10t = predict(m9t, newdata = as.data.frame(df3$Weeks))
df3$KNN = c(m10t)
pr_next4t = predict(m9t, newdata=data.frame(Weekst=dim(df3)[1]+1))
MSE4t = mse(df3$Sales,df3$KNN)
MAE4t = mae(df3$Sales,df3$KNN)
RMSE4t = rmse(df3$Sales,df3$KNN)
MASE4t = mase(df3$Sales,df3$KNN)

cols <- c("69E8C8", "#FFCC00", "#0000FF", "#FF00CC")
pl <- ggplot(df3 , aes(x = Weeks, color = Variable))
pl <- pl + geom_line(aes(y = Sales, color = "1 Sales"), group = 1)
pl <- pl + geom_line(aes(y = RF, color = "5 RF"),group = 1)
pl <- pl + geom_line(aes(y = KNN, color = "6 KNN"),group = 1)
pl <- pl + theme(panel.background = element_rect(fill = 'white'),
                 panel.border = element_rect(color = "black", fill=NA, size = 0.3),
                 legend.position = c(0.07, 0.83))
pl <- pl + scale_color_manual(values = cols)
pl

############################
#### Sexto Metodo: ANN ####

##########################################
####             Train                ####
##########################################

set.seed(2)
m11 = nnetar(Sales, lambda="auto")
m12 = forecast(m11,1,level=99)
pr_next5 = m12$mean[1]
df2$ANN = c(m11$fitted)
MSE5 = mse(df2$Sales[(m11$p+1):length(Sales)],df2$ANN[(m11$p+1):length(Sales)])
MAE5 = mae(df2$Sales[(m11$p+1):length(Sales)],df2$ANN[(m11$p+1):length(Sales)])
RMSE5 = rmse(df2$Sales[(m11$p+1):length(Sales)],df2$ANN[(m11$p+1):length(Sales)])
MASE5 = mase(df2$Sales[(m11$p+1):length(Sales)],df2$ANN[(m11$p+1):length(Sales)])

cols <- c("69E8C8", "#FFCC00", "#0000FF", "#FF00CC")
pl <- ggplot(df2 , aes(x = Weeks, color = Variable))
pl <- pl + geom_line(aes(y = Sales, color = "1 Sales"), group = 1)
pl <- pl + geom_line(aes(y = RF, color = "5 RF"),group = 1)
pl <- pl + geom_line(aes(y = KNN, color = "6 KNN"),group = 1)
pl <- pl + geom_line(aes(y = ANN, color = "7 ANN"),group = 1)
pl <- pl + theme(panel.background = element_rect(fill = 'white'),
                 panel.border = element_rect(color = "black", fill=NA, size = 0.3),
                 legend.position = c(0.07, 0.8))
pl <- pl + scale_color_manual(values = cols)
pl

##########################################
####              Test                ####
##########################################

set.seed(2)
m11t = nnetar(Salest, p = 3, lambda="auto")
m12t = forecast(m11t,1,level=99)
pr_next5t = m12t$mean[1]
df3$ANN = c(m11t$fitted)
MSE5t = mse(df3$Sales[(m11t$p+1):length(Sales)],df3$ANN[(m11t$p+1):length(Sales)])
MAE5t = mae(df3$Sales[(m11t$p+1):length(Sales)],df3$ANN[(m11t$p+1):length(Sales)])
RMSE5t = rmse(df3$Sales[(m11$p+1):length(Sales)],df3$ANN[(m11t$p+1):length(Sales)])
MASE5t = mase(df3$Sales[(m11$p+1):length(Sales)],df3$ANN[(m11t$p+1):length(Sales)])

cols <- c("69E8C8", "#FFCC00", "#0000FF", "#FF00CC")
pl <- ggplot(df3 , aes(x = Weeks, color = Variable))
pl <- pl + geom_line(aes(y = Sales, color = "1 Sales"), group = 1)
pl <- pl + geom_line(aes(y = RF, color = "5 RF"),group = 1)
pl <- pl + geom_line(aes(y = KNN, color = "6 KNN"),group = 1)
pl <- pl + geom_line(aes(y = ANN, color = "7 ANN"),group = 1)
pl <- pl + theme(panel.background = element_rect(fill = 'white'),
                 panel.border = element_rect(color = "black", fill=NA, size = 0.3),
                 legend.position = c(0.07, 0.8))
pl <- pl + scale_color_manual(values = cols)
pl