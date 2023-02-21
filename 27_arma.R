# szereg<-c(0,1000,800,1100,1000,800,1000,1000,800,600,1000,800,1100,1500)
# szereg<-sample(500:700,30)

# install.packages("forecast")
# install.packages("tseries")
library(forecast)
library(tseries)
# ?forecast
# ?arma

# plot(szereg, type="l")
# modelArma<-arma(szereg, order = c(1, 1))
# szeregObliczonyModelem<-modelArma$fitted.values
# plot(szereg, type="l", col ="red")
# lines(szeregObliczonyModelem, col ="blue")
# modelArma$coef
# reszty<-modelArma$residuals
# (sredniBlad<-mean(reszty, na.rm=TRUE))
# (bladSrednioKwadratowy<-(sum(reszty^2, na.rm=TRUE)/length(reszty))^0.5)
# #mean(modelArma$fitted.values-szereg, na.rm=TRUE)
# 
# acf(szereg)
# pacf(szereg)
# ?acf

setwd("C://Users//rkiermasz//Documents//sgn kier/zadania2//76_bezrobotni//93_prognoza_covid")

# cesarRejWyrejStany2018<-read.csv("1_5_rej_wrej_stan.csv", encoding="UTF-8", header = TRUE, stringsAsFactors = FALSE)
cesarRejWyrejStany2016<-read.csv("1_5_rej_wrej_stan 08-05-2020.csv", encoding="UTF-8", header = TRUE, stringsAsFactors = FALSE)

cesarRejWyrejStany2016$Rok.miesiac<-paste0(cesarRejWyrejStany2016$Rok,"-",cesarRejWyrejStany2016$Numer.miesiąca.w.roku)
Rok.miesiac<-cesarRejWyrejStany2016$Rok.miesiac[1:60]
Stan.na.koniec.miesiaca<-cesarRejWyrejStany2016$Stan.na.koniec.miesiaca[1:51]#do marca 2020

plot(Stan.na.koniec.miesiaca, xaxt="n", las=2, xlab=NA, col="red",type="l",ylim=c(0,1700000), ylab=NA,xlim=c(0,64))
title("Stan osób bezrobotnych na koniec miesiąca")
at<-c(1,5,10,15,20,25,30,35,40,45,50,55,60)
axis(1, at=at, labels=Rok.miesiac[at], las=2)

#predykcja osób bezrobotnych w przypadku nie wystąpienia Koronawirusa

# model<-arima(Stan.na.koniec.miesiaca, order = c(1,1,1))
model<-arma(Stan.na.koniec.miesiaca, order = c(1,1))

(sredniBlad<-mean(reszty, na.rm=TRUE))#średnia błędu +-
(bladSrednioKwadratowy<-(sum(reszty^2, na.rm=TRUE)/length(reszty))^0.5)#średnie odchylenie łaczne RMSE
wspolczynniki<-model$coef
wspolczynniki[1]
reszty<-model$residuals
y51<-Stan.na.koniec.miesiaca[51]
y52<-as.numeric(wspolczynniki[1]*y51 + 0 + wspolczynniki[2]*reszty[51])
y53<-as.numeric(wspolczynniki[1]*y52 + 0 + wspolczynniki[2]*0)
y54<-as.numeric(wspolczynniki[1]*y53 + 0 + wspolczynniki[2]*0)

# szeregObliczonyModelem<-model$fitted.values
# lines(szeregObliczonyModelem, col ="blue")
prognozaArma<-ts(c(y52,y53,y54),start=52,end=54,frequency =1)
lines(prognozaArma, col ="green")

modelArima<-auto.arima(Stan.na.koniec.miesiaca)
prognoza<-forecast(modelArima, h=9)
lines(prognoza$mean, col ="blue")


#sarima
Stan.na.koniec.miesiaca2<-ts(cesarRejWyrejStany2016$Stan.na.koniec.miesiaca[1:51], start=1,end=51,frequency=1)#do marca 2020
plot(Stan.na.koniec.miesiaca2, xaxt="n", las=2, xlab=NA, col="black",type="l",ylim=c(0,1700000), ylab=NA,xlim=c(0,64))
title("Stan osób bezrobotnych na koniec miesiąca")
at<-c(1,6,12,18,24,30,36,42,48,51,54,60)
axis(1, at=at, labels=Rok.miesiac[at], las=2)
modelSarima<-arima(Stan.na.koniec.miesiaca2, order = c(1,1,1), seasonal=list(order=c(1,1,1),period=12))
prognoza2<-forecast(modelSarima, h=12)
lines(prognoza2$mean, col ="green")


acf(Stan.na.koniec.miesiaca2)
pacf(Stan.na.koniec.miesiaca2)
monthplot(Stan.na.koniec.miesiaca2)

Stan.na.koniec.miesiaca2Diff<-ts(diff(Stan.na.koniec.miesiaca2, lag=1), start=2,end=51,frequency=1)
plot(Stan.na.koniec.miesiaca2Diff)
acf(Stan.na.koniec.miesiaca2Diff)
pacf(Stan.na.koniec.miesiaca2Diff)

