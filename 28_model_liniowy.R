t<-1:51
t2<-(1:51)^2
t3<-(1:51)^3
modelLiniowy<-lm(formula = Stan.na.koniec.miesiaca ~ t + t2 +t3)
t<-1:63
y<-modelLiniowy$coe[1]+t*modelLiniowy$coe[2]+t^2*modelLiniowy$coe[3]+t^3*modelLiniowy$coe[4]
lines(y, col ="blue")