setwd("C://Users//rkiermasz//Documents//zadania//17_szydlowiec//r")
dane<-read.csv2("Pe³naAnaliza.csv")



rownames(dane)<-dane[,1]
dane<-dane[,-1]

colnames(dane)<-c(
"stopa bezrobocia (paŸdziernik 2017)",
"liczba uczniów w szko³ach ponadgimnazjalnych % z ludnoœci",
"liczba mieszkañców na szko³ê",
"dochody gmin na jednego mieszkañca",
"liczba mieszkañców na jeden supermarket",
"stopieñ wykorzystania miejsc noclegowych",
"liczba mieszkañców na zatrudniaj¹ca organizacjê",
"bezrobotny(profil I) % ludnoœci",
"bezrobotny(profil II)% ludnoœci",
"bezrobotny(profil III)% ludnoœci",
"poszukuj¹cy pracy% ludnoœci",
"mieszkañców na ofertê pracy",
"pracownicy w œrednich przedsiêbiorstwach % ludnoœci",
"liczba  mieszkañców przypadaj¹cych na du¿e przedsiêbiorstwo")

#colnames(dane)
#rownames(dane)
#dane<-dane[,-5]#usunieto supermarkety
#dane<-dane[,-3]#radom
#dane<-dane[,c(-2,-5,-13)]




srednie<-apply(dane,2,mean)
odchylenia<-apply(dane,2,sd)
for(i in 1:dim(dane)[1]){
dane[i,]<- (dane[i,]-srednie)/odchylenia
}

library(psych)

fit2 <- fa(dane,2, rotate = "varimax",correct=.7)
fit2$loadings
load <- fit2$loadings[,1:2]
plot(load,type="n") # set up plot
text(load,labels=names(dane),cex=0.7) # add variable names


fit2$loadings
write.csv2(fit2$loadings,"ladunkiCzynnikoweWskazniki.csv")
?fa

colnames(dane)

write.csv2(round(cor(dane),2),"korelacjeWskazniki.csv")

d <- dist(dane,method = "euclidean")^2
fit <- hclust(d,method = "ward")
plot(fit, labels = rownames(dane))


wojewodztwa<-dane
wss <- (nrow(wojewodztwa)-1)*sum(apply(wojewodztwa,2,var))
for (i in 2:6) wss[i] <- sum(kmeans(wojewodztwa, centers = i)$withinss)
plot(1:6, wss, type = "b", xlab = "liczba grup", ylab = "Suma kwadr wewn
grup")


