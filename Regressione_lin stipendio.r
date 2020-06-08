library(GGally)
library(rms)
library(arm)
library(ResourceSelection)
library(pROC)
library(RColorBrewer)
library(MASS)

#Pulisco la console, stampo directory, importo funzione di pulizia, importo dati, creo colori
rm(list=ls())
dev.off(dev.list()["RStudioGD"])
cat("\014")
getwd()
my_col = brewer.pal(9,'Set1')
source('PuliziaNA.R')
database <- read.table('statoccupazionali.txt',header=T)

#abbiamo visto che c'è differenza tra le medie di income per ragazze e ragazzi
tapply(na.omit(database$Retribuzione_PI),database$Sesso[which(!is.na(database$Retribuzione_PI))],mean)

#inoltre volevamo fare ua regressione per lo stipendio sulla base di voto, m/f, voto e m/f,luogo primo impiego, luogo m/f, tempo PI, major, major e m/f
#btw tolgo gli stagisti che potrebbero rovinare tutto: via quelli con stipe <1000
retnostage<-database$Retribuzione_PI
retnostage[which(database$Retribuzione_PI<1000)]<-NA

#definiamo le variabili miste

votoses<-database$Voto_LT*(1+as.numeric(database$Sesso==1))
majorses<-database$Major
for (i in 1:length(majorses)){
  if(database$Sesso[i]=="F") majorses[i]<-paste(database$Major[i], "isF", sep="", collapse = NULL)
  
}


dat1<-data.frame(retnostage,database$Sesso,database$Voto_LT,majorses,votoses, database$Luogo_PI,database$Tempo_PI, database$Major)

clndata<-PuliziaNA(dat1,1:length(dat1))

mod<-lm(retnostage~database.Sesso+database.Voto_LT+majorses+votoses+database.Luogo_PI+database.Tempo_PI+database.Major,data=clndata)
summary(mod)

#non sembra malvagio. Quei tre non definiti in base alle singolarità da capire

#vediamo un attimo le ipotesi
#plot vari dei residui

#residui e basta
plot(mod$residuals)
#sounds nice
plot(mod$fitted.values,mod$residuals)
#meno bello, i dati sembrano partire da poco variabili e andare a molto variabili 


shapiro.test(mod$res)
#col cavolo che sono normali
boxc<-boxcox(mod)
lambdaopt<-c(max(boxc$y),boxc$x[which(boxc$y==max(boxc$y))])
#esce che 1/stipendio^2/3 è normale... ma non vuol dire niente!!
#Tanto per vedere se ho speranze, vediamo così

mod1 = lm( (retnostage ^ lambdaopt[2] - 1)/lambdaopt[2] ~ database.Sesso+database.Voto_LT+majorses+votoses+database.Luogo_PI+database.Tempo_PI+database.Major,data=clndata)

shapiro.test(mod1$residuals)
#Otteniamo un 1% sudato sudato... niente sembrava bello ma mi sa che non funziona :((

#In realtà se osservo la likelihood della boxcox vedo che anche lambda=0 non è male, qualla eventualmente sarebbe interpretabile!!! Proviamo
mod2 = lm( log(retnostage) ~ database.Sesso+database.Voto_LT+majorses+votoses+database.Luogo_PI+database.Tempo_PI+database.Major,data=clndata)
shapiro.test(mod2$residuals)
#dai sempre 1%... lo teniamo?
#perfavore
#susususu
#beh io vedo di visualizzare un po' di cose
qqnorm(mod2$residuals)
qqline(mod2$residuals)
#aaah orribile

#residui e basta
plot(mod2$residuals)
#res standardizzati con la varianza campionaria
#plot(mod2$residuals/mod2$sigma)
#non me lo calcola per via degli NA probabilm
#sounds veeery nice
plot(mod2$fitted.values,mod$residuals)
#still ugly :(
#però però potrebbe esserci qualche leverage 
lev=hat(model.matrix(mod2))

p=mod2$rank
n=dim(clndata)[1]
watchout_points_lev = lev[ which( lev > 2 * p/n ) ]
watchout_ids_lev = seq_along( lev )[ which( lev > 2 * p/n ) ]
plot( mod2$fitted.values, lev, ylab = "Leverages", main = "Plot of Leverages",
      pch = 16, col = 'black' )
points( mod2$fitted.values[ watchout_ids_lev ], watchout_points_lev, col = 'red', pch = 16 )
#eh ci sono sei punti che ci rovinano la festa, next time vedremo se togliendoli cambia qualcosa

#no idea di che vuol dire se <0
AIC(mod2)
