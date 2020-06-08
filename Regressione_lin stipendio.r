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
