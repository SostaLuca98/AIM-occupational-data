# Title     : Correlazioni
# Objective : Valuto matrici di correlazione
# Created by: davide
# Created on: 06/06/2020

library(GGally)
library(rms)
library(arm)
library(ResourceSelection)
library(pROC)

#Pulisco la console, stampo directory, importo funzione di pulizia, importo dati
rm(list=ls())
currwd <- getwd()
setwd(currwd)
source('PuliziaNA.R')
source('SottoDF.R')
database <- read.table('statoccupazionali.txt',header=T)
names(database)

datipuliti <- PuliziaNA(database, c(7, 9, 22, 23, 24))
datiridotti <- SottoDF(datipuliti, c(7, 22, 23, 24))
for (i in 1:dim(datiridotti)[1]) {
  j=1
  if (datiridotti[[j]][[i]]=='Prelaurea') {datiridotti[[j]][[i]] <- 1}
  if (datiridotti[[j]][[i]]=='Immediato') {datiridotti[[j]][[i]] <- 2}
  if (datiridotti[[j]][[i]]=='Veloce') {datiridotti[[j]][[i]] <- 3}
  if (datiridotti[[j]][[i]]=='Medio') {datiridotti[[j]][[i]] <- 4}
  if (datiridotti[[j]][[i]]=='Lungo') {datiridotti[[j]][[i]] <- 5}
}
covarianzagen=matrix(cor((datiridotti)), nrow=4)

m=database$Sesso=='M'
datimaschi=datiridotti[m,]
datifemmine=datiridotti[!m,]
#covfemmine=matrix(cor(datifemmine),nrow=4)
#covmaschi=matrix(cor(datimaschi),nrow=4)

f=datipuliti$Major==1
s=datipuliti$Major==2
c=datipuliti$Major==3
datifin=datiridotti[f,]
datistat=datiridotti[s,]
daticalc=datiridotti[c,]
covfin=matrix(cor(datifin),nrow=4)
covstat=matrix(cor(datistat),nrow=4)
covcalc=matrix(cor(daticalc),nrow=4)

nfs=norm(covfin-covstat, "F")
nfc=norm(covfin-covcalc, "F")
nsc=norm(covstat-covcalc, "F")
