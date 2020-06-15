# Title     : Major_Settore, Stipendio_Settore, Major_Paese
# Objective : Confrontare le variabili per trovare correlazioni
# Created by: Giulia
# Created on: 04/06/2020

library(GGally)
library(rms)
library(arm)
library(ResourceSelection)
library(pROC)
library(RColorBrewer)

#Pulisco la console, stampo directory, importo funzione di pulizia, importo dati, creo colori
rm(list=ls())
dev.off(dev.list()["RStudioGD"])
cat("\014")
getwd()
my_col = brewer.pal(9,'Set1')
source('PuliziaNA.R')
database <- read.table('statoccupazionali.txt',header=T)
#costruisco un indice di preparazione

names(database)
dati1 <- PuliziaNA(database, c(9,10,12))
dati2 <- PuliziaNA(database, c(9,11))

#Major_Settore
## Confronto tra due variabili categoriche
# Costruisco e Stampo la tabella dei valori sommati
cat <- table( dati1$Major, dati1$Settore_PI)
cat
cat2 <-table( dati2$Major, dati2$Phd)
cat2

n1=cat2[1,2]
n2=cat2[2,2]
n3=cat2[3,2]

# Grafici degli istogrammi
par( mfrow = c(1,3) )
barplot( cat[1,]+c(n1,0,0,0,0,0,0), col = my_col[1], ylab='Numero Studenti', main = 'Finanza')
barplot( cat[2,]+c(n2,0,0,0,0,0,0), col = my_col[2], ylab='Numero Studenti', main = 'Statistica')
barplot( cat[3,]+c(n3,0,0,0,0,0,0), col = my_col[3], ylab='Numero Studenti', main = 'Calcolo')

#Major_Paese
cat <- table( dati1$Major, dati1$Luogo_PI)
cat
par( mfrow = c(1,3) )
barplot( cat[1,], col = my_col[1], ylab='Numero Studenti', main = 'Finanza')
barplot( cat[2,], col = my_col[2], ylab='Numero Studenti', main = 'Statistica')
barplot( cat[3,], col = my_col[3], ylab='Numero Studenti', main = 'Calcolo')

#Major_RetribuzionePI

names(database)
dati3 <- PuliziaNA(database, c(9,10,12,13))
dati4 <- PuliziaNA(database, c(9,11,15,16))

#Separo i dati in funzione della variabile categorica

StipendioNoPhD <- dati3[which (dati3$Retribuzione_PI >= 1000 & dati3$Retribuzione_PI<=3400), 13 ]
StipendioPhD <- dati4[which (dati4$Retribuzione_Phd<=3400), 16]

par( mfrow = c(1,1) )
qqnorm(StipendioNoPhD, datax=T, main = 'NoPhD', xlab = "", ylab = "", col =my_col[4])
qqnorm(StipendioPhD, datax=T, main = 'PhD', xlab = "", ylab = "", col =my_col[5])
qqplot(StipendioNoPhD,StipendioPhD)
abline(0,1)

par(mfrow=c(2,1))

#Make the plot
par(mar=c(0,5,3,3))
hist(StipendioNoPhD , main="Comparazione primi stipendi" , xlim=c(1000,3200), ylab="No PhD", xlab="", ylim=c(0,20) , xaxt="n", las=1 , col="slateblue1", breaks=24)
par(mar=c(5,5,0,3))
hist(StipendioPhD , main="" , xlim=c(1000,3200), ylab="PhD", xlab="", ylim=c(10,0) , las=1 , col="tomato3", breaks=24)