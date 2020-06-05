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



##Vogliamo confrontare una variabile continua con una categorica
##Scegliamo Voto Triennale e Erasmus

#Inserisco il database da pulire e le colonne dei dati da usare.
names(database)
dati1 <- PuliziaNA(database, c(13, 23,24))
str(dati1)

#Separo i dati in funzione della variabile categorica
StageSPI <- dati1[which (dati1$Stage == 1), 3 ]
StageNPI <- dati1[which (dati1$Stage == 0), 3 ]
maxsodd<-max(dati1$Soddisfazione_Laurea,dati1$Soddisfazione_Preparazione)
Sodd_compl<-5*(dati1$Soddisfazione_Laurea+dati1$Soddisfazione_Preparazione)/maxsodd

#Categorizzo soddisfazioni
Soddisf<-ifelse(Sodd_compl>7.5,1,0)


#Casi specifici
#Soddisfazione vs resto del mondo
#1°stipendio e sodd

boxplot(dati1$Retribuzione_PI[which(dati1$Retribuzione_PI>1000)] ~ Soddisf[which(dati1$Retribuzione_PI>1000)], col = my_col, xlab= 'Soddisfazione', ylab='Stipendio1', names = c('No Sodd', 'Sodd'))

#Sodd e major
t1<-table(Soddisf,dati1$Major)
t1
par( mfrow = c(1,3) )
barplot( t1[1,] + t1[2,] , col = my_col[1], ylab='Numero Studenti',  names = c('Finanza', 'Statistica', 'Calcolo'), main = 'Totale')
barplot( t1[1,]           , col = my_col[2], ylab='Numero Studenti',  names = c('Finanza', 'Statistica', 'Calcolo'), main = 'Non sodd')
barplot( t1[2,]           , col = my_col[3], ylab='Numero Studenti',  names = c('Finanza', 'Statistica', 'Calcolo'), main = 'Sodd')

#Sodd e voto
boxplot(dati1$Voto_LT~ Soddisf, col = my_col, xlab= 'Soddisfazione', ylab='VotoTriennale', names = c('No Sodd', 'Sodd')) #onestamente non c'entrano molto, ha variabilità altissima

#Sodd e stato dove lavora
t2<-table(Soddisf,dati1$Luogo_PI)
t2

dati3 <- PuliziaNA(database, c(11, 23,24))
StageSPI <- dati3[which (dati3$Stage == 1), 3 ]
StageNPI <- dati3[which (dati3$Stage == 0), 3 ]
maxsodd<-max(dati3$Soddisfazione_Laurea,dati3$Soddisfazione_Preparazione)
Sodd_compl<-5*(dati3$Soddisfazione_Laurea+dati3$Soddisfazione_Preparazione)/maxsodd
Soddisf<-ifelse(Sodd_compl>7.5,1,0)
#Sodd e phd
t3<-table(Soddisf,dati3$Phd)
t3
