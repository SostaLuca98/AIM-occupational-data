# Title     : Erasmus
# Objective : Confrontare le variabili per trovare correlazioni
# Created by: davide (e un pochetto pure Luca)
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



##Vogliamo confrontare una variabile continua con una categorica
##Scegliamo Voto Triennale e Erasmus

#Inserisco il database da pulire e le colonne dei dati da usare.
names(database)
dati1 <- PuliziaNA(database, c(3, 26))

#Separo i dati in funzione della variabile categorica
VotoSE <- dati1[which (dati1$Erasmus == 1), 3 ]
VotoNE <- dati1[which (dati1$Erasmus == 0), 3 ]

#Controlo la normalità dei dati graficamente
par( mfrow = c(1,2) )
qqnorm(VotoSE, datax=T, main = 'Sì Erasmus', xlab = "", ylab = "", col =my_col[4])
qqnorm(VotoNE, datax=T, main = 'No Erasmus', xlab = "", ylab = "", col =my_col[5])

#effettuo il test di shapiro sui dati separati
shapiro.test(VotoSE)               
shapiro.test(VotoNE)  

#Confrontiamo le varianze dei due gruppi, devono essere simili
var.test(dati1$Voto_LT ~ dati1$Erasmus)

#Controlliamo che le due abbiano distribuzioni confrontabili
par( mfrow = c(1,1) )
qqplot(VotoSE, VotoNE, xlab = "Sì Erasmus", ylab = "No Erasmus", col = my_col[7])
abline(0,1)

#Confrotiamo Graficamente i due set dei dati
par( mfrow = c(1,1) )
boxplot(dati1$Voto_LT ~ as.numeric(factor(dati1$Erasmus)), col = my_col, xlab= 'Erasmus', ylab='Voto Triennale', names = c('No Erasmus', 'Sì Erasmus'))

#Effettuiamo u test ANOVA
g <- aov(dati1$Voto_LT ~ as.numeric(factor(dati1$Erasmus))) 
summary(g)                                                  



## Confronto tra due variabili categoriche
## Scegliamo Erasmus e Major

# Inserisco il database da pulire e le colonne dei dati da usare.
names(database)                          
dati2 <- PuliziaNA(database, c(9, 26))   

# Costruisco e Stampo la tabella dei valori sommati
cat <- table(dati2$Erasmus, dati2$Major)
cat

# Grafici degli istogrammi
par( mfrow = c(1,3) )
barplot( cat[1,] + cat[2,] , col = my_col[1], ylab='Numero Studenti',  names = c('Finanza', 'Statistica', 'Calcolo'), main = 'Totale')
barplot( cat[1,]           , col = my_col[2], ylab='Numero Studenti',  names = c('Finanza', 'Statistica', 'Calcolo'), main = 'No Erasmus')
barplot( cat[2,]           , col = my_col[3], ylab='Numero Studenti',  names = c('Finanza', 'Statistica', 'Calcolo'), main = 'Sì Erasmus')


