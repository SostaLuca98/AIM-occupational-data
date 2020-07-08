# Title     : PhD vs Major, PhD vs Sesso, PhD vs Voto_LT
# Objective : Confrontare le variabili per trovare correlazioni
# Created by: Giulia
# Created on: 06/06/2020

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


names(database)
dati1 <- PuliziaNA(database, c(2,3,9,11))

## Confronto Sesso e PhD
cat <- table(dati1$Sesso, dati1$Phd)
cat

par( mfrow = c(1,2) )
barplot( cat[1,] , col = my_col[1], ylab='Numero Studenti',  names = c('No PhD', 'Si PhD'), main = 'Femmine')
barplot( cat[2,] , col = my_col[2], ylab='Numero Studenti',  names = c('No PhD', 'Si PhD'), main = 'Maschi')

## Confronto Major e PhD
cat <- table(dati1$Major, dati1$Phd)
cat

par( mfrow = c(1,3) )
barplot( cat[1,] , col = my_col[1], ylab='Numero Studenti',  names = c('No PhD', 'Si PhD'), main = 'Finanza')
barplot( cat[2,] , col = my_col[2], ylab='Numero Studenti',  names = c('No PhD', 'Si PhD'), main = 'Statistica')
barplot( cat[3,] , col = my_col[3], ylab='Numero Studenti',  names = c('No PhD', 'Si PhD'), main = 'Calcolo')

## Confronto Voto_LT e PhD

#Separo i dati in funzione della variabile categorica
VotoPhD <- dati1[which (dati1$Phd == "Si"), 3 ]
VotoNOPhD <- dati1[which (dati1$Phd == "No"), 3 ]

par( mfrow = c(1,2) )
qqnorm(VotoPhD, datax=T, main = 'PhD', xlab = "", ylab = "", col =my_col[4])
qqnorm(VotoNOPhD, datax=T, main = 'No PhD', xlab = "", ylab = "", col =my_col[5])

var.test(dati1$Voto_LT ~ dati1$Phd)

#Confrotiamo Graficamente i due set dei dati
par( mfrow = c(1,1) )
boxplot(dati1$Voto_LT ~ as.numeric(factor(dati1$Phd)), col = my_col, xlab= 'PhD', ylab='Voto_LT', names = c('No', 'Si'))

#Effettuiamo un test ANOVA
g <- aov(dati1$Voto_LT ~ as.numeric(factor(dati1$Phd))) 
summary(g)  
