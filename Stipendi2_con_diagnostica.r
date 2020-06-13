# Title     : Cose con gli stipendi
# Objective : TODO
# Created by: david
# Created on: 11/06/19

library(tidyverse)
library(caret)
library(leaps)
#library(MASS)

#Pulisco la console, stampo directory, importo funzione di pulizia, importo dati
rm(list=ls())
currwd <- getwd()
setwd(currwd)
source('PuliziaNA.R')
source('SottoDF.R')
database <- read.table('statoccupazionali.txt',header=T)
names(database)

database <- PuliziaNA(database, c(1, 2, 3, 4, 5, 8, 9, 13, 22, 23, 24, 25, 26) )
datireg <- database[c(1, 2, 3, 4, 5, 8, 9, 13, 22, 23, 24, 25, 26)]
datireg$Stage <- factor(datireg$Stage)
datireg$Sesso <- factor(datireg$Sesso)
datireg$Major <- factor(datireg$Major)
datireg$Erasmus <- factor(datireg$Erasmus)
datireg$Voto_LT <- as.numeric(datireg$Voto_LT)
datireg$Voto_LM <- as.numeric(datireg$Voto_LM)
datireg$Durata <- as.numeric(datireg$Durata)
datireg$Retribuzione_PI <- as.numeric(datireg$Retribuzione_PI)

varlin <- datireg$Retribuzione_PI
varlog <- log(datireg$Retribuzione_PI)
reg <- lm(varlog ~ .-Retribuzione_PI, data=datireg)
summary(reg)

slm1 <- step(reg, na.omit=TRUE, scope = . ~ .^2, nvmax = 4, trace = -1 )
summary(slm1)
shapiro.test(slm1$residuals)

slm2 <- step(reg, na.omit=TRUE, trace = -1 )
summary(slm2)
shapiro.test(slm2$residuals)
vif(slm1)

#Diagnsotics on this model
plot(slm2$fitted.values,hat(model.matrix(slm2)))
hmeans=2*summary(slm2)$df[1]/(summary(slm2)$df[2]+summary(slm2)$df[1])
abline(h=hmeans)
lev=hat(model.matrix(slm2))
bad_leverages=which(hat(model.matrix(slm2))>hmeans)
datireg$dati_definitivi_id[bad_leverages] #to be controlled

#We can check with boxplot and Cook's distance
Cdist=cooks.distance(slm2)
Cdist
p=summary(slm2)$df[1]-1
n=as.numeric(summary(slm2)$df[2]+summary(slm2)$df[1])
watchout_ids_Cdist = which( Cdist > 4/(n-p) )
watchout_Cdist = Cdist[ watchout_ids_Cdist ]
watchout_Cdist

par( mfrow = c( 1, 3 ) )
plot( slm2$fitted.values, Cdist, pch = 16, xlab = 'Fitted values',
      ylab = 'Cooks Distance', main = 'Cooks Distance' )
points( slm2$fitted.values[ watchout_ids_Cdist ], Cdist[ watchout_ids_Cdist ],
        col = 'green', pch = 16 )
plot( slm2$fitted.values, slm2$residuals/(summary(slm2)$sigma*lev), pch = 16, xlab = 'Fitted values',
      ylab = 'Studentized Residuals', main = 'Studentized Residuals' )
points( slm2$fitted.values[ watchout_ids_stud ], stud[ watchout_ids_stud ],
        col = 'pink', pch = 16 )
plot( slm2$fitted.values, lev, pch = 16, xlab = 'Fitted values',
      ylab = 'Leverages', main = 'Leverages' )
points( slm2$fitted.values[ bad_leverages ], lev[ bad_leverages ],
        col = 'orange', pch = 16 )

#remove suspect data in Cook's distance
id_to_keep = !( 1:n %in% watchout_ids_Cdist )
slm3<- lm(formula = varlog[id_to_keep] ~ Voto_LM[id_to_keep] + Durata[id_to_keep] + Stage[id_to_keep] + Erasmus[id_to_keep], data = datireg)
summary(slm3)
#pvalue has decreased (we took away data), but Rsq has improved and the model still seems significative


