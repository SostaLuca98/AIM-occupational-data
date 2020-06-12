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

database <- PuliziaNA(database, c(2, 3, 4, 5, 8, 9, 13, 22, 23, 24, 25, 26) )
datireg <- database[c(2, 3, 4, 5, 8, 9, 13, 22, 23, 24, 25, 26)]
datireg$Stage <- factor(datireg$Stage)
datireg$Sesso <- factor(datireg$Sesso)
datireg$Major <- factor(datireg$Major)
datireg$Erasmus <- factor(datireg$Erasmus)
datireg$Voto_LT <- as.numeric(datireg$Voto_LT)
datireg$Voto_LM <- as.numeric(datireg$Voto_LM)
datireg$Durata <- as.numeric(datireg$Durata)
datireg$Retribuzione_PI <- as.numeric(datireg$Retribuzione_PI)

reg <- lm(datireg$Retribuzione_PI ~ ., data=datireg)
summary(reg)

slm1 <- step(reg, na.omit=TRUE, scope = . ~ .^2, nvmax = 4, trace = -1 )
summary(slm1)

slm2 <- step(reg, na.omit=TRUE, trace = -1 )
summary(slm2)
vif(slm1)

anova(slm1, slm2)

models <- regsubsets(datireg$Retribuzione_PI~., data = datireg, nvmax = 5,
                     method = "seqrep")
summary(models)

# Set seed for reproducibility
set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model <- train(datireg$Retribuzione_PI ~., data = datireg,
                    method = "leapBackward",
                    tuneGrid = data.frame(nvmax = c(1,2,3,4,5)),
                    trControl = train.control
                    )
step.model$results

t=lm(formula = datireg$Retribuzione_PI ~ exp(Voto_LT:Voto_LM)  + Durata +
    Stage + Erasmus, data = datireg)
summary(t)
shapiro.test(t$residuals)
