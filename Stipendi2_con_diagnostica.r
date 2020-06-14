# Title     : Cose con gli stipendi
# Objective : TODO
# Created by: david
# Created on: 11/06/19

library(tidyverse)
library(caret)
library(leaps)
library(car)
library(GGally)
#library(MASS)

#Pulisco la console, stampo directory, importo funzione di pulizia, importo dati
rm(list=ls())
currwd <- getwd()
setwd(currwd)
source('PuliziaNA.R')
source('SottoDF.R')
source('Funzioni Train.R')
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
reg <- lm(varlog ~ .-Retribuzione_PI-dati_definitivi_id, data=datireg)
summary(reg)

#Visualizzo relazioni
pairs(datireg [, c('Voto_LT', 'Voto_LM', 'Durata', 'Fine_Studi', 'Retribuzione_PI', 'Soddisfazione_Lavorativa',
                   'Soddisfazione_Laurea', 'Soddisfazione_Preparazione')])
relazioni <- ggpairs(data = datireg[2:13], title ="Relationships between predictors & response", lower = list(continuous=wrap("points", alpha = 0.5, size=0.1)))
vcov(reg)


slm1 <- step(reg, na.omit=TRUE, scope = . ~ .^2, nvmax = 4, trace = -1 )
summary(slm1)
shapiro.test(slm1$residuals)
vif(slm1)


slm2 <- step(reg, na.omit=TRUE, trace = -1 )
summary(slm2)
shapiro.test(slm2$residuals)
vif(slm2)                               #Le covarianze tra i vari predittori sono sotto controllo
vcov(slm2)

#Diagnsotics on this model
plot(slm2$fitted.values,hat(model.matrix(slm2)))
hmeans <- 2*summary(slm2)$df[1]/(summary(slm2)$df[2]+summary(slm2)$df[1])
abline(h=hmeans)
lev <- hat(model.matrix(slm2))
bad_leverages <- which(hat(model.matrix(slm2))>hmeans)
datireg$dati_definitivi_id[bad_leverages] #to be controlled
points( slm2$fitted.values[bad_leverages], hat(model.matrix(slm2))[bad_leverages], col = 'red', pch = 16 )

#Single out high studentized residuals
stud <- rstandard(slm2 )
watchout_ids_stud <- which(abs(stud ) > 2 )
watchout_stud <- stud[watchout_ids_stud ]

#We can check with boxplot and Cook's distance
Cdist <- cooks.distance(slm2)
Cdist
p <- summary(slm2)$df[1]-1
n <- as.numeric(summary(slm2)$df[2]+summary(slm2)$df[1])
watchout_ids_Cdist <- which(Cdist > 4/(n-p) )
watchout_Cdist <- Cdist[watchout_ids_Cdist ]
watchout_Cdist

par( mfrow = c( 1, 3 ) )
plot( slm2$fitted.values, Cdist, pch = 16, xlab = 'Fitted values',
      ylab = 'Cooks Distance', main = 'Cooks Distance' )
points( slm2$fitted.values[ watchout_ids_Cdist ], Cdist[ watchout_ids_Cdist ],
        col = 'green', pch = 16 )
plot( slm2$fitted.values, stud, pch = 16, xlab = 'Fitted values',
      ylab = 'Studentized Residuals', main = 'Studentized Residuals' )
points( slm2$fitted.values[ watchout_ids_stud ], stud[ watchout_ids_stud ],
        col = 'pink', pch = 16 )
plot( slm2$fitted.values, lev, pch = 16, xlab = 'Fitted values',
      ylab = 'Leverages', main = 'Leverages' )
points( slm2$fitted.values[ bad_leverages ], lev[ bad_leverages ],
        col = 'orange', pch = 16 )


#remove suspect data in Cook's distance
id_to_keep <- !( 1:n %in% watchout_ids_Cdist )
slm3<- lm(formula = varlog[id_to_keep] ~ Voto_LM[id_to_keep] + Durata[id_to_keep] + Stage[id_to_keep] + Erasmus[id_to_keep], data = datireg)
summary(slm3)
#pvalue has decreased (we took away data), but Rsq has improved and the model still seems significative

#Verifico comportamento dei residui
par( mfrow = c( 1, 1 ) )
plot(slm3$fitted.values, slm3$residuals, xlab='Fitted Values',
     ylab='Residuals', main='Residuals Vs Fitted Values', pch=16)
abline( h = 0, lwd = 2, lty = 2, col = 'red' )
qqnorm(slm3$residuals, ylab='Residui')
qqline(slm3$residuals)
shapiro.test(slm3$residuals)

qqnorm( rstandard( slm3 ), ylab = "Studentized residuals", pch = 16 )
abline( 0, 1 )
hist( slm3$residuals, 10, probability = TRUE, col = 'lavender', main = 'residuals' )
boxplot( slm3$residuals, main = "Boxplot of model residuals", pch = 16, col = 'lavender' )

heatmap( cor( datireg[c(3,4,5,6)] ), Rowv = NA, Colv = NA, symm = TRUE, keep.dendro = F)

##Sezione dedicata al train and test
datireg <- datireg[id_to_keep,]
varlog  <- varlog[id_to_keep]
slm4 <- regsubsets(varlog ~.-Retribuzione_PI-dati_definitivi_id,data=datireg, nvmax=5,force.out=1,intercept=TRUE)
res.sum <- summary(slm4)

#Posso scegliere il mio dello preferito in base all'indice che mi piace di più
data.frame(
  Adj.R2 = which.max(res.sum$adjr2),
  CP = which.min(res.sum$cp),
  BIC = which.min(res.sum$bic)
)

get_model_formula(4, slm4, "Retribuzione_PI")

# Compute cross-validation error
model.ids <- 1:5
cv.errors <-  map(model.ids, get_model_formula, slm4, "Retribuzione_PI") %>%
  map(get_cv_error, data = datireg, numfold=5) %>%
  unlist()
cv.errors  #Il modello che ridue l'errore cv è quello con 4 variabili
which.min(cv.errors)
coef(slm4,4) #I coefficienti sono molto simili a slm3, mi pare una cosa buona
coef(slm4,5) #Aumentando il numero di fold l'errore minore si ha per 4 o 5 predittori, ma se 5 avrei entrammbe le lauree
