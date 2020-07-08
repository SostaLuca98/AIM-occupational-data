# Title     : Regressione Multinomiale
# Objective : Trovare relazione tra indice soddisfazione e il resto
# Created by: david
# Created on: 02/07/2020

library(foreign)
library(ggplot2)
library(MASS)
library(Hmisc)
library(reshape2)

#Pulisco la console, stampo directory, importo funzione di pulizia, importo dati, creo colori
rm(list=ls())
getwd()
#my_col = brewer.pal(9,'Set1')
source('PuliziaNA.R')
database <- read.table('statoccupazionali.txt',header=T)

#Inserisco il database da pulire e le colonne dei dati da usare.
nomi=names(database)
dati1 <- PuliziaNA(database, c(2,4,5,8,9,23,24,26))
tipi=str(dati1)

maxsodd<-max(dati1$Soddisfazione_Laurea,dati1$Soddisfazione_Preparazione)
Sodd_compl<-5*(dati1$Soddisfazione_Laurea+dati1$Soddisfazione_Preparazione)/maxsodd
#Sodd_compl<-(dati1$Soddisfazione_Laurea+dati1$Soddisfazione_Preparazione)

Sodd <- Sodd_compl
Sodd[which(Sodd<=6)] <- 1
Sodd[which(Sodd>6 & Sodd<=8)] <- 2
Sodd[which(Sodd>8)] <- 3

Sodd[which(Sodd==1)] <- 'Poca Soddisfazione'
Sodd[which(Sodd==2)] <- 'Media Soddisfazione'
Sodd[which(Sodd==3)] <- 'Alta Soddisfazione'

length(Sodd[which(Sodd=='Poca Soddisfazione')])    #15 elementi, forse un po' pochini?
length(Sodd[which(Sodd=='Media Soddisfazione')])    #60 elementi
length(Sodd[which(Sodd=='Alta Soddisfazione')])    #66 elementi


dati1 <- cbind(dati1, Sodd)
dati1$Sodd <- factor(dati1$Sodd, levels = c("Poca Soddisfazione", "Media Soddisfazione", "Alta Soddisfazione"), ordered = TRUE)
#dati1$Sodd <- factor(dati1$Sodd)
dati1$Major <- factor(dati1$Major)
dati1$Sesso <- as.numeric(factor(dati1$Sesso))
dati1$Erasmus <- factor(dati1$Erasmus)
dati1$Phd <- factor(dati1$Phd)


#Dividing data into training and test set
#Random sampling
samplesize <- floor(1*nrow(dati1))
index <- sample(1:nrow(dati1), size = samplesize)

#Creating training and test set
datatrain <- dati1[index,]
datatest <- dati1[-index,]


#Osservazione dei dati
lapply(dati1[, c("Major", "Sesso", "Voto_LT", "Voto_LM", "Durata", "Sodd")], table)
ftable(xtabs(~ Major + Sesso + Sodd , data = dati1))
summary(dati1$Voto_LT)
summary(dati1$Voto_LM)
summary(dati1$Durata)

#Visualizzazione
ggplot(dati1, aes(x = Sodd, y = Voto_LM)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) +
  facet_grid(Major ~ Sesso, margins = TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

#Ordinal Logistic Regression
all <- polr(Sodd ~ Voto_LM + Major + Sesso + Durata + Erasmus + Stage + Phd + Python + Comunicazione, data = datatrain, Hess=TRUE)
modello <- summary(all)

#Aggiungo un p-value
(ctable <- coef(modello))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))

#Intervalli di confidenza
(ci <- confint(all))

#Valuto in termini di odds e non di log
exp(coef(all))
exp(cbind(OR = coef(all), ci))
n=length(datatrain$Sodd)

#Trovo il modello migliore con metodo Step
m2 <- stepAIC(all, ~.^2, na.omit=TRUE, k=3)

summary(m2)
m2$anova

#Aggiungo un p-value
(ctable <- coef(summary(m2)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))


#Compute confusion table and misclassification error
predizione <- predict(m2, datatrain)
confusionmatrix <- table(datatrain$Sodd, predizione)
confusionmatrix

  percgiusti <- (confusionmatrix[1, 1]+confusionmatrix[2, 2]
    +confusionmatrix[3, 3])/(sum(confusionmatrix[,]))
  percgiusti




sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)))
}

(s <- with(dati1, summary(as.numeric(Sodd) ~ Voto_LM + Phd + Python, fun=sf)))

newdat <- data.frame(
  #Sesso = rep(0:1, 300),
  Major = rep(c(1,2,3), each = 200),
  Python = factor(rep(c("Si","No"), 300)),
  Phd = factor(rep(c("No","Si", "Si", "No"), 150)),
  Voto_LM = rep(seq(from = 80, to = 111, length.out = 100), 6))

newdat <- cbind(newdat, predict(m2, newdat, type = "probs"))

##show first few rows
head(newdat)

lnewdat <- melt(newdat, id.vars = c( "Major", "Python", "Phd", "Voto_LM"),
  variable.name = "Level", value.name="Probability")
## view first few rows
head(lnewdat)

ggplot(lnewdat, aes(x = Voto_LM, y = Probability, colour = Level))+
  geom_line() + facet_grid(Python ~ Phd, labeller="label_both")
