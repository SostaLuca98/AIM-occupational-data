library(tidyverse)
library(caret)
library(leaps)
library(car)
library(DMwR)
library(GGally)
library(MASS)
#library( ellipse )
library( faraway )
#library(mgcv)
library(nnet)
library(mlogit)

#Pulizia

rm(list=ls())
currwd <- getwd()
setwd(currwd)
source('PuliziaNA.R')
source('SottoDF.R')
database <- read.table('statoccupazionali.txt',header=T)
names(database)

database <- PuliziaNA(database, c(1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 22, 23, 24, 25, 26) )
view(database)
datireg <- database[c(1, 2, 3, 4, 5,7, 8, 9, 10, 11, 13,16, 22, 23, 24, 25, 26)]

view(datireg)

id_pi=which(!is.na(datireg$Retribuzione_PI))
id_phd=which(!is.na(datireg$Retribuzione_Phd))
cisono<-sort(c(id_pi,id_phd), decreasing=FALSE)
# max(a)
# non_cisono<-which(!(1:123 %in% a) )
Retr<-c(datireg$Retribuzione_PI[id_pi],datireg$Retribuzione_Phd[id_phd])
Retribuzione<-rep(NA,length(datireg$Retribuzione_PI))
Retribuzione[id_pi]<-datireg$Retribuzione_PI[id_pi]
Retribuzione[id_phd]<-datireg$Retribuzione_Phd[id_phd]
Retribuzione<-as.numeric(na.omit(Retribuzione))
                      

dati<-datireg[cisono,-12]
view(dati)
dati[,11]<-Retribuzione
view(dati)
names(dati)[11]<-"Retribuzione"
rm(Retribuzione)

#finalmente abbiamo il nostro bel database

dati$Soddisfazione_Laurea
dati$Soddisfazione_Lavorativa
dati$Soddisfazione_Preparazione

#quante e quali classi definire: tieni conto che 5+5 ne hai 30, 5+4 ne hai 22, 5+3 ne hai 3, 5+2 ne hai 1... insomma sono casi che andrebbero separati ma onestamente sono troppo pochi per complicarsi la vita

which(dati$Soddisfazione_Laurea==5&dati$Soddisfazione_Preparazione==5)
length(which(dati$Soddisfazione_Laurea==5&dati$Soddisfazione_Preparazione==5))
n=length(dati$Soddisfazione_Laurea)

table(dati$Soddisfazione_Laurea,dati$Soddisfazione_Preparazione)
min(dati$Soddisfazione_Laurea)
#quindi nessuno ha dato meno di 3 al corso di ing mat, ma alcuni pensano che non sia tanto utile

#soddisfazione massima (sodd9): 5 + 5; soddisfazione medio-alta (sodd7): 4+5,4+4, 5+3; medio bassa (sodd5): 4+3 e sotto

id9=which(dati$Soddisfazione_Laurea==5&dati$Soddisfazione_Preparazione==5)
id7=which(dati$Soddisfazione_Laurea==5&dati$Soddisfazione_Preparazione==4|dati$Soddisfazione_Laurea==4&dati$Soddisfazione_Preparazione==5|dati$Soddisfazione_Laurea==4&dati$Soddisfazione_Preparazione==4|dati$Soddisfazione_Laurea==5&dati$Soddisfazione_Preparazione==3)
id5=which(!(1:n %in% id9)&!(1:n %in% id7))

sodcompl<-rep(0,n)
sodcompl[id9]<-9
sodcompl[id7]<-7
sodcompl[id5]<-5
dati[,17]<-sodcompl
names(dati)[17]<-"Soddisfazione_Aggregata"

#vorrei apire che fare dell'indice sodd lavorativa: è correlato con le altre due sodd? E quelle sono cor tra loro?
corlaureaprep<-cor.test(dati$Soddisfazione_Laurea,dati$Soddisfazione_Preparazione,method="spearman")
corlaureaprep
#sono un bel po' correlate
corpreplavoro<-cor.test(dati$Soddisfazione_Lavorativa,dati$Soddisfazione_Preparazione,method="spearman")
corpreplavoro
#mediamente correlate anche loro
corlaurealavoro<-cor.test(dati$Soddisfazione_Lavorativa,dati$Soddisfazione_Laurea,method="spearman")
corlaurealavoro
#idem

coraggrlavoro<-cor.test(dati$Soddisfazione_Lavorativa,dati$Soddisfazione_Aggregata,method="spearman")
coraggrlavoro
#non ci sono cambiamenti sostanziali, quindi il modo di considerare le sodd aggregate non rovina la correlazione che c'è con le variabili originarie

#Quindi faccio bene a guardarne solo 2 e non spiegarle con l'altra, forse nell'aggregata potrei anche metterci la terza soddisfazione dentro

#prova<-gam(Soddisfazione_Aggregata ~ Voto_LM+Retribuzione, family=multinom(K=2),data=dati )
prova<- multinom(Soddisfazione_Aggregata ~ Voto_LM+Retribuzione, data=dati)
summary(prova)
prova2<-multinom(Soddisfazione_Aggregata ~.-Soddisfazione_Laurea-Soddisfazione_Preparazione, data=dati)
summary(prova2)


step(prova2)
#ci dà che le migliori sono effettivamente il voto LM e la soddisfazione lavorativa. Essendo questa però successiva io la escluderei

prova3<-multinom(Soddisfazione_Aggregata ~.-Soddisfazione_Laurea-Soddisfazione_Preparazione-Soddisfazione_Lavorativa, data=dati)
summary(prova3)


step(prova3)
#Già meglio, ci dice che dipende da Voto_LM, durata e Phd (solitamente i Phd sono i più affezionati)

mnmod1<-multinom(Soddisfazione_Aggregata ~ Voto_LM+ Durata + 
                   Phd, data = dati)
-(prova3$deviance-mnmod1$deviance)
#la devianza è peggiorata (ma è logico: ho tolto predittori quindi meno dof!!)
mnmod1$AIC-prova3$AIC
#invece l'AIC è molto migliorato (mi sa più per la diminnuzione delle covariate che per aumento della likelihood però)

summary(mnmod1)

#Top adesso andrebbe interpretato e ci dovremmo fare un po' di diagnostica

#Wald test per capire se è safe dire che i coef sono diversi da zero
#(e la normalità non andrebbe in qualche modo giustificata?)
z <- summary(mnmod1)$coefficients/summary(mnmod1)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

#Sono abbastanza altini...

#Provo ad aggiungere variabile dipendente da prodotto di ho fatto un phd con retribuzione


prova4<-multinom(Soddisfazione_Aggregata ~ .-Soddisfazione_Laurea-Soddisfazione_Preparazione-Soddisfazione_Lavorativa
                   +as.numeric(Phd=="Si")*Retribuzione+ as.numeric(Phd=="No")*Retribuzione, data=dati)
summary(prova4)
mnmod2<-step(prova4)
z2<-summary(mnmod2)$coefficients/summary(mnmod2)$standard.errors
p2 <- (1 - pnorm(abs(z2), 0, 1)) * 2
p2
#onestamente era meglio prima

#per la diagnostica provo a usare un altra libreria, mlogit
mdata<-mlogit.data(dati, varying = NULL, choice = "Soddisfazione_Aggregata", shape="wide")
head(mdata)
ristretto<-data.frame(dati[c(2,4,5,6,8,10,11,17)])
mdataristretto<-mlogit.data(ristretto, varying = NULL, choice = "Soddisfazione_Aggregata", shape="wide")

model<-mlogit(Soddisfazione_Aggregata~0|Durata+Voto_LM+Phd, mdataristretto)

#confronto quest'ultimo con quello trovato da multinom

summary(model)
summary(mnmod1)

#Ok cool questo ha i test e la visualizzazione carina + log-lik e mcFadden R^2 (qui pessimo) ma niente devianza e AIC, quindi no step. coef un filo diversi

#aggiungere a quelle 3 variabili singolarmente major oppure retribuzione (oppure tempo_pi, settore, sesso, erasmus, stage) non migliora quasi per nulla


#Separare stipendi di phd e non non cambia nulla
# dd<-data.frame(dati,a,b)
#  View(dd)
# > ddm<-mlogit.data(dd, varying=NULL, choice = "Soddisfazione_Aggregata", shape="wide")
# > names(dd)
# [1] "dati_definitivi_id"         "Sesso"                      "Voto_LT"                    "Voto_LM"                   
# [5] "Durata"                     "Tempo_PI"                   "Fine_Studi"                 "Major"                     
# [9] "Settore_PI"                 "Phd"                        "Retribuzione"               "Soddisfazione_Lavorativa"  
# [13] "Soddisfazione_Laurea"       "Soddisfazione_Preparazione" "Stage"                      "Erasmus"                   
# [17] "Soddisfazione_Aggregata"    "a"                          "b"                         
# > mm1<-mlogit(Soddisfazione_Aggregata~0|Durata+Voto_LM+Phd+a+b, ddm)
# > summary(mm1)

#Frequencies of alternatives:choice
# 5       7       9 
# 0.31250 0.41964 0.26786 
# 
# nr method
# 6 iterations, 0h:0m:0s 
# g'(-H)^-1g = 4.39E-06 
# successive function values within tolerance limits 
# 
# Coefficients :
# Estimate  Std. Error z-value Pr(>|z|)   
# (Intercept):7  7.0082e+00  5.5209e+00  1.2694 0.204299   
# (Intercept):9 -2.2318e+01  1.0147e+01 -2.1994 0.027846 * 
# Durata:7      -6.0111e-01  3.1179e-01 -1.9279 0.053863 . 
# Durata:9      -5.4042e-01  3.8992e-01 -1.3860 0.165754   
# Voto_LM:7     -2.8724e-02  4.6030e-02 -0.6240 0.532604   
# Voto_LM:9      2.3549e-01  8.9981e-02  2.6172 0.008866 **
# PhdSi:7       -1.0784e+00  1.9687e+00 -0.5478 0.583839   
# PhdSi:9       -5.2115e-01  2.0266e+00 -0.2572 0.797061   
# a:7           -2.2768e-04  4.2799e-04 -0.5320 0.594745   
# a:9           -3.3043e-04  4.8248e-04 -0.6849 0.493429   
# b:7            8.2134e-04  1.1276e-03  0.7284 0.466360   
# b:9            7.8085e-04  1.1393e-03  0.6854 0.493109   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Log-Likelihood: -106.02
# McFadden R^2:  0.1241 
# Likelihood ratio test : chisq = 30.043 (p.value = 0.00084275)
# > 

#Ho provato anche le seguenti: 1. Usare il settore come finanza vs resto del mondo, ma niente di che
#2. Finanza o consulenza vs resto ma non cambia molto (anzi non migliora quasi per nniente, meno che con solo finanza)
#togliere durata. Questo ovviamente a scapito dell'R^2 e della likelihood, però migliora significatività dei coefficienti relativi alla soddisfazione massima. Insomma lascia pensare che tra quelli max soddisfatti e quelli insoddisfatti ci sia un discriminante in base a voto (tanto) e phd (un po' meno, occhio però che dobbiamo controllare la correlazione voto e phd), mentre tra i mediamente soddisfatti e gli altri non c'è un andamento netto. Questo ci dice che dobbiamo ripensare le categorie!

#controllo correlazione tra voto lm e phd

corphdvoto<-cor.test(dati$Voto_LM,as.numeric(dati$Phd=="Si"), method = "spearman")
corphdvoto
#abbastanza poco cor e questo è un bene

corphdmajor<-cor.test(dati$Major,as.numeric(dati$Phd=="Si"), method = "kendall")
corphdmajor
#Questi pare di si che lo siano (anche se con le riserve dovute alla rappresentazione numerica del major)

