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

#Facciamo qualche ragionamento temporale
names(database)
dat1<- PuliziaNA(database, c(8,28))
ref<-dat1$Fine_Studi

summary(ref)


groupyrs = unique( as.character( ref ) )
groupyrs=as.numeric(groupyrs)
groupyrs<-sort(groupyrs)
groupyrs<-as.character(groupyrs)
numref<-tapply(ref,ref,length)[groupyrs]
numref

comsi<-as.numeric(dat1$Comunicazione=="Si")

numcomsi<- tapply(comsi,ref,sum)
perccomsi=numcomsi/numref

perccomsi
#non sono particolarmente correlate

pysi<-as.numeric(dat1$Python=="Si")
numpysi<- tapply(pysi,ref,sum)
percpysi=numpysi/numref
#scopriamo che nessuno da prima del 2011 ha sentito la mancanza di python


boxplot(dat1$Voto_LT ~ ref, xlab= 'anno', ylab='Voto Triennale')
boxplot(dat1$Voto_LM ~ ref, xlab= 'anno', ylab='Voto Magistrale')
dat1$Voto_LM[which(ref==2011)]
tapply(dat1$Voto_LM,ref,length)

tb<-table(ref,dat1$Settore_PI)
tb

#però funziona poco perchè nonostante la media sia 2015 indietro ne abbiamo più sparsi

dat2<-PuliziaNA(database,c(3,8))
ref<-dat2$Fine_Studi

Old<-which(ref<2015)
New<-which(ref>=2015)
length(Old)
length(New)
new<-as.numeric(ref<2015)
old<-as.numeric(ref>=2015)

mean(dat2$Voto_LT[New])
mean(dat2$Voto_LT[Old])

table(new, dat2$Tempo_PI)

#nice, proviamo con la magis
dat3<-PuliziaNA(database,c(4,8))
ref<-dat3$Fine_Studi

Old<-which(ref<2015)
New<-which(ref>=2015)
length(Old)
length(New)
new<-as.numeric(ref<2015)
old<-as.numeric(ref>=2015)

mean(dat3$Voto_LM[New])
mean(dat3$Voto_LM[Old])

table(new, dat3$Tempo_PI)

ita<-as.numeric(dat3$Luogo_PI=="Italia")
table(new,ita)

Ita<-as.numeric(dat3$Luogo_IA=="Italia")
table(new,Ita)

table(dat3$Luogo_Phd,new)

#cerco le persone con stipendio alto, prima e dopo, stabilendo cosa è alto dall'iqr*1.5 + Q3

summary(dat3$Retribuzione_PI)

stipendioalto<-as.numeric(dat3$Retribuzione_PI>2200)
people_sa<-sum(na.omit(stipendioalto))
people_sa
tot_peop<-length(na.omit(stipendioalto))
tot_peop

table(new,stipendioalto)

summary(dat3$Retribuzione_IA)

Stipendioalto<-as.numeric(dat3$Retribuzione_IA>4000+ (4000-1900)*1.5)
People_sa<-sum(na.omit(Stipendioalto))
People_sa
Tot_peop<-length(na.omit(Stipendioalto))
Tot_peop

#mi sa che non ha senso perchè l'iqr pesa allo stesso modo i numeri

#vediamo che fa quando cerco di confrontare quanti old e quanti new


goodin<- as.numeric(dat3$Retribuzione_IA>4215)
table(goodin,new)

goodin<- as.numeric(dat3$Retribuzione_PI>1570)
table(goodin,new)

#phd si negli anni
table(new,dat3$Phd)

#stage negli anni
table(dat3$Stage,new)

#major negli anni
table(dat3$Major,new)

#erasmus negli anni
table(dat3$Erasmus,new)

#sesso
table(dat3$Sesso,new)


#riassunto delle analisi:
# 1. abbiamo provato ad analizzare per anno: purtroppo prima del 2012 poche osservazioni anno x anno, quindi non importa troppo se trovo differenze
# 2. Raggruppando tra pre 2015, post meglio:
# - la media di triennale si è abbassata
# - la media di magistrale invariata
# - diminuiti phd europa di più poli e mondo
# - molta più gente ha un ottimo income (più della media) ora che prima
# - un tempo meno phd
# - meno stage ora
# - meno gente a stat
# - meno erasmus oggi
# - meno boys, stesse girls