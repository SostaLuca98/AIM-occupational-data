library( rms )
library(arm)
library(ResourceSelection)
library(pROC)

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
dati[,11]<-Retribuzione
names(dati)[11]<-"Retribuzione"
rm(Retribuzione)

n=length(dati$Soddisfazione_Laurea)
id=which(dati$Soddisfazione_Laurea==5&dati$Soddisfazione_Preparazione==5|dati$Soddisfazione_Laurea==4&dati$Soddisfazione_Preparazione==5|dati$Soddisfazione_Laurea==5&dati$Soddisfazione_Preparazione==4)
sodmax<-rep(0,n)
sodmax[id]<-1

#Il modello che pensavamo di fare è Soddisfazione massima ~ phd+voto_LM

plot( dati$Voto_LM, sodmax, pch = ifelse( sodmax == 1, 3, 4 ),
      col = ifelse( sodmax == 1, 'forestgreen', 'red' ),
      xlab = 'Voto_LM', ylab = 'sodmax s_n', main = 'Soddisfazione massima vs. Voto_LM', lwd = 2, cex = 1.5 )
#Meglio


#Ma non saranno un po' troppo sbilanciate?
table(dati$Soddisfazione_Laurea,dati$Soddisfazione_Preparazione)
#in effetti sarebbe meglio aggiungere i 4+5, 5+4, ma lo facciamo in un secondo momento (tanto ci vuole un attimo)

ndata<-data.frame(dati,sodmax)
rm(sodmax)
mod = glm( sodmax ~ Phd+Voto_LM , data=ndata, family = binomial( link = logit ) )
summary(mod)
#direi bene, da qui ci farei previsione (proc ecc)


compl<-glm(sodmax~.-dati_definitivi_id-Soddisfazione_Lavorativa-Soddisfazione_Preparazione-Soddisfazione_Laurea,data=ndata, family = binomial( link = logit ) )
best<-step(compl)
summary(best)
(mod$aic-best$aic)/mod$aic
#la goodness of fit è molto simile
(mod$deviance-best$deviance)/mod$deviance
#la devianza tra i due è pari a 2, cioè un 1.7%.. non mi sembra così drammatico trascurare Durata
anova(mod,compl,test="Chisq")
#ipotesi nulla: i due modelli sono equivalenti. Qui pvalue altissimo: non ci sono problemi a dire che questo modello prende tutta l'info che si può tirare fuori da quei dati

# Z<-model.matrix(mod)
# beta<-mod$coefficients
# p_fit<-exp(Z%*%beta)/(1+exp(Z%*%beta))
p_fit<-mod$fitted.values

#qualche verifica di GOF
binnedplot(ndata$Voto_LM, rstandard(mod))
#ok in generale ci stanno dentro bene tranne che sotto il 100 dove immagino che i coef vadano negativi e quindi l'exp si stabilizzi (?) questo va indagato cmq

hoslem.test( mod$y, fitted( mod), g = 5 )
#misura la differenza tra la quantità predetta di valori 1 e la quantità osservata degli stessi. Bene se non rifiuto, ovvero se non posso dire che la statistica test (che sotto h0 è una chisq G-2 dof)è grande. Va preso g>p=r+1 e qui pval di 0.6 è ottimo

#Brier score
n=as.numeric(length(ndata$sodmax))
brier<-1/n*sum((ndata$sodmax-p_fit)^2)
brier
#sembra bene, considerato che sono 112 contributi

plot( ndata$Voto_LM, ndata$sodmax, pch = ifelse( ndata$sodmax == 1, 3, 4 ),
      col = ifelse( ndata$sodmax == 1, 'forestgreen', 'red' ),
      xlab = 'votolm', ylab = 'sodmax s-n', main = 'Grafico', lwd = 2, cex = 1.5 )
points( ndata$Voto_LM, mod$fitted, col = 'blue' , pch = 16)
points( ndata$Voto_LM[ndata$Phd=="Si"],mod$fitted[ndata$Phd=="Si"],col="black" , pch = 16)
#qui vediamo come l'aver fatto un PhD dà probabilità molto maggiore di avere soddisfazione completa

fit = mod$fitted
#media campionaria della prob di sopravvivenza nel campione
soglia_roc = seq( 0, 1, length.out = 2e2 )
lens = length( soglia_roc )-1
ascissa_roc = rep( NA, lens )
ordinata_roc = rep( NA, lens )
for ( k in 1 : lens )
{
  soglia = soglia_roc [ k ]
  classification = as.numeric( sapply( fit, function( x ) ifelse( x < soglia, 0, 1 ) ) )
  # ATTENZIONE, voglio sulle righe il vero e sulle colonne il predetto
  t.misc = table( ndata$sodmax, classification )
  if(!(sum(classification)==0|sum(classification)==length(classification))){
    ordinata_roc[ k ] = sum( classification[ which( ndata$sodmax == 1 ) ] == 1 )
    length( which( ndata$sodmax == 1 ) )
    ascissa_roc[ k ] = sum( classification[ which( ndata$sodmax == 0 ) ] == 1 ) /
      length( which( ndata$sodmax == 0 ) )
    ordinata_roc [ k ] = t.misc [ 1, 1 ] /( t.misc [ 1, 1 ] + t.misc [ 1, 2 ] )
    
    ascissa_roc [ k ] = t.misc [ 2, 1 ] /( t.misc [ 2, 1 ] + t.misc [ 2, 2 ] )
  }
  else{
    if((sum(classification)==0)){
      ordinata_roc[k]=0
      ascissa_roc[k]=0
    }
    else{
      ordinata_roc[k]=1
      ascissa_roc[k]=1
    }
  }
}

plot( ascissa_roc, ordinata_roc, type = "l", xlab = "1 - Specificity", ylab = "Sensitivity",
      main = "Curva ROC", lwd = 2, col = 'darkblue', ylim = c( 0, 1 ), xlim = c( 0, 1 ) )
abline( h = c( 0, 1 ), v = c( 0, 1 ), lwd = 1, lty = 2, col = 'red' )
abline( a = 0, b = 1, lty = 2, col = 'black' )
# We could have a problem here
#Secondo me il problema è che la probabilità non va mai sopra il 70% e poi il campione si ferma di punto in bianco a 111 quindi la curva roc viene troncata
#In ogni caso allora usiamo la funzione di r e ci facciamo fare la curva. Ecco cosa troviamo

soglia = 0.5
valori.predetti <-predict(mod,ndata,type="response")
roc_obj <- roc(ndata$sodmax, valori.predetti)
plot(roc_obj)

auc(roc_obj)
#auc=0.7104 (non granchè ) appena appena classificabile come fair :{

#intervalli di conf
(ci <- confint(mod))
#non sembrano molto eloquenti


#odds ratio per un incremento di uno del voto:
exp(coef(mod)[3])
#è >1 e questo ci fa piacere
#odds ratio per un phd vs non phd
exp(coef(mod)[2])

grid=(85:111)
pred<-predict(mod,data.frame(Voto_LM=grid,Phd="Si"),se=T)

gl = binomial( link = logit )
plot( grid, gl$linkinv( pred$fit ),type="l",col="darkblue" )
lines( grid, gl$linkinv( pred$fit - qnorm( 1-0.025 ) * pred$se ), col = "red", lty = 2 )
lines( grid, gl$linkinv( pred$fit + qnorm( 1-0.025 ) * pred$se ), col = "red", lty = 2 )

pred2<-predict(mod,data.frame(Voto_LM=grid,Phd="No"),se=T)
lines( grid, gl$linkinv( pred2$fit ),col="blue" )
lines( grid, gl$linkinv( pred2$fit - qnorm( 1-0.025 ) * pred2$se ), col = "green", lty = 2 )
lines( grid, gl$linkinv( pred2$fit + qnorm( 1-0.025 ) * pred2$se ), col = "green", lty = 2 )
#si vede che la probabilità di essere massimamente soddisfatti aumenta con chi ha fatto un dottorato
