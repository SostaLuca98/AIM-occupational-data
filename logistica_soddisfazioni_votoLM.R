library(GGally)
library(rms)
library(arm)
library(ResourceSelection)
library(pROC)
library(RColorBrewer)
library(ROCit)

#Pulisco la console, stampo directory, importo funzione di pulizia, importo dati, creo colori
rm(list=ls())
dev.off(dev.list()["RStudioGD"])
cat("\014")
getwd()
my_col = brewer.pal(9,'Set1')
source('PuliziaNA.R')
database <- read.table('statoccupazionali.txt',header=T)
dati <- PuliziaNA(database, c(4,23,24))

#Introduco l'indice di soddisfazione complessiva del percorso
maxsodd<-max(dati$Soddisfazione_Laurea,dati$Soddisfazione_Preparazione)
Sodd_compl<-5*(dati$Soddisfazione_Laurea+dati$Soddisfazione_Preparazione)/maxsodd

#Soddisfatti: punteggio 9-10-8
#Insoddisfatti: punteggio <=7
plot( dati$Voto_LT, Sodd_compl, pch = ifelse( Sodd_compl >= 7.5,2,4),
      col = ifelse( Sodd_compl>=7.5 , 'forestgreen', 'red' ),
      xlab = 'Voto_LT', ylab = 'Soddisfazione', main = 'Soddisfazione vs Voto_LT', lwd = 2, cex = 1.5 )

Sodd_bin=rep(NA, length(Sodd_compl))
for (i in 1:length(Sodd_compl))
{Sodd_bin[i] <- ifelse(Sodd_compl[i]>=7.5,1,0)}

plot( dati$Voto_LM, Sodd_bin, pch = ifelse( Sodd_compl >= 1,2,4),
      col = ifelse( Sodd_bin>=1 , 'forestgreen', 'red' ),
      xlab = 'Voto_LM', ylab = 'Soddisfazione', main = 'Soddisfazione vs Voto_LM', lwd = 2, cex = 1.5 )
                     
#Creo il vettore delle soddisfazioni "binarie"

Sodd_bin=rep(NA, length(Sodd_compl))
for (i in 1:length(Sodd_compl))
  {Sodd_bin[i] <- ifelse(Sodd_compl[i]>=7.5,1,0)}

#Regressione logistica: Soddisfazione in base al Voto_LT

mod = glm( Sodd_bin ~ dati$Voto_LM, family = binomial( link = logit ) )
summary( mod )

#Disegno la curva ROC

fit2=mod$fitted.values

soglia_roc  = seq( 0, 1, length.out = 2e2 )
lens = length( soglia_roc )-1
ascissa_roc  = rep( NA, lens )
ordinata_roc = rep( NA, lens )


for ( k in 1 : lens )
{
  soglia = soglia_roc [ k ]
  
  classification = as.numeric( sapply( fit2, function( x ) ifelse( x < soglia, 0, 1 ) ) )
  
  ordinata_roc[ k ] = sum( classification[ which( Sodd_bin == 1 ) ] == 1 ) /
    length( which( Sodd_bin == 1 ) )
  
  ascissa_roc[ k ] = sum( classification[ which( Sodd_bin == 0 ) ] == 1 ) /
    length( which( Sodd_bin == 0 ) )
  
  plot( ascissa_roc, ordinata_roc, type = "l", xlab = "1 - Specificità", ylab = "Sensitività",
        main = "Curva ROC", lwd = 2, col = 'darkblue', ylim = c( 0, 1 ), xlim = c( 0, 1 ) )
  abline( h = c( 0, 1 ), v = c( 0, 1 ), lwd = 1, lty = 2, col = 'red' )
  abline( a = 0, b = 1, lty = 2, col = 'black' )
}

#Nuovo comando super powerful

ROCit_obj <- rocit(score=fit2,class=Sodd_bin)
plot(ROCit_obj)
my_roc <- roc(Sodd_bin, fit2)
soglia<-coords(my_roc, "best", ret = "threshold")

classification = as.numeric( sapply( fit2, function( x ) ifelse( x < soglia, 0, 1 ) ) )

ordinata_rocOpt = sum( classification[ which( Sodd_bin == 1 ) ] == 1 ) /
  length( which( Sodd_bin == 1 ) )

ascissa_rocOpt = sum( classification[ which( Sodd_bin == 0 ) ] == 1 ) /
  length( which( Sodd_bin == 0 ) )

#Calcolo AUC
roc_obj <- roc(Sodd_bin,fit2)
auc(roc_obj)