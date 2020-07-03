library(tidyverse)
library(caret)
library(leaps)
library(car)
library(DMwR)
library(GGally)
library(MASS)
library( ellipse )
library( faraway )


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
reg <- lm(varlog ~ .-Retribuzione_PI-dati_definitivi_id, data=datireg)
summary(reg)

slm1 <- step(reg, na.omit=TRUE, scope = . ~ .^2, nvmax = 4, trace = -1 )
summary(slm1)
shapiro.test(slm1$residuals)

slm2 <- step(reg, na.omit=TRUE, trace = -1 )
summary(slm2)
shapiro.test(slm2$residuals)
vif(slm1)

#proviamo un boxcox per vedere se siamo ottimali 
boxcox(slm2)
#praticamente sì perchè lambda=1 vuol dire tengo la variabile attuale, che è log(stipendi)

#Diagnsotics on this model
plot(slm2$fitted.values,hat(model.matrix(slm2)))
hmeans=2*summary(slm2)$df[1]/(summary(slm2)$df[2]+summary(slm2)$df[1])
abline(h=hmeans)
lev=hat(model.matrix(slm2))
bad_leverages=which(hat(model.matrix(slm2))>hmeans)
datireg$dati_definitivi_id[bad_leverages] #to be controlled

#Single out high studentized residuals
stud = rstandard( slm2 )
watchout_ids_stud = which( abs( stud ) > 2 )
watchout_stud = stud[ watchout_ids_stud ]

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
plot( slm2$fitted.values, stud, pch = 16, xlab = 'Fitted values',
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

#leave one out cross validation
#non mi funziona

#result<-loocv(slm2,dataset(varlog ~ Voto_LM + Durata + Stage + Erasmus, datireg), loocvSettings(1234))

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
# datireg <- datireg[id_to_keep]
# varlog  <- varlog[id_to_keep]
slm4 <- regsubsets(varlog ~.-Retribuzione_PI-dati_definitivi_id,data=datireg, nvmax=5,force.out=1,intercept=TRUE)
res.sum <- summary(slm4)

#Posso scegliere il mio dello preferito in base all'indice che mi piace di più
data.frame(
  Adj.R2 = which.max(res.sum$adjr2),
  CP = which.min(res.sum$cp),
  BIC = which.min(res.sum$bic)
)

get_model_formula(4, slm4, "Retribuzione_PI")

#slm3 non ha voti triennale, vediamo cor con voti lm
#kendall's tau = (n. concordant-n.discordant)/(0.5 n*(n-1)) prende i dati a coppie e 
#vede se sono concordi o discordi tra le due variabili osservate: 
#se x1>=x2, y1>=y2 o x1<x2, y1<y2 allora sono concordi, altrimenti sono discordi (non sono sicura che l'uguale sia giusto lì)
corLTLMk<-cor.test(datireg$Voto_LT,datireg$Voto_LM,method= "kendall")
corLTLMk
#spearman's rank: sorta di estensione di R^2, va bene per variabili di tipo " rank" cioè dove si può trovare un ordinamento (bene quindi variabili discrete)
# si calcola come 1- somma(distanza tra xi e yi)/(n*(n^2-1)/6) dove distanza tra xi e yi viene calcolata ordinando in modo crescente le x e numerando da 1 a n, ordinando in modo crescente le y e numerando da 1 a n, infine facendo il ^2 della differenza tra questi numeriper ogni osservazione 
corLTLMs<-cor.test(datireg$Voto_LT,datireg$Voto_LM,method= "spearman")
corLTLMs
#dà problemi col pvalue ma comunque mi sembra evidente che la correlazione c'è e anche forte (>0.5 è già molta, qui è 0.63)
#quindi scelgo il modello 4 che minimizza cp e bic

# Compute cross-validation error
model.ids <- 1:5
cv.errors <-  map(model.ids, get_model_formula, slm4, "Retribuzione_PI") %>%
  map(get_cv_error, data = datireg, numfold=5) %>%
  unlist()
cv.errors  #Il modello che ridue l'errore cv è quello con 4 variabili
which.min(cv.errors)
coef(slm4,4) #I coefficienti sono molto simili a slm3, mi pare una cosa buona
coef(slm4,5) #Aumentando il numero di fold l'errore minore si ha per 4 o 5 predittori,ma se 5 avrei entrammbe le lauree


#Cerco di interpretare quanto ho trovato
#log Y=b0+b1*Z1+b2*Z2+... <-> Y=e^b0*e^(b1*Z1)*e^(b2*Z2)*...
#usare il logaritmo degli stipendi vuol dire limitarsi a un range di 
min_stip_log<-min(varlog)
max_stip_log<-max(varlog)

min_stip_log
max_stip_log


#di questo, circa 3.25 è comune a tutti, e influisce con un fattore moltiplicativo di circa 25
exp(slm3$coefficients[1])
#mentre il resto dipende: - dal voto di laurea magistrale
exp(slm3$coefficients[2]*min(datireg$Voto_LM[id_to_keep]))
exp(slm3$coefficients[2]*max(datireg$Voto_LM[id_to_keep]))
#La variabilità è molto alta: tra il minimo e il massimo voto di laurea lo stipendio raddoppia!
#La dipendenza è tramite coefficiente positivo: al crescere del voto cresce lo stipendio medio al primo impiego

#Si potrebbe andare a vedere se il terzo quantile e sopra di stipendi ha trovato lavoro con modalità diversa rispetto al primo quantile e sotto

#-dalla durata degli studi ???

summary(datireg$Durata)
plot(datireg$Durata[id_to_keep],exp(slm3$fitted.values))
#non so io non ci vedo tutto questo significato, comunque l'interpretazione che potremmo dare è che chi si laurea più in fretta è disposto ad accettare 
#ma tra l'altro c'è correlazione tra voto finale e durata?
test<-cor.test(datireg$Durata[id_to_keep],datireg$Voto_LM[id_to_keep],method= "kendall")
test
#non sembra così diverso da zero
testspear<-cor.test(datireg$Durata[id_to_keep],datireg$Voto_LM[id_to_keep],method= "spearman")
testspear
#senza troppa convinzione diciamo che c'è una leggera correlazione negativa in generale comunque le ragioni di un percorso più lungo non sono ricerca di perfezionismo, se mai leggermente chi fa più fatica ci mette di più
prplot(slm2,2)
#però effettivamente il beta considerato descrive un andamento crescente dello stipendio, quindi sembra confermarsi questa cosa controintuitiva: diresti che uno cerca di far durare di più per avere i voti perfetti ma non è così perchè non c'è corr positiva tra i due... 
durataeerasmus<-cor.test(datireg$Durata[id_to_keep],as.numeric(datireg$Erasmus[id_to_keep]),method= "spearman")
durataeerasmus
#non si può affatto dire che le due cose siano correlate

#-aver fatto uno stage (dà un leggero vantaggio)
exp(slm3$coefficients[4])

#-aver fatto l'erasmus (conta anche un po' di più dello stage, segno che è valutato positivamente)
exp(slm3$coefficients[5])
#tieni conto che questo 1.3 ti manda lo stipendio su di 500€ che è più del 20% nel caso di un profilo ottimo (110, 6 anni che è la mendiana)
#senza
exp(slm3$coefficients[1]+110*slm3$coefficients[2]+6*slm3$coefficients[3])
#con
exp(slm3$coefficients[1]+110*slm3$coefficients[2]+6*slm3$coefficients[3])* exp(slm3$coefficients[5])

#mentre con lo stage
exp(slm3$coefficients[1]+110*slm3$coefficients[2]+6*slm3$coefficients[3])* exp(slm3$coefficients[4])
#qui sono 300€




#quello che potremmo fare è dare un IC per studente laureato in 6 anni (che è la mediana) per la retribuzione in base a voto di laurea, nei casi sì/no stage, sì/no erasmus
#-> scopo: aiutare gli studenti a pianificare la loro carriera (fare o no erasmus/stage/sacrificare un po' di tempo per avere voti migliori) in base a quanto desiderano arrivare a guadagnare
grid = seq( min( datireg$Voto_LM ), max( datireg$Voto_LM ), by=1)

# attach(datireg)
# 
# newdata<-data.frame(
#   Voto_LM<-grid,
#   Durata=6,
#   Erasmus=as.factor(1),
#   Stage=as.factor(1)
# )
# 
# ymeanpred<-predict(slm3,newdata,interval="confidence",se=T)
# ey<-ymeanpred$fit[,1]
# ey.inf<-ymeanpred$fit[,2]
# ey.sup<-ymeanpred$fit[,3]
# ypred<-predict(slm3,newdata,interval="prediction",se=T)
# y<-ypred$fit[,1]
# y.sup<-ypred$fit[,3]
# y.inf<-ypred$fit[,2]
# 
# matplot( grid, cbind( ey, ey.inf, ey.sup ), lty = c( 1, 2, 2 ),
#          col = c( 1, 'blue', 'blue' ), type = "l", xlab = "voto_LM",
#          ylab = "primo_stipendio", main = 'IC per la media della risposta' )
# points( Voto_LM, Retribuzione_PI, col = "black", pch = 16 )
# 
# detach(datireg)

#non capisco perchè ma mi dà ey con due valori in meno di grid e non sapendo quali non vanno conviene fare manualmente
#ci sono problemi conl'ottavo dato a quanto pare e con il ventitrè

ndata = cbind( rep( 1, length( grid ) ), grid,6*rep( 1, length( grid ) ),rep( 1, length( grid ) ),rep( 1, length( grid ) ) )
y.pred_fit = ndata %*% slm3$coefficients
y.pred_fit
n=length(grid)
y.pred_se = rep( 0, n )
X = model.matrix( slm3 )
for( i in 1:n )
{
  y.pred_se[ i ] = summary( slm3 )$sigma * sqrt( t( ndata[i,] ) %*% solve( t(X) %*% X ) %*% ndata[i,] )
}
y.pred_se
p=4
y.pred_df=n-p-1
tc = qt( 0.975, y.pred_df )

ey = y.pred_fit[ ,1 ]
ey.sup = y.pred_fit[ ,1 ] + tc * y.pred_se
ey.inf = y.pred_fit[ ,1 ] - tc * y.pred_se
eIC = cbind( ey, ey.inf, ey.sup )

matplot( grid, cbind( ey, ey.inf, ey.sup ), lty = c( 1, 2, 2 ),
                  col = c( 1, 'blue', 'blue' ), type = "l", xlab = "voto_LM",
                  ylab = "primo_stipendio", main = 'IC per la media della risposta' )
points( datireg$Voto_LM[id_to_keep], varlog[id_to_keep], col = "black", pch = 16 )

y.pred2_se = rep( 1, n )
for( i in 1:n ){
  y.pred2_se[ i ] = summary( slm3 )$sigma * sqrt( 1 + t( ndata[i,] ) %*% solve( t(X) %*% X ) %*% ndata[i,]) }

y.pred2_se


tc = qt( 0.975, n-p-1 )
y = y.pred_fit[,1]
y.sup = y.pred_fit[,1] + tc * y.pred2_se
y.inf = y.pred_fit[,1] - tc * y.pred2_se
IP = cbind( y, y.inf, y.sup )
IP

lines( grid, y.sup , col = "blue", lty = 2, xlab = "voto_LM", ylab = "primo_stipendio" )
lines( grid, y.inf , col = "blue", lty = 2, xlab = "voto_LM", ylab = "primo_stipendio" )

#passo a stipendio non logaritmico
w=exp(y.pred_fit)
w.sup=exp(y.sup)
w.inf=exp(y.inf)

#Plotto intervallo di predizione al 5% (che mi sa che è più dieci percento :{ ). Però calma tutto ciò ha senso perchè abbiamo fissato stage, erasmus e durata!! Quindi non male, anzi mostra che non è così sbagliato rappresentare tutti con quelle lì Direi che lasciamo perdere intervallo per la media
matplot( grid, cbind( w, w.inf, w.sup ), lty = c( 1, 2, 2 ),
         col = c( 1, 'blue', 'blue' ), type = "l", xlab = "voto_LM",
         ylab = "primo_stipendio", main = 'IC per la media della risposta' )
points( datireg$Voto_LM[id_to_keep], datireg$Retribuzione_PI[id_to_keep], col = "black", pch = 16 )

#ripeto tuuutto con 5 anni
ndata5 = cbind( rep( 1, length( grid ) ), grid,5*rep( 1, length( grid ) ),rep( 1, length( grid ) ),rep( 1, length( grid ) ) )
y5.pred_fit = ndata5 %*% slm3$coefficients
y5.pred_fit
y5.pred_se = rep( 0, n )
for( i in 1:n )
{
  y5.pred_se[ i ] = summary( slm3 )$sigma * sqrt( 1 + t( ndata5[i,] ) %*% solve( t(X) %*% X ) %*% ndata5[i,])
}
y5.pred_se

y5 = y5.pred_fit
y5.sup = y5.pred_fit + tc * y5.pred_se
y5.inf = y5.pred_fit - tc * y5.pred_se


#passo a stipendio non logaritmico
w5=exp(y5.pred_fit)
w5.sup=exp(y5.sup)
w5.inf=exp(y5.inf)

#meglio 5 in realtà

#ripeto con no erasmus
ndatae = cbind( rep( 1, length( grid ) ), grid,5*rep( 1, length( grid ) ),rep( 1, length( grid ) ),0*rep( 1, length( grid ) ) )
ye.pred_fit = ndatae %*% slm3$coefficients
ye.pred_fit
ye.pred_se = rep( 0, n )
for( i in 1:n )
{
  ye.pred_se[ i ] = summary( slm3 )$sigma * sqrt( 1 + t( ndatae[i,] ) %*% solve( t(X) %*% X ) %*% ndatae[i,])
}
ye.pred_se

ye = ye.pred_fit
ye.sup = ye.pred_fit + tc * ye.pred_se
ye.inf = ye.pred_fit - tc * ye.pred_se


#passo a stipendio non logaritmico
we=exp(ye.pred_fit)
we.sup=exp(ye.sup)
we.inf=exp(ye.inf)

lines( grid, we.sup , col = "green", lty = 2, xlab = "voto_LM", ylab = "primo_stipendio" )
lines( grid, we.inf , col = "green", lty = 2, xlab = "voto_LM", ylab = "primo_stipendio" )
#Il cambiamento è drammatico... da pagato la fame uno passa a pagato bene. Molto bello, peccato che molti dei dati stiano qua dentro invece che sull'intervallo con l'erasmus... comunque erasmus sono 27 su 77 quindi neanche così insignificante


#Per riassumere:
# -il bello del modello con risposta trasformata logaritmicamente è che le covariate influiscono non sommandosi ma moltiplicandosi al valore dovuto all'intercetta
# -notevole il contributo che dà l'Erasmus (aumento mediamente di 500€, si vede dal coefficiente beta e confermato dagli intervalli di previsione (verde no Eramus, rosso sì))
# -un po' minore ma sempre interessante lo stage fatto
# -strano fenomeno per cui aumenta lo stipendio insieme alla durata del percorso di studi (e questa io non me la spiego proprio...)
# -il logaritmo è effettivamente la maniera migliore di trattare la Y, così che sia quasi quasi normale. Inoltre il modello migliore ha le 4 covariate selezionate (confermato dallo step e dal training con regsubsets)
# - NON sono riuscita a fare il leave one out test, bisognerebbe riprovarci
# -esiste una correlazione positiva forte tra voto LT e LM, nessuna invece tra voto e durata studi
# -(fatto bello da sottolineare secondo me): in nessuno dei modelli provati il sesso è significativo: il nostro corso è molto equo in termini di genere :)
