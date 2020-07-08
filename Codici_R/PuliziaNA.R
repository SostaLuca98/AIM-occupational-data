# Title     : PuliziaNA
# Objective : Pulire gli NA nelle colonne indicate
# Created by: david
# Created on: 03/06/2020

#La funzione riceve in input il dataframe e le colonne da considerare. Per ognuna di queste colonne
#la funzione trova i valori NA ed elimina le corrispondenti righe dal dataframe

PuliziaNA <- function(dati,vect){

  if (length(vect)==0) {
    return (dati)
  } #Caso base della ricorsione: se non ci sono colonne da considerare smetto di lavorare

  n <- sum(!is.na(dati[[vect[1]]]) )            #Trovo dimensione che avrà il dataframe dopo pulizia della prima colonna
  datinew <- data.frame(1:n)  #Creo dataframe di una sola colonna della dimensione giusta
  datinew[[1]] <- dati[[1]][which (!is.na(dati[[vect[1]]]))]  #Inserisco i dati puliti relativi alla prima colonna

  for (j in 2:length(names(dati))) {
      cbind(datinew,names(dati)[j])         #Lego le altre colonne al dataframe
      datinew[[j]] <- dati[[j]][which (!is.na(dati[[vect[1]]]))] #Inserisco i dati puliti
  }
  A <- PuliziaNA(datinew, vect[-1])
  for (j in seq_along(names(dati))) {
  names(A)[j] <- names(dati)[j]
  }

  return(A)
}
