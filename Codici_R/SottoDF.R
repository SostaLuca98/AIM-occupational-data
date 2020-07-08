# Title     : SottoDF
# Objective : TODO
# Created by: david
# Created on: 06/06/2020

SottoDF <- function(dati,vect){
  j <- vect[1]
  n <- sum(!is.na(dati[[j]]) )
  nuovo <- data.frame(1:n)
  nuovo[[1]] <- dati[[1]][which (!is.na(dati[[j]]))]
  if (length(vect)==1) {
    return (nuovo)
  }
  for (i in 2:length(vect)) {
    j <- vect[i]
    nuovo <- cbind(nuovo, dati[[j]])
  }

  for (i in 1:length(vect)) {
    j <- vect[i]
    names(nuovo)[i] <- names(dati)[j]
  }

  return (nuovo)

}
