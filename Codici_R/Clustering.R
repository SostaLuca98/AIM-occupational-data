# Title     : Clustering
# Objective : Clustering
# Created by: david
# Created on: 06/06/2020

library(GGally)
library(rms)
library(arm)
library(ResourceSelection)
library(pROC)

#Pulisco la console, stampo directory, importo funzione di pulizia, importo dati
rm(list=ls())
currwd <- getwd()
setwd(currwd)
source('PuliziaNA.R')
source('SottoDF.R')
database <- read.table('statoccupazionali.txt',header=T)
names(database)

#----- Dissimilarity Matrix -----#

dati <- SottoDF(database, c(1, 7, 22, 23, 24))

for (i in 1:dim(dati)[1]) {
  if (dati[[2]][[i]]=='Prelaurea') {dati[[2]][[i]] <- '1 Tempo'}
  if (dati[[2]][[i]]=='Immediato') {dati[[2]][[i]] <- '2 Tempo'}
  if (dati[[2]][[i]]=='Veloce') {dati[[2]][[i]] <- '3 Tempo'}
  if (dati[[2]][[i]]=='Medio') {dati[[2]][[i]] <- '4 Tempo'}
  if (dati[[2]][[i]]=='Lungo') {dati[[2]][[i]] <- '5 Tempo'}
}

for (i in 1:dim(dati)[1]) {
  if (dati[[3]][[i]]==1) {dati[[3]][[i]] <- '1 Lavoro'}
  if (dati[[3]][[i]]==2) {dati[[3]][[i]] <- '2 Lavoro'}
  if (dati[[3]][[i]]==3) {dati[[3]][[i]] <- '3 Lavoro'}
  if (dati[[3]][[i]]==4) {dati[[3]][[i]] <- '4 Lavoro'}
  if (dati[[3]][[i]]==5) {dati[[3]][[i]] <- '5 Lavoro'}
}

for (i in 1:dim(dati)[1]) {
  if (dati[[4]][[i]]==1) {dati[[4]][[i]] <- '1 Laurea'}
  if (dati[[4]][[i]]==2) {dati[[4]][[i]] <- '2 Laurea'}
  if (dati[[4]][[i]]==3) {dati[[4]][[i]] <- '3 Laurea'}
  if (dati[[4]][[i]]==4) {dati[[4]][[i]] <- '4 Laurea'}
  if (dati[[4]][[i]]==5) {dati[[4]][[i]] <- '5 Laurea'}
}

for (i in 1:dim(dati)[1]) {
  if (dati[[5]][[i]]==1) {dati[[5]][[i]] <- '1 Prep'}
  if (dati[[5]][[i]]==2) {dati[[5]][[i]] <- '2 Prep'}
  if (dati[[5]][[i]]==3) {dati[[5]][[i]] <- '3 Prep'}
  if (dati[[5]][[i]]==4) {dati[[5]][[i]] <- '4 Prep'}
  if (dati[[5]][[i]]==5) {dati[[5]][[i]] <- '5 Prep'}
}

#dati$Major <- factor ( dati$Major )
dati$Soddisfazione_Lavorativa <- factor ( dati$Soddisfazione_Lavorativa )
dati$Soddisfazione_Laurea <- factor ( dati$Soddisfazione_Laurea )
dati$Soddisfazione_Preparazione <- factor ( dati$Soddisfazione_Preparazione )
dati$Tempo_PI <- factor ( dati$Tempo_PI )

library(cluster)
# to perform different types of hierarchical clustering
# package functions used: daisy(), diana(), clusplot()
gower.dist <- daisy(dati [ , c(2,3,4,5)], metric = c("gower"))
#class(gower.dist)
## dissimilarity , dist

divisive.clust <- diana(as.matrix(gower.dist), diss = TRUE, keep.diss = TRUE)
plot(divisive.clust, main = "Divisive")

#------------ AGGLOMERATIVE CLUSTERING ------------#
# I am looking for the most balanced approach
# Complete linkages is the approach that best fits this demand - I will leave only this one here, don't want to get it cluttered
# complete
aggl.clust.c <- hclust(gower.dist, method = "complete")
plot(aggl.clust.c, main = "Agglomerative, complete linkages")

# Cluster stats comes out as list while it is more convenient to look at it as a table
# This code below will produce a dataframe with observations in columns and variables in row
# Not quite tidy data, which will require a tweak for plotting, but I prefer this view as an output here as I find it more comprehensive
library(fpc)
cstats.table <- function(dist, tree, k) {
clust.assess <- c("cluster.number","n","within.cluster.ss","average.within","average.between",
                  "wb.ratio","dunn2","avg.silwidth")
clust.size <- c("cluster.size")
stats.names <- c()
row.clust <- c()
output.stats <- matrix(ncol = k, nrow = length(clust.assess))
cluster.sizes <- matrix(ncol = k, nrow = k)
for(i in c(1:k)){
  row.clust[i] <- paste("Cluster-", i, " size")
}
for(i in c(2:k)){
  stats.names[i] <- paste("Test", i-1)

  for(j in seq_along(clust.assess)){
    output.stats[j, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.assess])[j]

  }

  for(d in 1:k) {
    cluster.sizes[d, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.size])[d]
    dim(cluster.sizes[d, i]) <- c(length(cluster.sizes[i]), 1)
    cluster.sizes[d, i]

  }
}
output.stats.df <- data.frame(output.stats)
cluster.sizes <- data.frame(cluster.sizes)
cluster.sizes[is.na(cluster.sizes)] <- 0
rows.all <- c(clust.assess, row.clust)
# rownames(output.stats.df) <- clust.assess
output <- rbind(output.stats.df, cluster.sizes)[ ,-1]
colnames(output) <- stats.names[2:k]
rownames(output) <- rows.all
is.num <- sapply(output, is.numeric)
output[is.num] <- lapply(output[is.num], round, 2)
output
}
# I am capping the maximum amout of clusters by 7
# I want to choose a reasonable number, based on which I will be able to see basic differences between customer groups as a result
stats.df.divisive <- cstats.table(gower.dist, divisive.clust, 7)
stats.df.divisive

stats.df.aggl <-cstats.table(gower.dist, aggl.clust.c, 7) #complete linkages looks like the most balanced approach
stats.df.aggl

# --------- Choosing the number of clusters ---------#
# Using "Elbow" and "Silhouette" methods to identify the best number of clusters
# to better picture the trend, I will go for more than 7 clusters.
library(ggplot2)
# Elbow
# Divisive clustering
ggplot(data = data.frame(t(cstats.table(gower.dist, divisive.clust, 15))),
  aes(x=cluster.number, y=within.cluster.ss)) +
  geom_point()+
  geom_line()+
  ggtitle("Divisive clustering") +
  labs(x = "Num.of clusters", y = "Within clusters sum of squares (SS)") +
  theme(plot.title = element_text(hjust = 0.5))

# Agglomerative clustering,provides a more ambiguous picture
ggplot(data = data.frame(t(cstats.table(gower.dist, aggl.clust.c, 15))),
  aes(x=cluster.number, y=within.cluster.ss)) +
  geom_point()+
  geom_line()+
  ggtitle("Agglomerative clustering") +
  labs(x = "Num.of clusters", y = "Within clusters sum of squares (SS)") +
  theme(plot.title = element_text(hjust = 0.5))

# Silhouette
ggplot(data = data.frame(t(cstats.table(gower.dist, divisive.clust, 15))),
  aes(x=cluster.number, y=avg.silwidth)) +
  geom_point()+
  geom_line()+
  ggtitle("Divisive clustering") +
  labs(x = "Num.of clusters", y = "Average silhouette width") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = data.frame(t(cstats.table(gower.dist, aggl.clust.c, 15))),
  aes(x=cluster.number, y=avg.silwidth)) +
  geom_point()+
  geom_line()+
  ggtitle("Agglomerative clustering") +
  labs(x = "Num.of clusters", y = "Average silhouette width") +
  theme(plot.title = element_text(hjust = 0.5))

library("ggplot2")
library("reshape2")
library("purrr")
library("dplyr")
# let's start with a dendrogram
suppressPackageStartupMessages(library(dendextend))
dendro <- as.dendrogram(aggl.clust.c)
dendro.col <- dendro %>%
  set("branches_k_color", k = 7, value =   c("darkslategray", "darkslategray4", "darkslategray3", "gold3", "darkcyan", "cyan3", "gold3")) %>%
  set("branches_lwd", 0.6) %>%
  set("labels_colors",
      value = c("darkslategray")) %>%
  set("labels_cex", 0.5)
ggd1 <- as.ggdend(dendro.col)
ggplot(ggd1, theme = theme_minimal()) +
  labs(x = "Num. observations", y = "Height", title = "Dendrogram, k = 7")

# Radial plot looks less cluttered (and cooler)
ggplot(ggd1, labels = T) +
  scale_y_reverse(expand = c(0.2, 0)) +
  coord_polar(theta="x")

# Time for the heatmap
# the 1st step here is to have 1 variable per row
# factors have to be converted to characters in order not to be dropped
clust.num <- cutree(aggl.clust.c, k = 7)
dati.cl <- cbind(dati, clust.num)
cust.long <- melt(data.frame(lapply(dati.cl, as.character), stringsAsFactors=FALSE),
                  id = c('dati_definitivi_id', "clust.num"), factorsAsStrings=T)
cust.long.q <- cust.long %>%
  group_by(clust.num, variable, value) %>%
  mutate(count = n_distinct(dati_definitivi_id)) %>%
  distinct(clust.num, variable, value, count)
# heatmap.c will be suitable in case you want to go for absolute counts - but it doesn't tell much to my taste
heatmap.c <- ggplot(cust.long.q, aes(x = clust.num,
                                     y = factor(value, levels = c("x","y","z", "mon", "tue", "wed", "thu", "fri","sat","sun",
                                                                  "delicious", "the one you don't like", "pizza",
                                                                  "facebook", "email", "link", "app",
                                                                  "area1", "area2", "area3", "area4",
                                                                  "small", "med", "large"), ordered = T))) +

  geom_tile(aes(fill = count))+
  scale_fill_gradient2(low = "darkslategray1", mid = "yellow", high = "turquoise4")
# calculating the percent of each factor level in the absolute count of cluster members
cust.long.p <- cust.long.q %>%
  group_by(clust.num, variable) %>%
  mutate(perc = count / sum(count)) %>%
  arrange(clust.num)
heatmap.p <- ggplot(cust.long.p, aes(x = clust.num, y = factor(value, levels = c('1 Tempo','2 Tempo','3 Tempo','4 Tempo','5 Tempo', '1 Lavoro',
                                                                                 '2 Lavoro','3 Lavoro','4 Lavoro','5 Lavoro', '2 Laurea','3 Laurea','4 Laurea','5 Laurea',
                                                                                   '1 Prep','2 Prep','3 Prep','4 Prep','5 Prep' ), ordered = T))) +
geom_tile(aes(fill = perc), alpha = 0.85)+
  labs(title = "Distribution of characteristics across clusters", x = "Cluster number", y = NULL) +
  geom_hline(yintercept = 5.5) +
  geom_hline(yintercept = 9.5) +
  geom_hline(yintercept = 12.5) +
  geom_hline(yintercept = 17.5) +
  geom_hline(yintercept = 23.5) +
  geom_hline(yintercept = 26.5) +
  scale_fill_gradient2(low = "darkslategray1", mid = "yellow", high = "turquoise4")
heatmap.p
