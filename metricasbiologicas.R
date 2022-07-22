

# Instação dos pacotes:
# install.packages(c('cluster', 'factoextra', 'clValid', 'ggplot2', 'lemon', 'dplyr', 'tibble', 'Biobase', 'annotate', 'GO.db', 'moe430a.db'))

# Carregar os Pacotes:
library(cluster)
library(factoextra)
library(clValid)
library(ggplot2)
library(lemon)
library(dplyr)
library(tibble)

library(Biobase)
library(annotate)
library(GO.db)
library(moe430a.db)

# Carregar os dados:

data("mouse")
express <- mouse[,c("M1","M2","M3","NC1","NC2","NC3")]
rownames(express) <- mouse$ID


fc <- tapply(rownames(express),mouse$FC, c)
fc <- fc[!names(fc)%in%c("EST","Unknown")]

bio <- clValid(express, 2:6, clMethods= c("hierarchical","kmeans", "pam"),
               validation="biological", annotation=fc)

# Plotar os parâmetros BHI e BSI:

optimalScores(bio) # K-means apresenta o melhor valor de BHI


if(require("Biobase", quietly = TRUE) && require("annotate", quietly = TRUE) && 
   require("GO.db", quietly = TRUE) && require("moe430a.db", quietly = TRUE)) {
  bio2 <- clValid(express, 2:6, clMethods=c("hierarchical","kmeans","pam"),
                  validation="biological",annotation="moe430a.db",GOcategory="all")
    
if(exists("bio2")) optimalScores(bio2)

  
# plotar as métricas:
op <-  par(cex = 0.8)   
plot(bio, measure="BHI", legendLoc="topleft", pch = 16 )


plot(bio, measure="BSI")

