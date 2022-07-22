

# Instação dos pacotes:
# install.packages(c('cluster', 'factoextra', 'clValid', 'ggplot2', 'lemon', 'dplyr', 'tibble'))

# Carregar os Pacotes:
library(cluster)
library(factoextra)
library(clValid)
library(ggplot2)
library(lemon)
library(dplyr)
library(tibble)


# Carregar os dados escalonados:
# Os dados foram obtidos junto ao Kaggle no link abaixo:

# https://www.kaggle.com/datasets/camnugent/california-housing-prices

# Carregar os dados já rescalados:
df2 <- read.csv(file = '.../data/df2.csv', sep = ';', header =  TRUE)


# Métricas Internas (Concetividade, Dunn e Silhueta):
# Estimando o número de clusters, de 2 à 8 grupos:
cmin <- 2
cmax <- 8
algoritmos <- c("hierarchical","kmeans","pam", "clara")

intern <- clValid::clValid(df2, cmin:cmax, 
                           clMethods = algoritmos, validation = "internal", maxitems = nrow(df2))
summary(intern) 


# Plotar as métricas internas de Validação: Conectividade, Dunn e Silhueta:
op <- par(no.readonly = TRUE)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
plot(intern, legend = FALSE)
plot(nClusters(intern), measures(intern, "Dunn")[, , 1], type = "n",
     axes = F, xlab = "", ylab = "")
legend("center", clusterMethods(intern), col = 1:4, lty = 1:4,
       pch = paste(1:4)) ; par(op)



# Métricas de Estabilidade:
# Métrica APN (Average Proportion of Non-overlap)
# A métrica AD (Average Distance)
# A métrica ADM (Average Distance between Means)
# A métrica FOM (Figure of Merit)

# Estimando o número de clusters, de 2 à 8 grupos e avaliando as métricas de estabilidade:
cmin <- 2
cmax <- 8
algoritmos <- c("hierarchical","kmeans","pam", "clara")
estabilidade <- clValid::clValid(df2, cmin:cmax, 
                           clMethods = algoritmos, validation = "stability", maxitems = nrow(df2))
summary(estabilidade) 

# plotar as métricas de estabilidade:
df_estabilidade <- as.data.frame(estabilidade@measures)

# hierárquico:
df_estabilidade_hierar <-  df_estabilidade %>% 
                            dplyr::select(1:length(cmin:cmax)) %>% t %>%
                            as.data.frame() %>% 
                            tibble::remove_rownames() %>% 
                            dplyr::mutate(Clusters = 2:8) %>%  
                            reshape2::melt(id.vars = "Clusters")


metricas_estab_hier <- ggplot2::ggplot(df_estabilidade_hierar) +
                            lemon::geom_pointline(aes(x = Clusters, y = value, 
                            col = variable, shape = variable),
                            size = 3, show.legend = F) +
                            facet_wrap(~variable, scales = "free") +
                            xlab("Número de Clusters - Agrupamento Hierárquico") + ylab("Valor")  + theme(axis.text.x = element_text(angle = 0),
                                          axis.title.x = element_text(size = 16, face = "bold"))

metricas_estab_hier


# K-Means:

df_estabilidade_kmeans <-  df_estabilidade %>%  
                           dplyr::select((length(cmin:cmax) + 1):(length(cmin:cmax)+ length(cmin:cmax))) %>% t %>%
                           as.data.frame() %>% 
                           tibble::remove_rownames() %>% 
                           dplyr::mutate(Clusters = 2:8) %>% 
                           reshape2::melt(id.vars = "Clusters")


metricas_estab_kmeans <- ggplot2::ggplot(df_estabilidade_kmeans) +
  lemon::geom_pointline(aes(x = Clusters, y = value, 
                            col = variable, shape = variable),
                        size = 3, show.legend = F) +
  facet_wrap(~variable, scales = "free") +
  xlab("Número de Clusters - Agrupamento K-means") + ylab("Valor")  + theme(axis.text.x = element_text(angle = 0),
                                                                                axis.title.x = element_text(size = 16, face = "bold"))

metricas_estab_kmeans


# PAM:

df_estabilidade_pam <-  df_estabilidade %>%  
                        dplyr::select(( length(cmin:cmax) + length(cmin:cmax) + 1):(length(cmin:cmax)+length(cmin:cmax)+length(cmin:cmax))) %>% t %>%
                        as.data.frame() %>% 
                        tibble::remove_rownames() %>% 
                        dplyr::mutate(Clusters = 2:8) %>% 
                        reshape2::melt(id.vars = "Clusters")

metricas_estab_pam <- ggplot2::ggplot(df_estabilidade_pam) +
  lemon::geom_pointline(aes(x = Clusters, y = value, 
                            col = variable, shape = variable),
                        size = 3, show.legend = F) +
  facet_wrap(~variable, scales = "free") +
  xlab("Número de Clusters - Agrupamento PAM") + ylab("Valor")  + theme(axis.text.x = element_text(angle = 0),
                                                                            axis.title.x = element_text(size = 16, face = "bold"))

metricas_estab_pam


# clara

df_estabilidade_clara <- df_estabilidade %>%
                         dplyr::select(( length(cmin:cmax) + length(cmin:cmax) + length(cmin:cmax) + 1):(length(cmin:cmax)+length(cmin:cmax)+length(cmin:cmax)+length(cmin:cmax)))%>% t %>%
                         as.data.frame() %>% 
                         tibble::remove_rownames() %>% 
                         dplyr::mutate(Clusters = 2:8) %>% 
                         reshape2::melt(id.vars = "Clusters")

metricas_estab_clara <- ggplot2::ggplot(df_estabilidade_clara) +
  lemon::geom_pointline(aes(x = Clusters, y = value, 
                            col = variable, shape = variable),
                        size = 3, show.legend = F) +
  facet_wrap(~variable, scales = "free") +
  xlab("Número de Clusters - Agrupamento CLARA") + ylab("Valor")  + theme(axis.text.x = element_text(angle = 0),
                                                                        axis.title.x = element_text(size = 16, face = "bold"))

metricas_estab_pam





