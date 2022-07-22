
# Clusterização com CLARA :

# Instação dos pacotes:
# install.packages(c('cluster', 'factoextra'))

# Carregar os Pacotes:
library(cluster)
library(factoextra)

# Carregar os dados escalonados:
# Os dados foram obtidos junto ao Kaggle no link abaixo:

# https://www.kaggle.com/datasets/camnugent/california-housing-prices

# Carregar os dados já rescalados:
df2 <- read.csv(file = '../../data/df2.csv', sep = ';', header =  TRUE)

# checar os tipos de dados:
str(df2)

# Número ótimo de Clusters:
fviz_nbclust(df2, FUNcluster = clara, method = 'silhouette') + theme_classic()



# Implmentar o algoritmo Clara com o número de cluster ótimo encontrado:
clara_cluster <- clara(df2, k = 2, samples = 100, pamLike = TRUE)
print(clara_cluster)

# plotar o cluster:
fviz_cluster(clara_cluster, data = df2, palette = c('red', 'blue'),
             geom = 'point',
             ellipse.type = 'confidence',
             ggtheme = theme_bw()
             )

# plot da silhueta:
silhueta <- silhouette(clara_cluster$cluster, dist(df2))
fviz_silhouette(silhueta)





