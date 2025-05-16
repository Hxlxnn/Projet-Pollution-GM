library(dplyr)

qualite <- read.table("qualite-de-lair-dans-le-reseau-de-transport-francilien (1).csv", header = TRUE, sep = ";")
head(qualite)

qualite_clean <- qualite %>%
  filter(rowSums(is.na(.)) != ncol(.)) %>%       
  select(where(~ !all(is.na(.)))) %>%             
  distinct()                                     


qualite_metro <- qualite_clean %>%
  filter(grepl("Métro", Nom.de.la.ligne))  # On filtre les stations de métro

# Créer un vecteur de correspondance pour les niveaux de pollution
degres_pollution <- c("pollution faible" = 1, "pollution moyenne" = 2, "pollution élevée" = 3, "station aérienne" = NA)

# Convertir la colonne "niveau_pollution" en valeurs numériques
qualite_metro$niveau_pollution <- degres_pollution[qualite_metro$niveau_pollution]

# Sélectionner les données pertinentes pour k-means
data_for_kmeans <- qualite_metro[, "niveau_pollution", drop = FALSE]

# Supprimer les valeurs manquantes
data_for_kmeans_clean <- na.omit(data_for_kmeans)

# Appliquer k-means sur les données sans NA
set.seed(123)  # Fixer la graine pour la reproductibilité
kmeans_model <- kmeans(data_for_kmeans_clean, centers = 3, nstart = 25)

# Afficher les résultats du modèle k-means
print(kmeans_model)

# Créer une copie du dataframe original avec les clusters
qualite_metro$cluster <- NA  # Initialiser la colonne 'cluster' avec des NA

# Ajouter les résultats du cluster à ton dataset original uniquement pour les lignes sans NA
qualite_metro[!is.na(qualite_metro$niveau_pollution), "cluster"] <- kmeans_model$cluster

# Vérifier les premières lignes du dataset avec les clusters
head(qualite_metro)

# Afficher les statistiques sur les clusters
table(qualite_metro$cluster)

library(ggplot2)

# Graphe de KMeans : Clusters vs Niveau de pollution réel
ggplot(qualite_metro, aes(x = factor(cluster), fill = factor(niveau_pollution))) +
  geom_bar(position = "fill") +
  labs(
    title = "Distribution des niveaux de pollution selon les clusters (KMeans)",
    x = "Cluster",
    y = "Proportion",
    fill = "Niveau pollution"
  ) +
  theme_minimal()
