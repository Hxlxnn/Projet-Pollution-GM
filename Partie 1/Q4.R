library(dplyr)
library(ggplot2)

qualite <- read.table("qualite-de-lair-dans-le-reseau-de-transport-francilien (1).csv", header = TRUE, sep = ";", quote = "\"")

qualite_clean <- qualite %>%
  filter(rowSums(is.na(.)) != ncol(.)) %>%
  select(where(~ !all(is.na(.)))) %>%
  distinct()

qualite_metro <- qualite_clean %>%
  filter(grepl("Métro", Nom.de.la.ligne))

degres_pollution <- c("pollution faible" = 1, "pollution moyenne" = 2, "pollution élevée" = 3, "station aérienne" = NA)
qualite_metro$niveau_pollution <- degres_pollution[qualite_metro$niveau_pollution]

data_for_kmeans <- qualite_metro %>%
  select(niveau_pollution, stop_lon, stop_lat) %>%
  filter(!is.na(niveau_pollution), !is.na(stop_lon), !is.na(stop_lat))

set.seed(123)
kmeans_model <- kmeans(data_for_kmeans, centers = 3, nstart = 25)

qualite_metro$cluster <- NA
indices <- which(!is.na(qualite_metro$niveau_pollution) & !is.na(qualite_metro$stop_lon) & !is.na(qualite_metro$stop_lat))
qualite_metro$cluster[indices] <- kmeans_model$cluster

kmeans_model

ggplot(qualite_metro[!is.na(qualite_metro$cluster), ], aes(x = stop_lon, y = stop_lat, color = factor(cluster))) +
  geom_point(alpha = 0.7, size = 2) +
  labs(color = "Cluster", x = "Longitude", y = "Latitude", title = "Clustering des stations de métro selon pollution et position géographique") +
  theme_minimal()
