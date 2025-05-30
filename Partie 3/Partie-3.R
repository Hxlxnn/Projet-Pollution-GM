library(igraph)
library(Matrix)
library(dplyr)
library(ggplot2)

pollution <- read.csv("qualite_metro.csv")
graph_metro <- read.csv("graphe_metro.csv")

g <- graph_from_data_frame(graph_metro, directed = FALSE)
pollution_clean <- subset(pollution, niveau_pollution != "station aérienne")
levels <- c("pollution faible", "pollution moyenne", "pollution élevée", "station aérienne")
pollution_clean$pollution_num <- as.numeric(factor(pollution_clean$niveau_pollution, levels = levels))

V(g)$pollution <- pollution_clean$pollution_num[match(V(g)$name, pollution_clean$Nom.de.la.Station)]
head(data.frame(station = V(g)$name, pollution = V(g)$pollution))
L <- laplacian_matrix(g, sparse = FALSE)
signal <- V(g)$pollution

summary(signal)
sommets_a_supprimer <- which(is.na(V(g)$pollution))
g <- delete_vertices(g, sommets_a_supprimer)

L <- laplacian_matrix(g, sparse = FALSE)
signal <- V(g)$pollution
summary(signal)

eig <- eigen(L)
valeurs_propres <- eig$values         
vecteurs_propres <- eig$vectors

plot(valeurs_propres, type = "b", main = "Spectre du graphe", xlab = "Indice", ylab = "Valeur propre")

# Choix du nombre de composantes (k)
k <- 10

# Reconstruction du signal lissé à partir des premières composantes
U <- vecteurs_propres[, 1:k]  # les k premiers vecteurs propres
signal_proj <- U %*% (t(U) %*% signal)  # projection puis reconstruction
# Différence entre signal réel et signal lissé
anomalie <- abs(signal - signal_proj)

stations_anomalie <- data.frame(station = V(g)$name, pollution = signal, lisse = signal_proj, anomalie = anomalie)

V(g)$name <- as.character(V(g)$name)
V(g)$lat <- pollution_clean$stop_lat[match(V(g)$name, pollution_clean$'Nom.de.la.Station')]
V(g)$lon <- pollution_clean$stop_lon[match(V(g)$name, pollution_clean$'Nom.de.la.Station')]
stations_anomalie$lat <- V(g)$lat
stations_anomalie$lon <- V(g)$lon

stations_critiques <- stations_anomalie %>%
  arrange(desc(anomalie)) %>%
  head(10)

ggplot(stations_anomalie, aes(x = lon, y = lat)) +
  geom_point(alpha = 0.3, color = "grey") +  # toutes les stations
  geom_point(data = stations_critiques, aes(x = lon, y = lat), 
             color = "red", size = 4) +      # les stations critiques
  geom_text(data = stations_critiques, aes(label = station), 
            vjust = -1, size = 3) +          # noms des stations
  theme_minimal() +
  labs(title = "Stations les plus critiques selon l'analyse spectrale",
       x = "Longitude", y = "Latitude")

stations_polluees <- data.frame( station = V(g)$name,  pollution = V(g)$pollution )
top10 <- stations_polluees[order(-stations_polluees$pollution), ][1:10, ]
print(top10)

t <- 1  # temps de diffusion
filtre_chaleur <- exp(-t * valeurs_propres)

# Signal après diffusion
signal_diffuse <- vecteurs_propres %*% (filtre_chaleur * (t(vecteurs_propres) %*% signal))

plot(signal, type = "h", main = "pollution sur le graphe", xlab = "Station", ylab = "Pollution")
plot(signal_diffuse, type = "h", main = "Propagation de la pollution sur le graphe", xlab = "Station", ylab = "Pollution diffuse")

data.frame(stations_polluees$'station', pollution_diffuse = signal_diffuse) |>
  arrange(desc(pollution_diffuse)) |>
  head(10)

ggplot(pollution, aes(x = stop_lon, y = stop_lat)) +
  # Points des autres stations
  geom_point(data = subset(pollution, niveau_pollution != "station aérienne"),
             color = "blue", size = 2) +
  # Points des stations aériennes
  geom_point(data = subset(pollution, niveau_pollution == "station aérienne"),
             color = "red", size = 3) +
  # Points des stations sans données
  geom_point(data = subset(pollution, niveau_pollution == "pas de données"),
             color = "green", size = 3) +
  labs(title = "Carte des stations du métro : stations aériennes en rouge, sans données en verte",
       x = "Longitude", y = "Latitude") +
  theme_minimal()