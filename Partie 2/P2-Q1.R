library(dplyr)
library(readr)
library(tidyr)

df <- read_delim("qualite-de-lair-dans-le-reseau-de-transport-francilien (1).csv",
                 delim = ";", locale = locale(encoding = "UTF-8"))

df_metro <- df %>%
  filter(grepl("Métro", `Nom de la ligne`)) %>%
  distinct(station = `Nom de la Station`, ligne = `Nom de la ligne`) %>%
  filter(!is.na(station), !is.na(ligne))

stations_uniques <- df_metro %>% distinct(station) %>% arrange(station)


couples_possibles <- expand_grid(
  station1 = stations_uniques$station,
  station2 = stations_uniques$station
) %>%
  filter(station1 < station2)  # Éviter doublons et auto-connections

df_liaisons <- df_metro %>%
  inner_join(df_metro, by = "ligne") %>%
  filter(station.x < station.y) %>%
  distinct(station1 = station.x, station2 = station.y) %>%
  mutate(connecte = TRUE) #On leur donne une valeur connecte=TRUE

graphe_complet <- couples_possibles %>%
  left_join(df_liaisons, by = c("station1", "station2")) %>%
  mutate(connecte = ifelse(is.na(connecte), FALSE, TRUE))

write_csv(graphe_complet, "graphe_connexions_complet.csv")

#Réduction possible ?
# Oui, il est possible de réduire la taille du fichier en supprimant toutes les lignes où les couples de stations ne sont pas connectés (connecte=FALSE)
# Ils ne sont pas nécessaires à la visualisation du graphe

#### Pour visualiser maintenant 
# On va crer un autre csv qui lie les station qui sont connecté une à une pour facilité la creation du graph dans cette partie 
df <- read.csv("qualite-de-lair-dans-le-reseau-de-transport-francilien (1).csv", sep = ";", fileEncoding = "UTF-8")

df <- df[grepl("Métro", df$Nom.de.la.ligne), ]

df_clean <- unique(df[, c("Nom.de.la.Station", "Nom.de.la.ligne")])

edges <- data.frame()

lignes <- unique(df_clean$Nom.de.la.ligne)

for (ligne in lignes) {
  stations <- unique(df_clean[df_clean$Nom.de.la.ligne == ligne, "Nom.de.la.Station"])
  if (length(stations) >= 2) {
    for (i in 1:(length(stations) - 1)) {
      edges <- rbind(edges, data.frame(
        station1 = stations[i],
        station2 = stations[i + 1],
        ligne = ligne,
        stringsAsFactors = FALSE
      ))
    }
  }
}

write.csv(edges, "graphe_metro.csv", row.names = FALSE)
# La Partie graph maintenant
library(igraph)

graph_data <- read.csv("graphe_metro.csv")

g <- graph_from_data_frame(graph_data[, c("station1", "station2")], directed = FALSE)
V(g)$media <- V(g)$name

layout_fr <- layout_with_fr(g, niter = 5000, area = vcount(g)^3)

# Export en png sinon j'arrive pas a voir dans plot le graph
png("graphe_metro_lisible.png", width = 5000, height = 5000, res = 300)

plot(g,
     layout = layout_fr,
     vertex.shape = "none",
     vertex.label = V(g)$media,
     vertex.label.font = 2,
     vertex.label.color = "black",
     vertex.label.cex = 0.6,        
     edge.color = "gray85")

dev.off()
