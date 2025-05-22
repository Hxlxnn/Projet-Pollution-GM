library(dplyr)
library(readr)
library(tidyr)

df <- read_delim("qualite-de-lair-dans-le-reseau-de-transport-francilien (1).csv",
                 delim = ";", locale = locale(encoding = "UTF-8"))

# On garde uniquement les lignes qui concernent des stations de métro
df_metro <- df %>%
  filter(grepl("Métro", `Nom de la ligne`)) %>%
  distinct(station = `Nom de la Station`, ligne = `Nom de la ligne`) %>%
  filter(!is.na(station), !is.na(ligne))

# On génère tous les couples possibles de stations de métro
stations_uniques <- df_metro %>% distinct(station) %>% arrange(station)


couples_possibles <- expand_grid(
  station1 = stations_uniques$station,
  station2 = stations_uniques$station
) %>%
  filter(station1 < station2)  # Éviter doublons et auto-connections

# On identifie les couples qui sont connectés (appartiennet à une même ligne)
df_liaisons <- df_metro %>%
  inner_join(df_metro, by = "ligne") %>%
  filter(station.x < station.y) %>%
  distinct(station1 = station.x, station2 = station.y) %>%
  mutate(connecte = TRUE) #On leur donne une valeur connecte=TRUE

# Joindre avec tous les couples possibles pour attribuer connecte=TRUE ou FALSE
graphe_complet <- couples_possibles %>%
  left_join(df_liaisons, by = c("station1", "station2")) %>%
  mutate(connecte = ifelse(is.na(connecte), FALSE, TRUE))

# On crée le fichier csv du graphe complet (avec toutes les couples possibles)
write_csv(graphe_complet, "graphe_connexions_complet.csv")

library(igraph)
g_complet <- graph_from_data_frame(graphe_complet, directed = FALSE)


# Taille : le nombre total de lignes du fichier = nombre de paires de stations distinctes
cat("Nombres de lignes du fichier:", nrow(graphe_complet), "lignes\n")

#Réduction possible ?
# Oui, il est possible de réduire la taille du fichier en supprimant toutes les lignes où les couples de stations ne sont pas connectés (connecte=FALSE)
# Ils ne sont pas nécessaires à la visualisation du graphe

#### Visualisation du graphe
install.packages("ggraph")
install.packages("tidygraph")
library(ggraph)
library(tidygraph)

# Nettoyer le graphe en ne gardant que les connexions valides (connecte=TRUE)
g_connecte <- delete_edges(g_complet, E(g_complet)[which(!E(g_complet)$connecte)])

# Convertir en tidygraph pour ggraph
g_tidy <- as_tbl_graph(g_connecte)
ggraph(g_tidy, layout = "fr") +  # "fr" pour layout de Fruchterman-Reingold
  geom_edge_link(alpha = 0.4, colour = "steelblue") +
  geom_node_point(color = "darkred", size = 2) +
  geom_node_text(aes(label = name), repel = TRUE, size = 2) +
  theme_void() +
  ggtitle("Graphe complet des connexions entre stations de métro")
