library(dplyr)
library(readr)
library(tidyr)

data <- read_delim("qualite-de-lair-dans-le-reseau-de-transport-francilien (1).csv",
                   delim = ";", locale = locale(encoding = "UTF-8"))

data_metro <- data %>%
  filter(grepl("Métro", `Nom de la ligne`)) %>%
  distinct(station = `Nom de la Station`, ligne = `Nom de la ligne`) %>%
  filter(!is.na(station), !is.na(ligne))

stations_uniques <- data_metro %>% distinct(station) %>% arrange(station)

couples_possibles <- expand_grid(
  station1 = stations_uniques$station,
  station2 = stations_uniques$station
) %>%
  filter(station1 < station2)

data_liaisons <- data_metro %>%
  inner_join(data_metro, by = "ligne") %>%
  filter(station.x < station.y) %>%
  distinct(station1 = station.x, station2 = station.y) %>%
  mutate(connecte = TRUE)

graph_complet <- couples_possibles %>%
  left_join(data_liaisons, by = c("station1", "station2")) %>%
  mutate(connecte = ifelse(is.na(connecte), FALSE, TRUE))

write_csv(graph_complet, "graphe_connexions_complet.csv")


# Visualisation
library(igraph)

data_graph <- edges[, c("station1", "station2")]
graph <- graph_from_data_frame(data_graph, directed = FALSE)


disposition <- layout_with_fr(graph, niter = 5000, area = vcount(graph)^3)

png("graphe_metro_lisible.png", width = 5000, height = 5000, res = 300)

plot(graph,
     layout = disposition,
     vertex.shape = "none",
     vertex.label = V(graph)$name,
     vertex.label.font = 2,
     vertex.label.color = "black",
     vertex.label.cex = 0.6,
     edge.color = "red")

dev.off()
