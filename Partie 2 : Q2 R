library(igraph)
library(dplyr)
library(readr)

# Lecture du fichier pollution
df <- read_delim("qualite-de-lair-dans-le-reseau-de-transport-francilien.csv",
                 delim = ";", locale = locale(encoding = "UTF-8"))

# Nettoyage et encodage pollution
df_metro <- df %>%
  filter(grepl("Métro", `Nom de la ligne`)) %>%
  select(`Nom de la Station`, `Nom de la ligne`, `Niveau de pollution`) %>%
  mutate(pollution_score = case_when(
    grepl("FAIBLE", `Niveau de pollution`, ignore.case = TRUE) ~ 1,
    grepl("MOYENNE", `Niveau de pollution`, ignore.case = TRUE) ~ 2,
    grepl("FORTE", `Niveau de pollution`, ignore.case = TRUE) ~ 3,
    TRUE ~ 2  # défaut si inconnu
  )) %>%
  distinct()

# Génération des connexions station1 -> station2
graph_edges <- df_metro %>%
  group_by(`Nom de la ligne`) %>%
  arrange(`Nom de la Station`) %>%
  mutate(station1 = lag(`Nom de la Station`),
         station2 = `Nom de la Station`,
         pollution = pollution_score) %>%
  filter(!is.na(station1)) %>%
  mutate(temps = 2) %>%
  select(station1, station2, pollution, temps)

# Graphe orienté pondéré
g <- graph_from_data_frame(graph_edges, directed = TRUE)

# Entrées qu'on DOIT changer pour tester 
station_depart <- "Concorde"
station_arrivee <- "Nation"
temps_max <- 20  # exemple : contrainte de 20 minutes

# Liste de tous les chemins simples entre 2 stations
chemins_possibles <- all_simple_paths(g, from = station_depart, to = station_arrivee)

# Fonction pour évaluer un chemin
eval_chemin <- function(path) {
  stations <- names(path)
  arêtes <- data.frame(from = stations[-length(stations)],
                       to = stations[-1])

  pollution_total <- 0
  temps_total <- 0
  for (i in 1:nrow(arêtes)) {
    edge_id <- get.edge.ids(g, c(arêtes$from[i], arêtes$to[i]))
    pollution_total <- pollution_total + E(g)[edge_id]$pollution
    temps_total <- temps_total + E(g)[edge_id]$temps
  }

  return(data.frame(path = paste(stations, collapse = " -> "),
                    pollution = pollution_total,
                    temps = temps_total))
}

# Appliquer à tous les chemins et filtrer selon la contrainte de temps
resultats <- do.call(rbind, lapply(chemins_possibles, eval_chemin))
chemin_optimal <- resultats %>%
  filter(temps <= temps_max) %>%
  arrange(pollution) %>%
  slice(1)

print(chemin_optimal)
