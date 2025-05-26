library(dplyr)
library(readr)
library(igraph)
library(tidyr)

fichier <- "qualite-de-lair-dans-le-reseau-de-transport-francilien (1).csv"
df <- read_delim(fichier, delim = ";", locale = locale(encoding = "UTF-8"))

# Nettoyage et filtrage des données métro
df_metro <- df %>%
  filter(grepl("Métro", `Nom de la ligne`)) %>%
  mutate(
    station = trimws(`Nom de la Station`),
    ligne = `Nom de la ligne`,
    lon = stop_lon,
    lat = stop_lat,
    pollution = `Niveau de pollution`
  ) %>%
  filter(!is.na(station), !is.na(ligne), !is.na(lon), !is.na(lat), !is.na(pollution))

# Fonction pour convertir les niveaux de pollution en scores
pollution_score <- function(p) {
  case_when(
    p == "FAIBLE" ~ 1,
    p == "MOYENNE" ~ 2,
    p == "ELEVE" ~ 3,
    p == "pas de données" ~ 1,
    p == "station aérienne" ~ 1,
    TRUE ~ NA_real_
  )
}

# Initialiser un tableau de liaisons entre stations proches sur la même ligne
liaisons <- df_metro %>%
  group_by(ligne) %>%
  do({
    d <- .
    couples <- expand_grid(station1 = d$station, station2 = d$station) %>% # On effectue tous les couples de stations possibles
      filter(station1 != station2) # On évite de lier deux mêmes stations
    
    couples <- couples %>%
      left_join(d, by = c("station1" = "station")) %>%
      rename(lon1 = lon, lat1 = lat, pol1 = pollution) %>%
      left_join(d, by = c("station2" = "station")) %>%
      rename(lon2 = lon, lat2 = lat, pol2 = pollution)
    
    couples <- couples %>%
      mutate(distance = sqrt((lon1 - lon2)^2 + (lat1 - lat2)^2)) %>% # Calcul de la distance entre stations
      group_by(station1) %>%
      slice_min(order_by = distance, n = 1) %>%
      ungroup()
  })

# Ajouter un poids de pollution
liaisons <- liaisons %>%
  mutate(
    pollution_val = pollution_score(pol1),
    cost = distance * pollution_val
  ) %>%
  filter(!is.na(cost), !is.nan(cost), !is.infinite(cost))

# Créer le graphe pondéré
g <- graph_from_data_frame(liaisons %>% select(from = station1, to = station2, weight = cost), directed = FALSE)

# Fonction pour trouver le chemin optimal
chemin_optimal <- function(graph, station_depart, station_arrivee) {
  sp <- shortest_paths(graph, from = station_depart, to = station_arrivee, weights = E(graph)$weight, output = "both") # Le chemin le plus court par rapport au facteur de poids 
  list(
    chemin = names(sp$vpath[[1]]), # Liste des stations du chemin
    poids_total = sum(E(graph, path = sp$vpath[[1]])$weight) # Somme des poids des stations du chemin
  )
}

# Exemple d'utilisation 
resultat <- chemin_optimal(g, "Trocadéro", "Aéroport d'Orly")
print(resultat$chemin)
cat("Coût total (pollution x distance) :", resultat$poids_total, "\n")
