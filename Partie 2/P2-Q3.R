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

# Fonction pour vérifier l'existence d'un chemin sous un seuil de pollution
chemin_pollution_max <- function(df_metro, seuil_pollution, station1, station2) {
  pollution_levels <- c("FAIBLE", "MOYENNE", "ELEVE")
  seuil_index <- match(seuil_pollution, pollution_levels)
  
  if (is.na(seuil_index)) {
    stop("Seuil de pollution invalide.")
  }
  
  # Filtrer les stations admissibles au seuil
  df_filtre <- df_metro %>%
    filter(pollution %in% pollution_levels[1:seuil_index])
  
  # Création des liaisons comme dans la question 2
  liaisons_filtrees <- df_filtre %>%
    group_by(ligne) %>%
    do({
      d <- .
      couples <- expand_grid(station1 = d$station, station2 = d$station) %>%
        filter(station1 != station2)
      
      couples <- couples %>%
        left_join(d, by = c("station1" = "station")) %>%
        rename(lon1 = lon, lat1 = lat) %>%
        left_join(d, by = c("station2" = "station")) %>%
        rename(lon2 = lon, lat2 = lat)
      
      couples <- couples %>%
        mutate(distance = sqrt((lon1 - lon2)^2 + (lat1 - lat2)^2)) %>%
        group_by(station1) %>%
        slice_min(order_by = distance, n = 1) %>%
        ungroup()
    })
  
  # Créer un graphe filtré
  g_filtre <- graph_from_data_frame(liaisons_filtrees %>% select(from = station1, to = station2), directed = FALSE)
  
  noms_stations <- V(g_filtre)$name
  station1 <- trimws(station1)
  station2 <- trimws(station2)
  
  if (!(station1 %in% noms_stations)) {
    message("Station de départ inaccessible sous ce seuil de pollution.")
    return(NULL)
  }
  if (!(station2 %in% noms_stations)) {
    message("Station d'arrivée inaccessible sous ce seuil de pollution.")
    return(NULL)
  }
  
  chemin <- suppressWarnings(shortest_paths(g_filtre, from = station1, to = station2)$vpath[[1]])
  
  if (length(chemin) > 0) {
    message("Chemin existant sous le seuil de pollution ", seuil_pollution, ":")
    print(names(chemin))
  } else {
    message("Aucun chemin trouvé sous ce seuil de pollution.")
  }
}

# Exemple d'utilisation :
chemin_pollution_max(df_metro, seuil_pollution = "MOYENNE", station1 = "Nation", station2 = "Reuilly - Diderot")
chemin_pollution_max(df_metro, seuil_pollution = "FAIBLE", station1 = "Nation", station2 = "Reuilly - Diderot")

