library(dplyr)

qualite <- read.table("qualite-de-lair-dans-le-reseau-de-transport-francilien (1).csv", header = TRUE, sep = ";")
head(qualite)

qualite_clean <- qualite %>%
  filter(rowSums(is.na(.)) != ncol(.)) %>%       
  select(where(~ !all(is.na(.)))) %>%             
  distinct() 

qualite_metro <- qualite_clean %>%
  filter(grepl("Métro", Nom.de.la.ligne))  # On filtre les stations de métro


View(qualite_metro)

# Diviser le dataset en 70 % pour l'entraînement et 30 % pour le test
set.seed(123)  # Pour reproduire l'échantillonnage aléatoire
train_indices <- sample(1:nrow(qualite_metro), size = 0.7 * nrow(qualite_metro))

# Créer le jeu d'entraînement (train) et le jeu de test (test)
train_data <- qualite_metro[train_indices, ]
test_data <- qualite_metro[-train_indices, ]

# Sauvegarder les datasets dans des fichiers CSV
write.csv(train_data, "train.csv", row.names = FALSE)
write.csv(test_data, "test.csv", row.names = FALSE)

#View(train_data)
#View(test_data)


