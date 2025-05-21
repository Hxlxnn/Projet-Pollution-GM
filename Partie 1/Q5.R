library(dplyr)
library(class)
library(ggplot2)

qualite <- read.table("qualite-de-lair-dans-le-reseau-de-transport-francilien (1).csv", 
                      header = TRUE, sep = ";", quote = "\"")

qualite_clean <- qualite %>%
  filter(rowSums(is.na(.)) != ncol(.)) %>%
  select(where(~ !all(is.na(.)))) %>%
  distinct()

qualite_metro <- qualite_clean %>%
  filter(grepl("Métro", Nom.de.la.ligne)) %>%
  select(niveau_pollution, stop_lat, stop_lon) %>%
  na.omit() %>%
  filter(!(niveau_pollution %in% c("pas de données", "station aérienne")))

qualite_metro$niveau_pollution <- as.factor(qualite_metro$niveau_pollution)

set.seed(123)
train_indices <- sample(1:nrow(qualite_metro), size = 0.7 * nrow(qualite_metro))
train_data <- qualite_metro[train_indices, ]
test_data <- qualite_metro[-train_indices, ]

k_value <- 5
knn_pred <- knn(train = train_data[, c("stop_lat", "stop_lon")],
                test = test_data[, c("stop_lat", "stop_lon")],
                cl = train_data$niveau_pollution,
                k = k_value)

accuracy <- sum(diag(conf_mat)) / length(test_data$niveau_pollution)
sprintf("Accuracy: %.2f%%", accuracy * 100)

conf_mat <- table(
  Predicted = factor(knn_pred, levels = levels(qualite_metro$niveau_pollution)),
  Actual = factor(test_data$niveau_pollution, levels = levels(qualite_metro$niveau_pollution))
)
print(conf_mat)

conf_mat_df <- as.data.frame(conf_mat)
ggplot(conf_mat_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), color = "black", size = 5) +
  scale_fill_gradient(low = "lightblue", high = "red") +
  labs(
    title = "Matrice de confusion KNN (sans classes inutiles)",
    x = "Vrai niveau de pollution",
    y = "Niveau prédit"
  ) +
  theme_minimal()
