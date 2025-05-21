library(dplyr)

qualite <- read.table("qualite-de-lair-dans-le-reseau-de-transport-francilien (1).csv", header = TRUE, sep = ";")

qualite_clean <- qualite %>%
  filter(rowSums(is.na(.)) != ncol(.)) %>%       
  select(where(~ !all(is.na(.)))) %>%             
  distinct() 

qualite_metro <- qualite_clean %>%
  filter(grepl("Métro", Nom.de.la.ligne)) 


View(qualite_metro)

set.seed(123)  
train_indices <- sample(1:nrow(qualite_metro), size = 0.7 * nrow(qualite_metro))

train_data <- qualite_metro[train_indices, ]
test_data <- qualite_metro[-train_indices, ]

write.csv(train_data, "train.csv", row.names = FALSE)
write.csv(test_data, "test.csv", row.names = FALSE)

View(train_data)
View(test_data)
