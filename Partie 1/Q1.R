library(dplyr)

qualite <- read.table("qualite-de-lair-dans-le-reseau-de-transport-francilien (1).csv", header = TRUE, sep = ";", quote = "\"")


qualite_clean <- qualite %>%
  filter(rowSums(is.na(.)) != ncol(.)) %>%       
  select(where(~ !all(is.na(.)))) %>%             
  distinct()             
View(qualite_clean)
