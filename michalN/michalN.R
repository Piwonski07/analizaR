##Dane nieruchomosci
library(dplyr)
library(tidyr)

adres_nieruchomosci <- paste0(
  "https://raw.githubusercontent.com/kflisikowsky/pandas_exercises/",
  "62863036107888c0c7273c40e8d309a43f4da3a9/data/models/real_estate.csv"
)
raw_nier <- read.csv(adres_nieruchomosci, check.names = FALSE)
stopifnot(nrow(raw_nier) == 414, ncol(raw_nier) == 7)

nier <- raw_nier
names(nier) <- c("id", "wiek_lat", "metro_m", "sklepy", "lat", "lon",
                 "cena_10k_twd_ping")
head(nier)

#zadanie 1
# WASZE ROZWIĄZANIE
# dim(), str(), summary(), colSums(is.na(...)), anyDuplicated()
# 1. Liczba rekordów (wierszy) i kolumn

# 1. Liczba rekordów i kolumn
dim(nier)

# 2. Typy danych
str(nier)

# 3. Liczba brakujących wartości w każdej kolumnie
colSums(is.na(nier))

# 4. Podstawowe statystyki opisowe
summary(nier)

# 5. Sprawdzenie, czy id jest unikatowe
anyDuplicated(nier$id)

# 6. Sprawdzenie braków w id
sum(is.na(nier$id))

# 7. Wyświetlenie zmiennych liczbowych
names(nier)[sapply(nier, is.numeric)]
