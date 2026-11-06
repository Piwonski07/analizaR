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


### Zadanie 2 — przelicz cenę i sprawdź jej rozkład
##Dodaj kolumnę `cena_twd_m2`: `cena_10k_twd_ping * 10000 / 3.3`. Oblicz minimum, medianę, średnią i maksimum ceny za m². Wyświetl pięć nieruchomości o najwyższej cenie jednostkowej wraz z ich wiekiem, odległością od metra i liczbą sklepów.
##Pytanie: czy średnia jest wyższa od mediany? Co to mówi o rozkładzie cen? Nie interpretuj tej ceny jako wartości całego lokalu.

#a) dodatnie kolumny z ceną za m2
nier <- nier %>%
  mutate(cena_twd_m2 = cena_10k_twd_ping * 10000 / 3.3)
#sprawdzenie wyniku
head(nier)
##b) podstawowe statystyki ceny za m2
nier %>%
  summarise(
    minimum = min(cena_twd_m2, na.rm = TRUE),
    mediana = median(cena_twd_m2, na.rm = TRUE),
    srednia = mean(cena_twd_m2, na.rm = TRUE),
    maksimum = max(cena_twd_m2, na.rm = TRUE)
  )
#c) Pięc nieruchomości o najwyższej cenie za m2
nier %>%
  arrange(desc(cena_twd_m2)) %>%
  select(id, cena_twd_m2, wiek_lat, metro_m, sklepy) %>%
  slice_head(n=5)
#d) Odp na pytanie:Średnia jest nieznacznie niższa od mediany, co może sugerować niewielką asymetrię lewostronną rozkładu cen. Różnica jest jednak niewielka, dlatego do potwierdzenia skośności konieczna byłaby dalsza analiza rozkładu.


### Zadanie 3 — ile obserwacji jest blisko metra?
##Policz, ile nieruchomości znajduje się w odległości do 500 m od stacji oraz jaki to odsetek wszystkich rekordów. Następnie porównaj mediany ceny TWD/m² w dwóch grupach: **do 500 m** i **powyżej 500 m**.
##Wynik opisz jednym zdaniem z liczbami i jednostkami. To proste porównanie opisowe, a nie dowód, że odległość powoduje zmianę ceny.

#a podzial nieruchomosci na dwie grupy
nier <- nier %>%
  mutate(blisko_metra = metro_m <= 500)

#b liczba nieruchomosci i mediana cen w obu grupach, TRUE- nieru znajduje sie max 500m od metra, FALSE nieru znajduje sie dalej niż 500m od metra
nier %>%
  group_by(blisko_metra) %>%
  summarise(
    liczba = n(),
    mediana = median(cena_twd_m2, na.rm = TRUE)
  ) %>%
  mutate(
    odsetek = liczba / sum(liczba) * 100
  )
#c WynikL Spośród 414 analizowanych nieruchomości 211 (51,0%) znajduje się w odległości do 500 m od stacji metra, a mediana ich ceny wynosi 135 758 TWD/m², wobec 85 152 TWD/m² dla 203 nieruchomości położonych dalej niż 500 m, co oznacza różnicę 50 606 TWD/m² na korzyść lokalizacji bliżej metra.

##### Zadanie 4 — wiek, odległość i sklepy
#Podziel obserwacje na trzy strefy odległości od metra: **do 500 m**, **powyżej 500 do 1500 m**, **powyżej 1500 m**. Dla każdej strefy oblicz liczbę obserwacji, medianę ceny TWD/m², medianę wieku i średnią liczbę sklepów.
#Następnie sprawdź, czy wśród budynków w wieku najwyżej 20 lat ranking median stref pozostaje taki sam. Dlaczego porównanie wszystkich budynków może różnić się od porównania budynków młodszych?

#a) podzial na trzy strefy odleglosci
nier <- nier%>%
  mutate(
    strefa = cut(metro_m, c(-Inf, 500, 1500, Inf),
                 labels = c("do 500 m", "500-1500 m", "ponad 1500 m"))
  )

#b) Statystyki dla kazdej strefy
nier %>%
  group_by(strefa) %>%
  summarise(
    liczba = n(),
    mediana_ceny = median(cena_twd_m2, na.rm = TRUE),
    mediana_wieku = median(wiek_lat, na.rm = TRUE),
    srednia_sklepow = mean(sklepy, na.rm = TRUE)
  )%>%
  arrange(desc(mediana_ceny))

#c Statystyki dla budynkow w wieku do 20 lat
nier %>%
  filter(wiek_lat <= 20) %>%
  group_by(strefa) %>%
  summarise(
    liczba = n(),
    mediana_ceny = median(cena_twd_m2, na.rm = TRUE),
    mediana_wieku = median(wiek_lat, na.rm = TRUE),
    srednia_sklepow = mean(sklepy, na.rm = TRUE)
  ) %>%
  arrange(desc(mediana_ceny))
## wynik : Analiza wykazała, że zarówno dla wszystkich nieruchomości, jak i budynków w wieku do 20 lat, najwyższa mediana ceny za m² występuje w strefie do 500 m od metra, a najniższa w strefie powyżej 1500 m. Ranking median pozostał więc niezmieniony. Po ograniczeniu analizy do młodszych budynków mediany cen wzrosły we wszystkich trzech strefach. Różnice między wynikami mogą wynikać z odmiennej struktury wieku nieruchomości, ponieważ wiek jest jednym z czynników potencjalnie związanych z ceną. Analiza ma charakter opisowy i nie dowodzi, że odległość od metra bezpośrednio wpływa na cenę nieruchomości.