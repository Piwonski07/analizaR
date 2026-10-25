# =============================================================
# Raport 3 — CBOS: porównania grup (zadania 7–8)
# Plik: R/cbos_porownania.R   (osoba C)
# Skrypt uruchamia się od zera: pakiety, odczyt danych, analizy.
# =============================================================

library(dplyr)
library(haven)

# ---- Wczytanie danych (jak w instrukcji) --------------------
adres_cbos <- paste0("https://raw.githubusercontent.com/kflisikowsky/sad/",
                     "main/data/CBOS_201733_331_SPSS.sav")
plik_cbos <- tempfile(fileext = ".sav")
download.file(adres_cbos, plik_cbos, mode = "wb", quiet = TRUE)
cbos_raw <- read_sav(plik_cbos)
unlink(plik_cbos) 

cbos <- cbos_raw %>% transmute(
  plec = as_factor(plec),
  miejscowosc = as_factor(klm6),
  kierunek = as_factor(q24),
  prognoza = as_factor(q25),
  wyksztalcenie = as_factor(q15)
)
dim(cbos)

# Podgląd dokładnych etykiet kategorii (pisownia musi się zgadzać w filter)
levels(cbos$miejscowosc)
levels(cbos$kierunek)
levels(cbos$prognoza)


# =============================================================
# Zadanie 7 — wieś i duże miasta
# =============================================================

grupy <- c("wieś", "miasto 500 000 i więcej")

# Kontrola: czy obie etykiety faktycznie występują w danych
stopifnot(all(grupy %in% levels(cbos$miejscowosc)))

cbos_7 <- cbos %>% filter(miejscowosc %in% grupy)

# Liczba respondentów w obu grupach
liczebnosci_7 <- cbos_7 %>% count(miejscowosc, name = "n_grupa")
liczebnosci_7

# Procent odpowiedzi na `kierunek` W OBRĘBIE każdej grupy
zad7 <- cbos_7 %>%
  count(miejscowosc, kierunek) %>%
  group_by(miejscowosc) %>%
  mutate(
    n_grupa = sum(n),
    procent = round(100 * n / n_grupa, 1)
  ) %>%
  ungroup()
zad7

# Kontrola: procenty w każdej grupie sumują się do 100
zad7 %>% group_by(miejscowosc) %>% summarise(suma_proc = sum(procent))

# Tylko odpowiedź „W złym” — do bezpośredniego porównania
zad7 %>% filter(kierunek == "W złym")

# Wniosek do wnioski.md (uzupełnij liczbami z wyniku powyżej):
# Odpowiedź „W złym” wybrało X% mieszkańców wsi (n = ...) oraz Y% mieszkańców
# miast 500 tys.+ (n = ...); mianownikiem jest liczba respondentów w danej grupie.
# Grupa dużych miast jest znacznie mniejsza, więc jej odsetek jest mniej stabilny
# — zmiana odpowiedzi kilku osób przesuwa go o kilka punktów procentowych.


# =============================================================
# Zadanie 8 — ocena teraźniejszości a oczekiwania
# =============================================================

# Tabela liczności odpowiedzi na `prognoza` (cała próba)
zad8_calosc <- cbos %>%
  count(prognoza) %>%
  mutate(procent = round(100 * n / sum(n), 1))
zad8_calosc

# Tylko osoby oceniające kierunek zmian jako „W złym”
stopifnot("W złym" %in% levels(cbos$kierunek))

zad8_w_zlym <- cbos %>%
  filter(kierunek == "W złym") %>%
  count(prognoza, sort = TRUE) %>%
  mutate(procent = round(100 * n / sum(n), 1))
zad8_w_zlym

# Najczęstsza odpowiedź w tej grupie
zad8_w_zlym %>% slice_head(n = 1)

# Wniosek do wnioski.md:
# Wśród osób oceniających kierunek zmian jako „W złym” (n = ...) najczęstszą
# prognozą była „...” (Z%). Tabela nie pozwala na wnioski przyczynowe: to jedno
# badanie przekrojowe, pokazuje tylko współwystępowanie odpowiedzi. Obie oceny
# mogą wynikać ze wspólnych czynników (np. poglądów politycznych, sytuacji
# materialnej, wieku). Dane z 2017 r., liczone na surowych rekordach bez wag.
