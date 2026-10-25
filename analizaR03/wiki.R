#skrypt wiczki 

library(haven)
adres_cbos <- paste0("https://raw.githubusercontent.com/kflisikowsky/sad/",
                     "main/data/CBOS_201733_331_SPSS.sav")
library(dplyr)
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
head(cbos)
# ZADANIE 5
#Sprawdź liczbę obserwacji i braków. Przygotuj tabelę liczności i odsetków dla 
#`plec` oraz `miejscowosc`. Jaki odsetek ankietowanych mieszkał na wsi, a jaki w
#miastach liczących co najmniej 500 tys. mieszkańców? Licz odsetki względem 
#liczby **niebrakujących odpowiedzi** w danej zmiennej i podaj użyty mianownik.

#liczba obserwacji 
nrow(cbos)
#braki w każdej zmiennej 
colSums(is.na(cbos))

#2. tabela liczebności i odsetków dla plci
n_plec <- sum(!is.na(cbos$plec))
n_plec

tab_plec <- cbos%>%
  filter(!is.na(plec))%>%
  count(plec)%>%
  mutate(odsetek = round(100*n/sum(n), 2))
tab_plec

# 3. Tabela liczności i odsetków dla miejscowosci
n_miej <- sum(!is.na(cbos$miejscowosc))   # mianownik
n_miej

tab_miej <- cbos %>%
  filter(!is.na(miejscowosc)) %>%
  count(miejscowosc) %>%
  mutate(odsetek = round(100 * n / sum(n), 2))
tab_miej

# 4. Odsetek: wieś oraz miasta >= 500 tys.
levels(cbos$miejscowosc)   # sprawdź dokładne etykiety

tab_miej %>% filter(grepl("wie", miejscowosc, ignore.case = TRUE))
tab_miej %>% filter(grepl("500", miejscowosc))

install.packages("usethis")

#test 
head(cbos)

