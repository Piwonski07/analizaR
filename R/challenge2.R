# CHALLENGE 2: Jakie są trzy najwyżej oceniane komedie dostępne w języku polskim?

library(tidyverse)

if (!exists("dane")) dane <- read.csv("dane.csv", encoding = "UTF-8")

dane %>%
  filter(str_detect(Genre, "Comedy"),
         str_detect(Languages, "Polish")) %>%
  arrange(desc(IMDb.Score), desc(IMDb.Votes)) %>%   
  select(Title, Series.or.Movie, Genre, IMDb.Score, IMDb.Votes) %>%
  head(3)