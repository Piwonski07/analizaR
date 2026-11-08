#Jakie są średnie oceny filmów wyprodukowanych w poszczególnych dekadach (tzn. lata 60, 70, 80, 90 etc.)?
dane <- dane%>%
  mutate(
    year = as.numeric(format(Release, "%Y")),
    decade = floor(year/10)*10,
    decade_label = paste0(decade, "-", decade + 9)
  )

dane%>%
  filter(Series.or.Movie == "Movie", !is.na(decade)) %>%
  group_by(decade_label) %>%
  summarise(
    avg_imdb=round(mean(IMDb.Score, na.rm = TRUE), 2), 
    n_filmow=n()
  ) %>%
  arrange(decade_label)
