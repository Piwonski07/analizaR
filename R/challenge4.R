library(stringr)

dane2 <- dane %>%
  mutate(
    for_pl = str_detect(dane$Country.Availability, "Poland")
  ) %>% 
  filter(for_pl == TRUE)

tagi <- dane2 %>%
  separate_rows(Tags, sep = ",\\s*") %>% 
  mutate(Tags = str_trim(Tags)) %>%
  count(Tags, sort = TRUE) %>%
  as.data.frame()

print("Najpopularniejsze tagi dla filmow z PL")
print(head(tagi, 10))


