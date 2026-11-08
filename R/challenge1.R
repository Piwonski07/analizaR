dane %>%
  select(Title, Director, Release) %>%
  filter(Director == "Woody Allen") %>%
  arrange(Release) %>% 
  head(1)