library(glue)
sredni_czas <- dane %>%
  filter(year(Release) %in% c(2019, 2020)) %>%
  summarise(
    sredni_czas = mean(
      as.numeric(Netflix.Release-Release)
    )
  )

glue("sredni czas wynosi {round(sredni_czas)} dni.")
