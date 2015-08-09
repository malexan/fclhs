ids <- data %>%
  select(hs6, fcl) %>%
  distinct() %>%
  mutate(id = row_number())

data <- left_join(data, ids)

data %>%
  group_by(id) %>%
  summarize(n = n()) %>%
  top_n(10) %>%
  arrange(desc(n))

