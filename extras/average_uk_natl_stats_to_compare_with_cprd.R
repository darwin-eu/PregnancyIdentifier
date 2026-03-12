library(dplyr)
# average the UK national stats for crude comparison with CPRD (Mode of delivery rate (% vaginal and c-section deliveries))
lines <- c(
  "UK-NI: 69.9","UK-S: 67.6","UK-W: 74.1",
  "UK-NI: 30.2","UK-S: 32.4","UK-W: 25.9",
  "UK-NI: 67.3","UK-S: 64.5","UK-W: 71.7",
  "UK-NI: 32.7","UK-S: 35.5","UK-W: 28.3"
)

tibble(raw = lines) %>%
  tidyr::separate(raw, into = c("region", "value"), sep = ": ") %>%
  mutate(
    value = as.numeric(value),
    group = ceiling(row_number() / 3)
  ) %>%
  group_by(group) %>%
  summarise(mean_value = mean(value), .groups = "drop")
