library(tidyquant)
library(tidyverse)

# unemp_raw <- tq_get(
#   c("UNRATE", "ORUR", "CAUR", "TXUR", "NYUR"),
#   get  = "economic.data",
#   from = "1976-01-01"
# )
# 
# head(unemp_raw)
# 
# dir.create("data", showWarnings = FALSE)
# 
# write_rds(unemp_raw, "data/unemp_raw.rds")

unemp_raw = read_rds("data/unemp_raw.rds")

unemp <- unemp_raw |>
  rename(unemployment_rate = price) |>
  mutate(
    region = case_when(
      symbol == "UNRATE" ~ "United States",
      symbol == "ORUR" ~ "Oregon",
      symbol == "CAUR" ~ "California",
      symbol == "TXUR" ~ "Texas",
      symbol == "NYUR" ~ "New York"
    )
  )

unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(
    unemployment_rate = replace(
      unemployment_rate,
      date == as.Date("2025-10-01"),
      unemployment_rate[date == as.Date("2025-09-01")]
    )
  ) |>
  ungroup()

unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(unemployment_rate_change = unemployment_rate - lag(unemployment_rate)) |>
  ungroup()

unemp_recent <- unemp |>
  filter(date >= "2015-01-01")

unemp_summary <- unemp |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate  = min(unemployment_rate,  na.rm = TRUE),
    max_rate  = max(unemployment_rate,  na.rm = TRUE)
  )

unemp_recent_summary <- unemp_recent |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate  = min(unemployment_rate,  na.rm = TRUE),
    max_rate  = max(unemployment_rate,  na.rm = TRUE)
  )

unemp_top_increases <- slice_max(unemp, unemployment_rate_change, n = 5)
unemp_top_decreases <- slice_min(unemp, unemployment_rate_change, n = 5)

unemp_top_increases_no_covid <- slice_max(
  filter(unemp, lubridate::year(date) != 2020), unemployment_rate_change, n = 5)
unemp_top_decreases_no_covid  <- slice_min(
  filter(unemp, lubridate::year(date) != 2020), unemployment_rate_change, n = 5)

ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +
  labs(title = "Unemployment Rates by Region", x = "Date", y = "Unemployment Rate") +
  theme_minimal()

ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  facet_wrap(~ region) +
  geom_line() +
  labs(title = "Unemployment Rates by Region", x = "Date", y = "Unemployment Rate") +
  theme_minimal()

ggplot(unemp_summary, aes(x = fct_reorder(region, mean_rate), y = mean_rate, color = region)) +
  geom_col() +
  labs(title = "Mean Unemployment Rate by Region", x = "Region", 
       y = "Mean Unemployment Rate") +
  theme_minimal()

ggplot(
  bind_rows(unemp_top_increases_no_covid, unemp_top_decreases_no_covid), aes(x = date, y = unemployment_rate_change, color = region)) +
  geom_point() +
  labs(title = "Top Increases and Decreases in Unemployment Rate", x = "Date", 
       y = "Change in Unemployment Rate") +
  theme_minimal()
#I plotted the top increases and decreases in the unemployment rate excluding Covid. 
#I was curious to see which regions in which years were the most extreme changes in the unemployment rate.

