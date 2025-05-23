## Read data directly from Statistics Greenland website using 'pxweb'

library(pxweb)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)

max_age <- 102
min_time <- 2020

## Obtain data

pxweb_query_list <- list(
  "sex" = c("F", "M"),
  "place of birth" = "T",
  "year of birth" = as.character(1875:2023),
  "triangles(lexis)" = c("0", "1", "9"),
  "event" = c("P", "I", "O", "D", "U", "B", "C"),
  "time" = as.character(min_time:2023)
)

px_data <-
  pxweb_get(
    url = "https://bank.stat.gl/api/v1/en/Greenland/BE/BE80/BE8020/BEXCALC2.px",
    query = pxweb_query_list
  )

px_data_frame <- as.data.frame(px_data,
  column.name.type = "text",
  variable.value.type = "text"
)

## Tidy
data <- px_data_frame %>%
  select(sex,
    cohort = "year of birth",
    triangle = "triangles(lexis)",
    event,
    time,
    count = "Population Accounts"
  ) %>%
  tibble() %>%
  filter(!is.na(count)) %>%
  mutate(
    cohort = as.integer(cohort),
    time = as.integer(time)
  )


## Reported values

popn_start <- data %>%
  filter((event == "Population (start of year)") &
    (time == min(time))) %>%
  mutate(time = time - 1) %>%
  select(-event)
popn_end <- data %>%
  filter(event == "Population (end of year)") %>%
  select(-event)
gl_report_popn <- bind_rows(popn_start, popn_end) %>%
  filter(triangle == "Lexis not applicable") %>%
  mutate(age = time - cohort) %>%
  filter((0 <= age) & (age <= max_age)) %>%
  count(age, sex, time, wt = count, name = "count")
save(gl_report_popn,
  file = "../data/gl_report_popn.rda",
  compress = "bzip2"
)

gl_report_deaths <- data %>%
  filter(
    event == "Death",
    triangle != "Lexis not applicable"
  ) %>%
  select(triangle, sex, cohort, time, count) %>%
  complete(triangle, sex, cohort, time, fill = list(count = 0L)) %>%
  mutate(age = time - cohort - (triangle == "Upper Lexis (before birthday)")) %>%
  filter((0 <= age) & (age <= max_age)) %>%
  count(age, sex, cohort, time, wt = count, name = "count")
save(gl_report_deaths,
  file = "../data/gl_report_deaths.rda",
  compress = "bzip2"
)

gl_report_immig <- data %>%
  filter(
    event == "Immigration",
    triangle != "Lexis not applicable"
  ) %>%
  select(triangle, sex, cohort, time, count) %>%
  complete(triangle, sex, cohort, time, fill = list(count = 0L)) %>%
  mutate(age = time - cohort - (triangle == "Upper Lexis (before birthday)")) %>%
  filter((0 <= age) & (age <= max_age)) %>%
  count(age, sex, cohort, time, wt = count, name = "count") %>%
  mutate(count = ifelse(time == 1992, NA, count))
save(gl_report_immig,
  file = "../data/gl_report_immig.rda",
  compress = "bzip2"
)

gl_report_emig <- data %>%
  filter(
    event == "Emigration",
    triangle != "Lexis not applicable"
  ) %>%
  select(triangle, sex, cohort, time, count) %>%
  complete(triangle, sex, cohort, time, fill = list(count = 0L)) %>%
  mutate(age = time - cohort - (triangle == "Upper Lexis (before birthday)")) %>%
  filter((0 <= age) & (age <= max_age)) %>%
  count(age, sex, cohort, time, wt = count, name = "count") %>%
  mutate(count = ifelse(time == 1992, NA, count))
save(gl_report_emig,
  file = "../data/gl_report_emig.rda",
  compress = "bzip2"
)
