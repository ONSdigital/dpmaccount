## Read data directly from Statistics Greenland website using 'pxweb'

library(pxweb)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)

min_time <- 2020

## Obtain data

pxweb_query_list <- list(
  "mother's year of birth" = as.character(1927:2006),
  "mother's age" = as.character(12:46),
  "sex" = c("F", "M"),
  "time" = as.character(min_time:2022)
)
# Download data
px_data <-
  pxweb_get(
    url = "https://bank.stat.gl/api/v1/en/Greenland/BE/BE80/BE8020/BEXFERT.px",
    query = pxweb_query_list
  )

# Convert to data.frame
px_data_frame <- as.data.frame(px_data,
  column.name.type = "text",
  variable.value.type = "text"
)


## Tidy
data <- px_data_frame %>%
  select(sex,
    cohort = "mother's year of birth",
    age = "mother's age",
    time,
    count = "Live births by Greenland's administrative division"
  ) %>%
  tibble() %>%
  filter(!is.na(count)) %>%
  mutate(
    age = as.integer(age),
    cohort = as.integer(cohort),
    time = as.integer(time)
  ) %>%
  mutate(triangle = time - age - cohort) %>%
  filter(triangle %in% 0:1) %>%
  complete(age, sex, triangle, time, fill = list(count = 0L)) %>%
  mutate(cohort = time - age - triangle)


## Reported values

gl_report_births <- data %>%
  select(age, sex, cohort, time, count)

save(gl_report_births,
  file = "../data/gl_report_births.rda",
  compress = "bzip2"
)
