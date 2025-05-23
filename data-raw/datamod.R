library(dplyr, warn.conflicts = FALSE)

set.seed(0)


## population

load("../data/gl_report_popn.rda")

gl_cover_ratio_popn <- gl_report_popn %>%
  select(age, sex) %>%
  unique() %>%
  mutate(ratio = runif(n = n(), min = 0.97, max = 1.03))
save(gl_cover_ratio_popn,
  file = "../data/gl_cover_ratio_popn.rda",
  compress = "bzip2"
)

gl_cover_sd_popn <- gl_report_popn %>%
  group_by(age, sex) %>%
  summarise(count = mean(count), .groups = "drop") %>%
  ungroup() %>%
  mutate(
    sd = rgamma(n = n(), shape = 0.5 * sqrt(count), rate = 1) + 0.1,
    sd = pmax(sd, 1)
  ) %>%
  select(age, sex, sd)
save(gl_cover_sd_popn,
  file = "../data/gl_cover_sd_popn.rda",
  compress = "bzip2"
)


## immigration

load("../data/gl_report_immig.rda")

gl_cover_ratio_immig <- gl_report_immig %>%
  select(age, sex) %>%
  unique() %>%
  mutate(ratio = runif(n = n(), min = 0.97, max = 1.03))
save(gl_cover_ratio_immig,
  file = "../data/gl_cover_ratio_immig.rda",
  compress = "bzip2"
)

gl_cover_sd_immig <- gl_report_immig %>%
  group_by(age, sex) %>%
  summarise(count = mean(count), .groups = "drop") %>%
  ungroup() %>%
  mutate(
    sd = rgamma(n = n(), shape = 0.5 * sqrt(count), rate = 1) + 0.1,
    sd = pmax(sd, 1)
  ) %>%
  select(age, sex, sd)
save(gl_cover_sd_immig,
  file = "../data/gl_cover_sd_immig.rda",
  compress = "bzip2"
)


## emigration

load("../data/gl_report_emig.rda")

gl_cover_ratio_emig <- gl_report_emig %>%
  select(age, sex) %>%
  unique() %>%
  mutate(ratio = runif(n = n(), min = 0.97, max = 1.03))
save(gl_cover_ratio_emig,
  file = "../data/gl_cover_ratio_emig.rda",
  compress = "bzip2"
)

gl_cover_sd_emig <- gl_report_emig %>%
  group_by(age, sex) %>%
  summarise(count = mean(count), .groups = "drop") %>%
  ungroup() %>%
  mutate(
    sd = rgamma(n = n(), shape = 0.5 * sqrt(count), rate = 1) + 0.1,
    sd = pmax(sd, 1)
  ) %>%
  select(age, sex, sd)
save(gl_cover_sd_emig,
  file = "../data/gl_cover_sd_emig.rda",
  compress = "bzip2"
)
