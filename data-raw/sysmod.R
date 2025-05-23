library(dplyr, warn.conflicts = FALSE)
library(bage)
library(rvec)

set.seed(0)

load("../data/gl_report_popn.rda")
load("../data/gl_report_births.rda")
load("../data/gl_report_deaths.rda")
load("../data/gl_report_immig.rda")
load("../data/gl_report_emig.rda")

exposure <- gl_report_popn %>%
  group_by(age, sex) %>%
  mutate(exposure = 0.5 * (count + lag(count))) %>%
  ungroup() %>%
  filter(time != min(time)) %>%
  select(-count)


## births

births <- gl_report_births %>%
  count(age, time, wt = count, name = "births")
exposure_births <- exposure %>%
  filter(sex == "Female") %>%
  select(-sex)
data <- inner_join(births, exposure_births, by = c("age", "time"))

mod_births <- mod_pois(births ~ age + time,
  data = data,
  exposure = exposure
) %>%
  set_prior(age ~ RW2()) %>%
  fit()

gl_sysmod_mean_births <- mod_births %>%
  augment() %>%
  select(age, time, mean = .expected) %>%
  mutate(mean = draws_mean(mean))
save(gl_sysmod_mean_births,
  file = "../data/gl_sysmod_mean_births.rda",
  compress = "bzip2"
)


## deaths

deaths <- gl_report_deaths %>%
  count(age, sex, time, wt = count, name = "deaths")
data <- inner_join(deaths, exposure, by = c("age", "sex", "time")) %>%
  mutate(exposure = ifelse((deaths > 0) & (exposure == 0), 0.5, exposure))

mod_deaths <- mod_pois(deaths ~ age * sex + time,
  data = data,
  exposure = exposure
) %>%
  set_prior(age ~ RW2()) %>%
  fit()

gl_sysmod_mean_deaths <- mod_deaths %>%
  augment() %>%
  select(age, sex, time, mean = .expected) %>%
  mutate(mean = draws_mean(mean))
save(gl_sysmod_mean_deaths,
  file = "../data/gl_sysmod_mean_deaths.rda",
  compress = "bzip2"
)


## immigration

immig <- gl_report_immig %>%
  count(age, sex, time, wt = count, name = "immig") %>%
  mutate(immig = ifelse(time == 1992, NA, immig))
mod_immig <- mod_pois(immig ~ age * sex + age * time + sex * time,
  data = immig,
  exposure = 1
) %>%
  fit()

gl_sysmod_mean_immig <- mod_immig %>%
  augment() %>%
  select(age, sex, time, mean = .expected) %>%
  mutate(mean = draws_mean(mean))
save(gl_sysmod_mean_immig,
  file = "../data/gl_sysmod_mean_immig.rda",
  compress = "bzip2"
)


## emigration

emig <- gl_report_emig %>%
  count(age, sex, time, wt = count, name = "emig") %>%
  mutate(emig = ifelse(time == 1992, NA, emig))
data <- inner_join(emig, exposure, by = c("age", "sex", "time")) %>%
  mutate(exposure = ifelse((emig > 0) & (exposure == 0), 0.5, exposure))

mod_emig <- mod_pois(emig ~ age * sex + age * time + sex * time,
  data = data,
  exposure = exposure
) %>%
  fit()

gl_sysmod_mean_emig <- mod_emig %>%
  augment() %>%
  select(age, sex, time, mean = .expected) %>%
  mutate(mean = draws_mean(mean))
save(gl_sysmod_mean_emig,
  file = "../data/gl_sysmod_mean_emig.rda",
  compress = "bzip2"
)




## library(ggplot2)
## mod_deaths %>%
##     augment() %>%
##     mutate(.expected = draws_mean(.expected)) %>%
##     ggplot(aes(x = age, color = sex)) +
##     facet_wrap(vars(time)) +
##     geom_line(aes(y = .expected)) +
##     scale_y_log10() +
##     geom_point(aes(y = .observed), size = 0.2)


## library(ggplot2)
## mod_immig %>%
##     augment() %>%
##     mutate(.expected = draws_mean(.expected)) %>%
##     ggplot(aes(x = age, color = sex)) +
##     facet_wrap(vars(time)) +
##     geom_line(aes(y = .expected)) +
##     scale_y_log10() +
##     geom_point(aes(y = .observed), size = 0.2)



## library(ggplot2)
## mod_births %>%
##     augment() %>%
##     mutate(.expected = draws_mean(.expected)) %>%
##     ggplot(aes(x = age)) +
##     facet_wrap(vars(time)) +
##     geom_line(aes(y = .expected)) +
##     geom_point(aes(y = .observed), size = 0.2)
