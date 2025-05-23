library(dpmaccount)

sysmod_births <- sysmod(
  mean = gl_sysmod_mean_births,
  disp = 0.2,
  nm_series = "births"
)
sysmod_deaths <- sysmod(
  mean = gl_sysmod_mean_deaths,
  disp = 0.2,
  nm_series = "deaths"
)
sysmod_ins <- sysmod(
  mean = gl_sysmod_mean_immig,
  disp = 0.2,
  nm_series = "ins"
)
sysmod_outs <- sysmod(
  mean = gl_sysmod_mean_emig,
  disp = 0.2,
  nm_series = "outs"
)
sysmods <- list(
  sysmod_births,
  sysmod_deaths,
  sysmod_ins,
  sysmod_outs
)

datamod_popn <- datamod_norm(
  data = gl_report_popn,
  sd = gl_cover_sd_popn,
  nm_series = "population"
)
datamod_births <- datamod_exact(
  data = gl_report_births,
  nm_series = "births"
)
datamod_deaths <- datamod_exact(
  data = gl_report_deaths,
  nm_series = "deaths"
)
datamod_ins <- datamod_norm(
  data = gl_report_immig,
  sd = gl_cover_sd_immig,
  scale_sd = 0.1,
  nm_series = "ins"
)
datamod_outs <- datamod_norm(
  data = gl_report_emig,
  sd = gl_cover_sd_emig,
  scale_sd = 0.1,
  nm_series = "outs"
)
datamods <- list(
  datamod_popn = datamod_popn,
  datamod_births = datamod_births,
  datamod_deaths = datamod_deaths,
  datamod_ins = datamod_ins,
  datamod_outs = datamod_outs
)

results_greenland <- estimate_account(
  sysmods = sysmods,
  datamods = datamods
)

save(results_greenland,
  file = "../data/results_greenland.rda",
  compress = "bzip2"
)
