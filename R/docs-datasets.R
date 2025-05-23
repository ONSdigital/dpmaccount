## Documentation for datasets

## 'gl_report_popn' -------------------------------------------------------

#' Registered population in Greenland
#'
#' Registered population
#' in Greenland at the end of the calendar year,
#' by cohort, age,
#' sex, and calendar year.
#'
#' @format A data frame with columns `"cohort"`,
#' `"age"`, `"sex"`,
#' `"time"`, and `"count"`.
#'
#' @source The StatsBank database on the Statistics Greenland
#' website, accessed using package 'pxweb'.
"gl_report_popn"


## 'gl_report_births' -----------------------------------------------------

#' Registered births in Greenland
#'
#' Registered births in Greenland by cohort and age of mother,
#' gender of child, and calendar year.
#'
#' @format A data frame with columns `"cohort"`,
#' `"age"`, `"sex"`,
#' `"time"`, and `"count"`.
#'
#' @source The StatsBank database on the Statistics Greenland
#' website, accessed using package 'pxweb'.
"gl_report_births"


## 'gl_report_deaths' -----------------------------------------------------

#' Registered deaths in Greenland
#'
#' Registered deaths in Greenland by cohort, age,
#' gender, and calendar year.
#'
#' @format A data frame with columns `"cohort"`,
#' `"age"`, `"sex"`,
#' `"time"`, and `"count"`.
#'
#' @source The StatsBank database on the Statistics Greenland
#' website, accessed using package 'pxweb'.
"gl_report_deaths"


## 'gl_report_immig' ------------------------------------------------------

#' Registered immigration in Greenland
#'
#' Registered immigration to Greenland by cohort, age,
#' gender, and calendar year.
#'
#' @format A data frame with columns `"cohort"`,
#' `"age"`, `"sex"`,
#' `"time"`, and `"count"`.
#'
#' @source The StatsBank database on the Statistics Greenland
#' website, accessed using package 'pxweb'.
"gl_report_immig"


## 'gl_report_emig' -------------------------------------------------------

#' Registered emigration from Greenland
#'
#' Registered emigration from Greenland by cohort, age,
#' gender, and calendar year.
#'
#' @format A data frame with columns `"cohort"`,
#' `"age"`, `"sex"`,
#' `"time"`, and `"count"`.
#'
#' @source The StatsBank database on the Statistics Greenland
#' website, accessed using package 'pxweb'.
"gl_report_emig"


## 'gl_sysmod_mean_births' ------------------------------------------------------

#' Estimates of birth rates in Greenland
#'
#' Mean of posterior distribution for
#' birth rates in Greenland by age of mother
#' and calendar year.
#'
#' @format A data frame with columns
#' `"age"`, `"time"`, and `"mean"`.
#'
#' @source Calculated from \code{\link{gl_report_births}} and
#' \code{\link{gl_report_popn}}.
"gl_sysmod_mean_births"


## 'gl_sysmod_mean_deaths' ------------------------------------------------------

#' Estimates of death rates in Greenland
#'
#' Mean of posterior distribution for
#' death rates in Greenland by age, sex,
#' and calendar year.
#'
#' @format A data frame with columns
#' `"age"`, `"sex"`,
#' `"time"`, and `"mean"`.
#'
#' @source Calculated from \code{\link{gl_report_deaths}}
#' and
#' \code{\link{gl_report_popn}}.
"gl_sysmod_mean_deaths"


## 'gl_sysmod_mean_immig' ------------------------------------------------------

#' Estimates of immigration 'rates' in Greenland
#'
#' Mean of posterior distribution for
#' expected counts for immigration in Greenland by age, sex,
#' and calendar year.
#'
#' @format A data frame with columns
#' `"age"`, `"sex"`,
#' `"time"`, and `"mean"`.
#'
#' @source Calculated from \code{\link{gl_report_immig}}.
"gl_sysmod_mean_immig"


## 'gl_sysmod_mean_emig' ------------------------------------------------------

#' Estimates of emigration rates in Greenland
#'
#' Mean of posterior distribution for
#' emigration rates in Greenland by age, sex,
#' and calendar year.
#'
#' @format A data frame with columns
#' `"age"`, `"sex"`,
#' `"time"`, and `"mean"`.
#'
#' @source Calculated from \code{\link{gl_report_emig}} and
#' \code{\link{gl_report_popn}}.
"gl_sysmod_mean_emig"


## 'gl_cover_ratio_popn' ------------------------------------------------------

#' Coverage ratios from simulated coverage survey for
#' Greenland population data
#'
#' @format A data frame with columns
#' `"age"`, `"sex"`, and `"ratio"'.
#'
#' @seealso Calculated from \code{\link{gl_report_popn}}.
"gl_cover_ratio_popn"


## 'gl_cover_sd_popn' ---------------------------------------------------------

#' Standard error for coverage ratios from simulated coverage survey
#' for Greenland population data
#'
#' @format A data frame with columns
#' `"age"`, `"sex"`, and `"sd".
#'
#' @seealso Calculated from \code{\link{gl_report_popn}}.
"gl_cover_sd_popn"


## 'gl_cover_ratio_immig' -----------------------------------------------------

#' Coverage ratios from simulated coverage survey for
#' Greenland immigration data
#'
#' @format A data frame with columns
#' `"age"`, `"sex"`, and `"ratio"'.
#'
#' @seealso Calculated from \code{\link{gl_report_immig}}.
"gl_cover_ratio_immig"


## 'gl_cover_sd_immig' --------------------------------------------------------

#' Standard error for coverage ratios from simulated coverage survey
#' for Greenland immigration data
#'
#' @format A data frame with columns
#' `"age"`, `"sex"`, and `"sd".
#'
#' @seealso Calculated from \code{\link{gl_report_immig}}.
"gl_cover_sd_immig"


## 'gl_cover_ratio_emig' ------------------------------------------------------

#' Coverage ratios from simulated coverage survey for
#' Greenland emigration data
#'
#' @format A data frame with columns
#' `"age"`, `"sex"`, and `"ratio"'.
#'
#' @seealso Calculated from \code{\link{gl_report_emig}}.
"gl_cover_ratio_emig"


## 'gl_cover_sd_emig' ---------------------------------------------------------

#' Standard error for coverage ratios from simulated coverage survey
#' for Greenland emigration data
#'
#' @format A data frame with columns
#' `"age"`, `"sex"`, and `"sd".
#'
#' @seealso Calculated from \code{\link{gl_report_emig}}.
"gl_cover_sd_emig"


#' Results obtained from fitting a model to Greenland data
#'
#' An object of class `dpmaccount_results`, created
#' by running [estimate_account()] on data for Greenland.
#'
#' `results_greenland` was created using the following
#' code
#' ```
#' sysmod_births <- sysmod(mean = gl_sysmod_mean_births,
#'                         disp = 0.2,
#'                         nm_series = "births")
#' sysmod_deaths <- sysmod(mean = gl_sysmod_mean_deaths,
#'                         disp = 0.2,
#'                         nm_series = "deaths")
#' sysmod_ins <- sysmod(mean = gl_sysmod_mean_immig,
#'                      disp = 0.2,
#'                      nm_series = "ins")
#' sysmod_outs <- sysmod(mean = gl_sysmod_mean_emig,
#'                       disp = 0.2,
#'                       nm_series = "outs")
#' sysmods <- list(sysmod_births,
#'                 sysmod_deaths,
#'                 sysmod_ins,
#'                 sysmod_outs)
#' datamod_popn <- datamod_norm(data = gl_report_popn,
#'                              sd = gl_cover_sd_popn,
#'                              nm_series = "population")
#' datamod_births <- datamod_exact(data = gl_report_births,
#'                                 nm_series = "births")
#' datamod_deaths <- datamod_exact(data = gl_report_deaths,
#'                                 nm_series = "deaths")
#' datamod_ins <- datamod_norm(data = gl_report_immig,
#'                             sd = gl_cover_sd_immig,
#'                             nm_series = "ins")
#' datamod_outs <- datamod_norm(data = gl_report_emig,
#'                              sd = gl_cover_sd_emig,
#'                              nm_series = "outs")
#' datamods <- list(datamod_popn = datamod_popn,
#'                  datamod_births = datamod_births,
#'                  datamod_deaths = datamod_deaths,
#'                  datamod_ins = datamod_ins,
#'                  datamod_outs = datamod_outs)
#' results_greenland <- estimate_account(sysmods = sysmods,
#'                                       datamods = datamods)
#' ```
#'
#' @format An object of class `dpmaccount_results`.
"results_greenland"
