#' Simulate arguments for function 'estimate_account'
#'
#' Designed for testing, not for use by end-users.
#'
#' @param n_age Number of age groups
#' @param n_time Number of periods
#' @param base_year First year for which events dataa is available
#' @param disp_rates Overdispersion for rates
#' @param scale_sd Parameter from data models
#' @param datamod_popn Whether to include a data model for population
#' @param datamod_ins Whether to include a data model for ins
#' @param datamod_outs Whether to include a data model for outs
#'
#' @returns A named list with elements 'sysmods' and 'datamods'
#'
#' @noRd
sim_args_estimate <- function(n_age = 3L,
                              n_time = 3L,
                              base_year = 2010L,
                              disp_rates = 0.1,
                              scale_sd = 0,
                              datamod_popn = TRUE,
                              datamod_ins = TRUE,
                              datamod_outs = TRUE) {
  ## classification variables -----------------------------------------------
  if (n_age < 2L) {
    stop("'n_age' is less than 2")
  }
  classif_vars_events_withcohort <- sim_classif_vars_events(
    n_age = n_age,
    n_time = n_time,
    has_cohort = TRUE,
    base_year = base_year
  )
  classif_vars_events_nocohort <- sim_classif_vars_events(
    n_age = n_age,
    n_time = n_time,
    has_cohort = FALSE,
    base_year = base_year
  )
  classif_vars_popn <- sim_classif_vars_popn(
    n_age = n_age,
    n_time = n_time,
    base_year = base_year
  )
  ## system models ----------------------------------------------------------
  ## sysmod - births
  mean_births <- classif_vars_events_nocohort[classif_vars_events_nocohort$age > 0L, ]
  mean_births$mean <- stats::runif(n = nrow(mean_births), max = 0.2)
  sysmod_births <- sysmod(
    mean = mean_births,
    disp = disp_rates,
    nm_series = "births"
  )
  ## sysmod - deaths
  mean_deaths <- classif_vars_events_nocohort
  mean_deaths$mean <- stats::runif(n = nrow(mean_deaths), max = 0.02)
  sysmod_deaths <- sysmod(
    mean = mean_deaths,
    disp = disp_rates,
    nm_series = "deaths"
  )
  ## sysmod - ins
  mean_ins <- classif_vars_events_nocohort
  mean_ins$mean <- stats::runif(n = nrow(mean_ins), max = 50)
  sysmod_ins <- sysmod(
    mean = mean_ins,
    disp = disp_rates,
    nm_series = "ins"
  )
  ## sysmod - outs
  mean_outs <- classif_vars_events_nocohort
  mean_outs$mean <- stats::runif(n = nrow(mean_outs), max = 0.02)
  sysmod_outs <- sysmod(
    mean = mean_outs,
    disp = disp_rates,
    nm_series = "outs"
  )
  ## combine
  sysmods <- list(
    sysmod_births,
    sysmod_deaths,
    sysmod_ins,
    sysmod_outs
  )
  ## data models ------------------------------------------------------------
  datamods <- list()
  ## datamod - popn
  if (datamod_popn) {
    data_popn <- classif_vars_popn
    data_popn$count <- stats::runif(n = nrow(data_popn), min = 950, max = 1050)
    sd_popn <- classif_vars_popn
    sd_popn$sd <- stats::runif(n = nrow(sd_popn), min = 10, max = 30)
    datamod_popn <- datamod_norm(
      data = data_popn,
      sd = sd_popn,
      scale_sd = scale_sd,
      nm_series = "population"
    )
    datamods <- c(datamods, list(datamod_popn))
  }
  ## datamod - births
  data_births <- classif_vars_events_withcohort[classif_vars_events_withcohort$age > 0L, ]
  data_births$count <- stats::rpois(n = nrow(data_births), lambda = 50)
  datamod_births <- datamod_exact(data = data_births, nm_series = "births")
  datamods <- c(datamods, list(datamod_births))
  ## datamod - deaths
  data_deaths <- classif_vars_events_withcohort
  data_deaths$count <- stats::rpois(n = nrow(data_deaths), lambda = 10)
  datamod_deaths <- datamod_exact(data = data_deaths, nm_series = "deaths")
  datamods <- c(datamods, list(datamod_deaths))
  ## datamod - ins
  if (datamod_ins) {
    data_ins <- classif_vars_events_withcohort
    data_ins$count <- stats::rpois(n = nrow(data_ins), lambda = 50)
    sd_ins <- classif_vars_events_nocohort
    sd_ins$sd <- stats::runif(n = nrow(sd_ins), min = 5, max = 20)
    datamod_ins <- datamod_norm(
      data = data_ins,
      sd = sd_ins,
      scale_sd = scale_sd,
      nm_series = "ins"
    )
    datamods <- c(datamods, list(datamod_ins))
  }
  ## datamod - outs
  if (datamod_outs) {
    data_outs <- classif_vars_events_withcohort
    data_outs$count <- stats::runif(n = nrow(data_outs), max = 20)
    sd_outs <- classif_vars_events_nocohort
    sd_outs$sd <- stats::runif(n = nrow(sd_outs), min = 2, 5)
    datamod_outs <- datamod_norm(
      data = data_outs,
      sd = sd_outs,
      scale_sd = scale_sd,
      nm_series = "outs"
    )
    datamods <- c(datamods, list(datamod_outs))
  }
  ## combine and return -----------------------------------------------------
  list(
    sysmods = sysmods,
    datamods = datamods
  )
}


## HAS_TESTS
#' Simulate classification variables for events
#'
#' @param n_age Number of age groups. Defaults to 3.
#' @param n_time Number of periods. Defaults to 3.
#' @param has_cohort Whether classification variables
#' include cohort. Defaults to TRUE.
#' @param base_year First year for which there are
#' values for events. Defaults to 2010.
#'
#' @returns Data frame with variables
#' 'age', 'sex', 'cohort', and 'time',
#' or 'age', sex', and 'time'.
#'
#' @noRd
sim_classif_vars_events <- function(n_age = 3L,
                                    n_time = 3L,
                                    has_cohort = TRUE,
                                    base_year = 2010L) {
  age <- seq.int(from = 0L, to = n_age - 1L)
  sex <- c("Female", "Male")
  time <- seq.int(from = base_year, length.out = n_time)
  args <- list(age = age, sex = sex, time = time)
  if (has_cohort) {
    triangle <- 0:1
    args <- c(args, list(triangle = triangle))
  }
  ans <- expand.grid(args, KEEP.OUT.ATTRS = FALSE)
  if (has_cohort) {
    ans <- within(ans, {
      cohort <- time - age - triangle
    })
    ans <- ans[c("age", "sex", "cohort", "time")]
  }
  ans
}


## HAS_TESTS
#' Simulate classification variables for population
#'
#' @param n_age Number of age groups. Defaults to 3.
#' @param n_time Number of periods. Defaults to 3.
#' (Note periods, not time points.)#
#' @param base_year First year for which there are
#' values for events. Defaults to 2010.
#'
#' @returns Data frame with variables
#' 'age', sex', and 'time'.
#'
#' @noRd
sim_classif_vars_popn <- function(n_age = 3L,
                                  n_time = 3L,
                                  base_year = 2010L) {
  age <- seq.int(from = 0L, to = n_age - 1L)
  sex <- c("Female", "Male")
  time <- seq.int(from = base_year - 1L, length.out = n_time + 1L)
  args <- list(age = age, sex = sex, time = time)
  expand.grid(args, KEEP.OUT.ATTRS = FALSE)
}


## 'sim_codatamod' ------------------------------------------------------------

## HAS_TESTS
#' Construct a simulated Normal data model
#'
#' Note that 'sd <- pmax(sd_relative * val, 0.5)'
#'
#' @param val Simulated true values values
#' @param sd_relative Standard deviation of error
#' as proportion of true value.
#' @param scale_ratio,scale_sd Arguments passed to datamod_norm.
#' @param is_obs Integer. Whether value is observed
#' @param time Vector of times
#' @param age Vector of ages
#'
#' @returns An object of class "dpmaccount_codatamod_norm"
#'
#' @noRd
sim_codatamod <- function(val, sd_relative, scale_ratio, scale_sd, is_obs, time, age) {
  n <- length(val)
  ratio <- rep(1, times = n)
  sd <- pmax(sd_relative * val, 0.5)
  n_obs <- sum(is_obs)
  data <- rep(NA_real_, times = n)
  is_obs_lgl <- as.logical(is_obs)
  data[is_obs_lgl] <- pmax(
    round(
      stats::rnorm(
        n = n_obs,
        mean = ratio[is_obs_lgl] * val[is_obs_lgl],
        sd = sd[is_obs_lgl]
      )
    ),
    0
  )
  args <- data.frame(
    data = data,
    is_obs = is_obs,
    ratio = ratio,
    sd = sd,
    scale_ratio = scale_ratio,
    scale_sd = scale_sd,
    time = time,
    age = age
  )
  new_codatamod_norm(args)
}


## 'sim_comod' ----------------------------------------------------------------

## HAS_TESTS
#' Simulate a comod object
#'
#' @section Warning: The interface for this function
#' is still evolving: eg we need to include the
#' ability to specify non-normal data models.
#'
#' @param K Number of Lexis triangles
#' @param mean_stk_init Prior expected
#' value for initial stock
#' @param rate_exit Death rate and
#' outs rate (which are assumed to be identical)
#' @param disp Overdispersion. Same
#' value applied to all rates.
#' @param sd_relative Size of errors
#' in (normal) data models
#' @param scale_ratio,scale_sd Arguments passed to datamod_norm.
#' @param is_new_cohort Whether the cohort
#' being simulated was born during the
#' estimation period.
#'
#' @returns An object of class "dpmaccount_comod"
#'
#' @keywords internal
#' @export
sim_comod <- function(K = 20,
                      mean_stk_init = 1000,
                      rate_exit = 0.01,
                      disp = 0.1,
                      sd_relative = 0.1,
                      scale_ratio = 0,
                      scale_sd = 0,
                      is_new_cohort = FALSE) {
  ## cohort, sex
  cohort <- if (is_new_cohort) 2015L else 1990L
  sex <- "Female"
  ## prior_stk_init
  sd_stk_init <- if (is_new_cohort) 0 else Inf
  prior_stk_init <- list(
    mean = mean_stk_init,
    sd = sd_stk_init
  )
  ## classif_vars
  time_age <- sim_time_age(
    K = K,
    cohort = cohort,
    is_new_cohort = is_new_cohort
  )
  ## symod_bth, sysmod_dth, sysmod_ins, sysmod_outs
  sysmod_bth <- data.frame(mean = if (is_new_cohort) 0 else rate_exit, disp = disp)
  sysmod_dth <- data.frame(mean = rate_exit, disp = disp)
  sysmod_ins <- data.frame(mean = mean_stk_init * rate_exit, disp = disp)
  sysmod_bth <- cbind(sysmod_bth, time_age)
  sysmod_dth <- cbind(sysmod_dth, time_age)
  sysmod_ins <- cbind(sysmod_ins, time_age)
  sysmod_outs <- sysmod_dth
  ## true stock, births, deaths, ins, outs
  vals <- project_cohort(
    is_new_cohort = is_new_cohort,
    prior_stk_init = prior_stk_init,
    sysmod_bth = sysmod_bth,
    sysmod_dth = sysmod_dth,
    sysmod_ins = sysmod_ins,
    sysmod_outs = sysmod_outs
  )
  ## count_bthdth
  has_bth <- rep(as.integer(!is_new_cohort), times = K)
  count_bthdth <- data.frame(
    has_bth = has_bth,
    val_bth = rep(NA, times = K),
    val_dth = vals$val_dth,
    nm_data_bth = "dataset_bth",
    nm_data_dth = "dataset_dth",
    time = time_age$time,
    age = time_age$age
  )
  count_bthdth$val_bth <- vals$val_bth ## list
  ## datasets
  if (is_new_cohort) {
    val <- vals$val_stk[-1L]
    is_obs <- rep(1:0, length.out = K)
    time <- time_age$time
    age <- time_age$age
    datamod_stk <- sim_codatamod(
      val = val,
      sd_relative = sd_relative,
      scale_ratio = scale_ratio,
      scale_sd = scale_sd,
      is_obs = is_obs,
      time = time,
      age = age
    )
  } else {
    val <- vals$val_stk
    is_obs <- rep(1:0, length.out = K + 1L)
    time <- c(time_age$time[1L] - 1L, time_age$time)
    age <- c(time_age$age[1L], time_age$age)
    datamod_stk <- sim_codatamod(
      val = val,
      sd_relative = sd_relative,
      scale_ratio = scale_ratio,
      scale_sd = scale_sd,
      is_obs = is_obs,
      time = time,
      age = age
    )
  }
  datamod_ins <- sim_codatamod(
    val = vals$val_ins,
    sd_relative = sd_relative,
    scale_ratio = scale_ratio,
    scale_sd = scale_sd,
    is_obs = rep(1L, times = K),
    time = time_age$time,
    age = time_age$age
  )
  datamod_outs <- sim_codatamod(
    val = vals$val_outs,
    sd_relative = sd_relative,
    scale_ratio = scale_ratio,
    scale_sd = scale_sd,
    is_obs = rep(1L, times = K),
    time = time_age$time,
    age = time_age$age
  )
  ## return value
  new_comod(
    cohort = cohort,
    sex = sex,
    is_new_cohort = is_new_cohort,
    prior_stk_init = prior_stk_init,
    count_bthdth = count_bthdth,
    sysmod_bth = sysmod_bth,
    sysmod_dth = sysmod_dth,
    sysmod_ins = sysmod_ins,
    sysmod_outs = sysmod_outs,
    datamods_stk = list(dataset_stk = datamod_stk),
    datamods_ins = list(dataset_ins = datamod_ins),
    datamods_outs = list(dataset_outs = datamod_outs)
  )
}
