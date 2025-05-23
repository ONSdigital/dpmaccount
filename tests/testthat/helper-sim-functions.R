## HAS_TESTS
#' Generate future stock, births, deaths, ins, outs for a single cohort given
#' a set of system rates and an initial population
#'
#' @param is_new_cohort Whether the cohort was born during
#' the estimation period
#' @param prior_stk_init Prior for initial stock.
#' List with elements 'mean' and 'sd'.
#' @param sysmod_bth, sysmod_dth, sysmod_ins, sysmod_outs
#' System models for births, deaths, ins, and outs, each
#' a list with elements 'mean' and 'disp'.
#'
#' @returns A named list of vectors of doubles, with
#' elements 'val_stk', 'val_bth', 'val_dth',
#' 'val_ins', and 'val_outs'.
#'
#'
#' @noRd
project_cohort <- function(is_new_cohort,
                           prior_stk_init,
                           sysmod_bth,
                           sysmod_dth,
                           sysmod_ins,
                           sysmod_outs) {
  ## vectors to hold results
  K <- length(sysmod_dth$mean)
  val_stk <- numeric(length = K + 1L)
  val_dth <- numeric(length = K)
  val_ins <- numeric(length = K)
  val_outs <- numeric(length = K)
  ## initial value for stock
  mean_stk_init <- prior_stk_init$mean
  sd_stk_init <- prior_stk_init$sd
  if (is.finite(sd_stk_init)) {
    val_stk[[1L]] <- max(
      round(stats::rnorm(
        n = 1L,
        mean = mean_stk_init,
        sd = sd_stk_init
      )),
      0
    )
  } else {
    val_stk[[1L]] <- stats::rpois(
      n = 1L,
      lambda = mean_stk_init
    )
  }
  ## iterate through triangles
  for (k in seq_len(K)) {
    ## generate ins
    val_ins[[k]] <- with(
      sysmod_ins,
      stats::rnbinom(
        n = 1L,
        size = 1 / disp[[k]],
        mu = mean[[k]]
      )
    )
    ## half of the ins enter at the start, half at the end
    val_ins_start <- whole_num_halve(val_ins[[k]])
    val_ins_end <- val_ins[[k]] - val_ins_start
    stk_start <- val_stk[[k]] + val_ins_start
    ## generate rates for deaths and outs
    rate_dth <- with(
      sysmod_dth,
      stats::rgamma(
        n = 1L,
        shape = 1 / disp[[k]],
        rate = 1 / (disp[[k]] * mean[[k]])
      )
    )
    rate_outs <- with(
      sysmod_outs,
      stats::rgamma(
        n = 1L,
        shape = 1 / disp[[k]],
        rate = 1 / (disp[[k]] * mean[[k]])
      )
    )
    ## turn into multinomial draws of deaths and outs
    prob_exit <- 1 - exp(-0.5 * (rate_dth + rate_outs))
    val_exit <- stats::rbinom(
      n = 1L,
      size = stk_start,
      prob = prob_exit
    )
    prob_dth <- rate_dth / (rate_dth + rate_outs)
    val_dth[[k]] <- stats::rbinom(
      n = 1L,
      size = val_exit,
      prob = prob_dth
    )
    val_outs[[k]] <- val_exit - val_dth[[k]]
    ## apply demographic accounting equation to derive stock
    val_stk[[k + 1L]] <- val_stk[[k]] - val_exit + val_ins[[k]]
  }
  ## generate births
  if (is_new_cohort) {
    val_bth <- rep(list(c(Female = NA_real_, Male = NA_real_)),
      times = K
    )
  } else {
    exposure <- 0.25 * (val_stk[-(K + 1L)] + val_stk[-1L])
    val_bth_female <- with(
      sysmod_bth,
      stats::rnbinom(
        n = K,
        size = 1 / disp,
        mu = mean * exposure
      )
    )
    val_bth_male <- with(
      sysmod_bth,
      stats::rnbinom(
        n = K,
        size = 1 / disp,
        mu = mean * exposure
      )
    )
    val_bth <- lapply(
      1:K,
      function(k) {
        c(
          Female = val_bth_female[k],
          Male = val_bth_male[k]
        )
      }
    )
  }
  ## return results
  list(
    val_stk = val_stk, ## K + 1L
    val_bth = val_bth, ## K
    val_dth = val_dth, ## K
    val_ins = val_ins, ## K
    val_outs = val_outs ## K
  )
}

#' Simulate system model arguments for function 'estimate_account'
#'
#' Designed for testing, not for use by end-users.
#'
#' @param n_age Integer - Number of age groups
#' @param n_time Integer - Number of periods
#' @param base_year Integer - First year for which events data is available
#' @param disp_rates Double - Overdispersion for rates
#'
#' @return A named list with elements 'sysmods'
#'
#' @noRd
sim_arg_system_models <- function(n_age = 3L,
                                  n_time = 3L,
                                  base_year = 2010L,
                                  disp_rates = 0.1) {
  if (n_age < 2L) {
    stop("'n_age' is less than 2")
  }

  classif_vars_events_nocohort <- sim_classif_vars_events(
    n_age = n_age,
    n_time = n_time,
    has_cohort = FALSE,
    base_year = base_year
  )

  mean_births <- classif_vars_events_nocohort[classif_vars_events_nocohort$age > 0L, ]
  mean_births$mean <- stats::runif(n = nrow(mean_births), max = 0.2)
  sysmod_births <- sysmod(
    mean = mean_births,
    disp = disp_rates,
    nm_series = "births"
  )

  mean_deaths <- classif_vars_events_nocohort
  mean_deaths$mean <- stats::runif(n = nrow(mean_deaths), max = 0.02)
  sysmod_deaths <- sysmod(
    mean = mean_deaths,
    disp = disp_rates,
    nm_series = "deaths"
  )

  mean_ins <- classif_vars_events_nocohort
  mean_ins$mean <- stats::runif(n = nrow(mean_ins), max = 50)
  sysmod_ins <- sysmod(
    mean = mean_ins,
    disp = disp_rates,
    nm_series = "ins"
  )

  mean_outs <- classif_vars_events_nocohort
  mean_outs$mean <- stats::runif(n = nrow(mean_outs), max = 0.02)
  sysmod_outs <- sysmod(
    mean = mean_outs,
    disp = disp_rates,
    nm_series = "outs"
  )

  sysmods <- list(
    sysmod_births,
    sysmod_deaths,
    sysmod_ins,
    sysmod_outs
  )
}


#' Simulate data model arguments for function 'estimate_account'
#'
#' Designed for testing, not for use by end-users.
#' @param n_age Integer - Number of age groups
#' @param n_time Integer - Number of periods
#' @param base_year Integer - First year for which events data is available
#' @param scale_sd Double - Parameter from data models
#' @param datamod_popn Logical - Whether to include a data model for population
#' @param datamod_ins Logical - Whether to include a data model for ins
#' @param datamod_outs Logical - Whether to include a data model for outs
#'
#' @return A named list of 'datamods'
#'
#' @noRd
sim_arg_data_models <- function(n_age = 3L,
                                n_time = 3L,
                                base_year = 2010L,
                                scale_sd = 0,
                                datamod_popn = TRUE,
                                datamod_ins = TRUE,
                                datamod_outs = TRUE) {
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

  datamods <- list()
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

  data_births <- classif_vars_events_withcohort[classif_vars_events_withcohort$age > 0L, ]
  data_births$count <- stats::rpois(n = nrow(data_births), lambda = 50)
  datamod_births <- datamod_exact(data = data_births, nm_series = "births")
  datamods <- c(datamods, list(datamod_births))

  data_deaths <- classif_vars_events_withcohort
  data_deaths$count <- stats::rpois(n = nrow(data_deaths), lambda = 10)
  datamod_deaths <- datamod_exact(data = data_deaths, nm_series = "deaths")
  datamods <- c(datamods, list(datamod_deaths))

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
  datamods
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
  count_bthdth$val_bth <- vals$val_bth ## list - NB val_bth itself is a list
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


## HAS_TESTS
#' Simulate time and age variables
#' for a cohort
#'
#' @param K Number of triangles
#' @param is_new_cohort Whether cohort born during
#' estimation period
#'
#' @returns A data frame with variables
#' 'time' and 'age'
#'
#' @noRd
sim_time_age <- function(K, cohort, is_new_cohort) {
  if (is_new_cohort) {
    s_time <- seq.int(from = cohort, length.out = (K + 1L) / 2L)
    time <- rep(s_time, each = 2L)[2L:(K + 1L)]
    triangle <- rep(0:1, times = ceiling((K + 1L) / 2L))[1L:K]
  } else {
    age_start <- 30L
    s_time <- seq.int(
      from = cohort + age_start + 1L,
      length.out = (K + 1L) / 2L
    )
    time <- rep(s_time, each = 2L)[1L:K]
    triangle <- rep(0:1, times = ceiling((K + 1L) / 2L))[2L:(K + 1L)]
  }
  age <- time - cohort - triangle
  data.frame(
    time = time,
    age = age
  )
}


## HAS_TESTS
#' Halve a number, and then if the result is not
#' a whole number, randomly round to whole number
#'
#' @param x Number to halve
#'
#' @returns A integerish double
#'
#' @noRd
whole_num_halve <- function(x) {
  ans <- x / 2
  remainder <- ans - floor(ans)
  if (isTRUE(all.equal(remainder, 0))) {
    ans
  } else {
    add <- stats::rbinom(n = 1L, size = 1L, prob = remainder)
    floor(ans) + add
  }
}
