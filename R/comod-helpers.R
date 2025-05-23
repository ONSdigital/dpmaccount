## HAS_TESTS
#' Extract elements from fitted model for
#' single cohort
#'
#' Extract estimates or data from a fitted object of
#' class `"dpmaccount_comod"`.
#'
#' Argument `what` can be one or more of the following,
#' - `"population"` Population counts.
#' - `"events"` Counts of births, deaths, ins, outs.
#' - `"exposure"` Person-years of exposure.
#' - `"rates"` Rates for births, deaths, ins,
#' and outs.
#' - `"sysmods"` Hyper-parameters from system models
#' - `"datamods"` Parameters from data models
#' - `"data_population"` Input data on population.
#' - `"data_events"` Input data on births, deaths,
#' ins, outs.
#'
#' @section Warning:
#'
#' Rates for births, deaths, and outs
#' are defined as events per person-year lived, while
#' rates for ins are defined as events per year
#' (or events per person-months lived and
#' events per month in
#' the case of monthly models.)
#'
#' @param object A fitted `"dpmaccount_comod"` object.
#' @param what Component(s) to extract.
#' @param n_draw Number of draws from posterior distribution.
#' Defaults to 1000.
#' @param seed_list list of seeds produced by make_seed_account()
#'
#' @returns Named list of tibbles.
#'
#' @noRd
components_cohort <- function(object,
                              what = "population",
                              n_draw = 1000L,
                              seed_list,
                              only_obs = TRUE) {
  choices <- c(
    "population",
    "events",
    "exposure",
    "rates",
    "sysmods",
    "datamods",
    "data_population",
    "data_events"
  )
  is_not_fitted <- !is_fitted(object)
  if (is_not_fitted) {
    stop(gettext("model not fitted"),
      call. = FALSE
    )
  }
  checkmate::assert_subset(
    x = what,
    choices = choices,
    empty.ok = FALSE
  )
  what <- unique(what)
  account <- components_cohort_account(object,
    n_draw = n_draw,
    seed_list = seed_list,
    only_obs = only_obs
  )
  ans <- list()
  if ("population" %in% what) {
    ans <- c(ans, account["population"])
  }
  if ("events" %in% what) {
    ans <- c(ans, account["events"])
  }
  if ("exposure" %in% what) {
    ans <- c(ans, account["exposure"])
  }
  if ("rates" %in% what) {
    rates <- components_cohort_rates(object,
      account = account,
      n_draw = n_draw,
      seed_list = seed_list
    )
    ans <- c(ans, list(rates = rates))
  }
  if ("sysmods" %in% what) {
    sysmods <- components_cohort_sysmods(object)
    ans <- c(ans, list(sysmods = sysmods))
  }
  if ("datamods" %in% what) {
    datamods <- components_cohort_datamods(
      object = object,
      population = account[["population"]],
      events = account[["events"]],
      seed_list = seed_list
    )
    ans <- c(ans, list(datamods = datamods))
  }
  if (("data_population" %in% what) || ("data_events" %in% what)) {
    data <- components_cohort_data(object)
    if ("data_population" %in% what) {
      ans <- c(ans, list(data_population = data$population))
    }
    if ("data_events" %in% what) {
      ans <- c(ans, list(data_events = data$events))
    }
  }
  ans <- c(ans, list(propn_adjusted = account$propn_adjusted))
  ans
}


## HAS_TESTS
#' Generate demographic counts for a cohort
#' from a fitted 'comod' object
#'
#' Helper function for 'components_cohort()'.
#'
#' @param object A fitted object of class
#' "dpmaccount_comod"
#' @param n_draw Number of draws from
#' posterior distribution
#' @param seed_list list of seeds produced by make_seed_account()
#'
#' @returns A named list with tibbles
#' 'population', 'events', and 'exposure',
#' and scalar 'propn_adjusted'.
#'
#' @noRd
components_cohort_account <- function(object, n_draw, seed_list, only_obs = TRUE) {
  cohort <- object$cohort
  sex <- object$sex
  is_new_cohort <- object$is_new_cohort
  stk_init_mean <- object$prior_stk_init$mean
  stk_init_sd <- object$prior_stk_init$sd
  count_bthdth <- object$count_bthdth
  mean <- object$mean
  var <- object$var
  K <- nrow(count_bthdth)
  is_known_stk_init <- isTRUE(all.equal(stk_init_sd, 0))
  nms_extract <- c("log_val_ins", "log_val_outs")
  if (!is_known_stk_init) {
    nms_extract <- c("log_val_stk_init", nms_extract)
  }
  mean_log_val <- unlist(mean[names(mean) %in% nms_extract])
  var_log_val <- var[rownames(var) %in% nms_extract,
    colnames(var) %in% nms_extract,
    drop = FALSE
  ]
  stk_init <- if (is_known_stk_init) stk_init_mean else NULL
  counts <- draw_counts(
    n_draw = n_draw,
    mean = mean_log_val,
    var = var_log_val,
    stk_init = stk_init,
    val_dth = count_bthdth$val_dth,
    cohort = cohort,
    sex = sex,
    seed_in = seed_list$draw_counts_seed
  )
  val_stk_init <- counts$val_stk_init
  val_stk <- counts$val_stk
  val_ins <- counts$val_ins
  val_outs <- counts$val_outs
  n_adj <- counts$n_adj
  ## make 'population'

  if (is_new_cohort) {
    if (only_obs) {
      i_popn <- rep(c(TRUE, FALSE), length.out = K)
    } else {
      i_popn <- rep(TRUE, length.out = K)
    }
    population <- val_stk[i_popn, , drop = FALSE]
    time <- count_bthdth$time[i_popn]
    age <- count_bthdth$age[i_popn]
  } else {
    if (only_obs) {
      i_popn <- rep(c(FALSE, TRUE), length.out = K)
    } else {
      i_popn <- rep(TRUE, length.out = K)
    }
    population <- rbind(
      val_stk_init,
      val_stk[i_popn, , drop = FALSE]
    )
    time <- c(
      count_bthdth$time[[1L]] - 1L,
      count_bthdth$time[i_popn]
    )
    age <- c(
      count_bthdth$age[[1L]],
      count_bthdth$age[i_popn]
    )
  }

  population <- matrix_to_list_rows(population)
  population <- tibble::tibble(
    time = time,
    age = age,
    population = population
  )
  ## make 'events'
  ins <- matrix_to_list_rows(val_ins)
  outs <- matrix_to_list_rows(val_outs)
  events <- tibble::tibble(
    time = count_bthdth$time,
    age = count_bthdth$age,
    births = count_bthdth$val_bth,
    deaths = count_bthdth$val_dth,
    ins = ins,
    outs = outs
  )
  ## make 'exposure'
  start <- rbind(val_stk_init, val_stk[-K, , drop = FALSE])
  end <- val_stk
  exposure <- 0.25 * (start + end) ## 0.25 because triangles, not squares
  exposure <- matrix_to_list_rows(exposure)
  exposure <- tibble::tibble(
    time = count_bthdth$time,
    age = count_bthdth$age,
    exposure = exposure
  )
  ## make 'propn_adjusted'
  propn_adjusted <- n_adj / ncol(val_outs)
  ## return
  list(
    population = population,
    events = events,
    exposure = exposure,
    propn_adjusted = propn_adjusted
  )
}


## HAS_TESTS
#' Extract input data from a 'comod' object
#'
#' Helper function for 'components_cohort()'.
#'
#' @param object An object of class
#' "dpmaccount_comod"
#'
#' @returns A named list with tibbles
#' 'population' and 'events'
#'
#' @noRd
components_cohort_data <- function(object) {
  cohort <- object$cohort
  count_bthdth <- object$count_bthdth
  datamods_stk <- object$datamods_stk
  datamods_ins <- object$datamods_ins
  datamods_outs <- object$datamods_outs
  ## births and deaths
  births <- count_bthdth[c("time", "age", "val_bth")]
  names(births)[[3L]] <- count_bthdth[1L, "nm_data_bth"]
  deaths <- count_bthdth[c("time", "age", "val_dth")]
  names(deaths)[[3L]] <- count_bthdth[1L, "nm_data_dth"]
  ## population, ins, outs
  get_time_age_data <- function(obj, nm) {
    ans <- tibble::tibble(
      time = obj$time,
      age = obj$age,
      data = obj$data
    )
    names(ans)[[3L]] <- nm
    ans
  }
  population <- .mapply(get_time_age_data,
    dots = list(
      datamods_stk,
      names(datamods_stk)
    ),
    MoreArgs = list()
  )
  ins <- .mapply(get_time_age_data,
    dots = list(
      datamods_ins,
      names(datamods_ins)
    ),
    MoreArgs = list()
  )
  outs <- .mapply(get_time_age_data,
    dots = list(
      datamods_outs,
      names(datamods_outs)
    ),
    MoreArgs = list()
  )
  population <- Reduce(merge, population)
  is_accession <- population$age == population$time - cohort - 1L
  population <- population[!is_accession, , drop = FALSE]
  events <- c(list(births), list(deaths), ins, outs)
  events <- Reduce(merge, events)
  list(
    population = population,
    events = events
  )
}


## HAS_TESTS
#' Extract values for data models for a cohort
#' from a fitted 'comod' object
#'
#' Helper function for 'components_cohort()'.
#'
#' @param object A fitted object of class
#' "dpmaccount_comod"
#'
#' @returns A tibble.
#'
#' @noRd
components_cohort_datamods <- function(object, population, events, seed_list) {
  datamods_stk <- object$datamods_stk
  datamods_ins <- object$datamods_ins
  datamods_outs <- object$datamods_outs
  has_par <- c(any(get_has_par_all(datamods_stk)) ||
    any(get_has_par_all(datamods_ins)) ||
    any(get_has_par_all(datamods_outs)))
  if (has_par) {
    labels_popn <- make_labels_codatamods(
      codatamods = datamods_stk,
      nm_series = "population"
    )
    labels_ins <- make_labels_codatamods(
      codatamods = datamods_ins,
      nm_series = "ins"
    )
    labels_outs <- make_labels_codatamods(
      codatamods = datamods_outs,
      nm_series = "outs"
    )
    value <- draw_par_datamods(
      object = object,
      population = population,
      events = events,
      seed_in = seed_list$draw_par_datamods_seed
    )
    value <- matrix_to_list_rows(value)
    ans <- rbind(labels_popn, labels_ins, labels_outs)
    ans$value <- value
  } else {
    ans <- tibble::tibble(
      series = character(),
      data = character(),
      par = character(),
      value = list()
    )
  }
  ans
}


## HAS_TESTS
#' Generate demographic rates for a cohort
#' from a fitted 'comod' object
#'
#' Helper function for 'components_cohort()'.
#'
#' @param object A fitted object of class
#' "dpmaccount_comod"
#' @param account A list with components "population",
#' "events", and "exposure", typically created by
#' function 'components_cohort_account'.
#' @param n_draw
#'
#' @returns A tibble with columns
#' 'time', 'age', 'births', 'deaths',
#' 'ins', and 'outs'
#'
#' @noRd
components_cohort_rates <- function(object, account, n_draw, seed_list) {
  deaths <- draw_rates(
    sysmod = object$sysmod_dth,
    events = account$events$deaths,
    exposure = account$exposure$exposure,
    n_draw = n_draw,
    seed_in = seed_list$draw_rates_dth_seed
  )
  is_at_risk_bth <- is_at_risk_bth(object)
  if (is_at_risk_bth) {
    events_bth <- vapply(account$events$births, sum, 0) ## sum across sex
    births <- draw_rates(
      sysmod = object$sysmod_bth,
      events = events_bth,
      exposure = account$exposure$exposure,
      n_draw = n_draw,
      seed_in = seed_list$draw_rates_bth_seed
    )
  } else {
    births <- rep.int(list(rep(NA_real_, times = n_draw)),
      times = length(deaths)
    )
  }
  ins <- draw_rates(
    sysmod = object$sysmod_ins,
    events = account$events$ins,
    exposure = NULL,
    n_draw = n_draw,
    seed_in = seed_list$draw_rates_ins_seed
  )
  outs <- draw_rates(
    sysmod = object$sysmod_outs,
    events = account$events$outs,
    exposure = account$exposure$exposure,
    n_draw = n_draw,
    seed_in = seed_list$draw_rates_outs_seed
  )
  ans <- tibble::tibble(
    time = object$count_bthdth$time,
    age = object$count_bthdth$age,
    births = births,
    deaths = deaths,
    ins = ins,
    outs = outs
  )
  ans
}


## HAS_TESTS
#' Extract values for system models for a cohort
#' from a fitted 'comod' object
#'
#' Helper function for 'components_cohort()'.
#'
#' @param object A fitted object of class
#' "dpmaccount_comod"
#'
#' @returns A tibble with columns
#' 'time', 'age', 'births', 'deaths',
#' 'ins', and 'outs'
#'
#' @noRd
components_cohort_sysmods <- function(object) {
  time_age <- object$sysmod_bth[c("time", "age")]
  births <- object$sysmod_bth[c("mean", "disp")]
  deaths <- object$sysmod_dth[c("mean", "disp")]
  ins <- object$sysmod_ins[c("mean", "disp")]
  outs <- object$sysmod_outs[c("mean", "disp")]
  names(births) <- paste0("births.", names(births))
  names(deaths) <- paste0("deaths.", names(deaths))
  names(ins) <- paste0("ins.", names(ins))
  names(outs) <- paste0("outs.", names(outs))
  tibble::tibble(
    time_age,
    births,
    deaths,
    ins,
    outs
  )
}


## HAS_TESTS
#' Calculate increments for data models for a
#' demographic series (ie stock, ins, or outs),
#' and combine into one tibble
#'
#' @param datamods Named list of objects of class
#' "dpmaccount_codatamod"
#' @param cohort Integer
#' @param is_stock Whether the data models refer
#' to stock (ie population) data
#'
#' @returns A tibble
#'
#' @noRd
increments_codatamod_all <- function(datamods, cohort, is_stock) {
  nms_data <- names(datamods)
  ans <- .mapply(increments_codatamod,
    dots = list(
      x = datamods,
      nm_data = nms_data
    ),
    MoreArgs = list(
      cohort = cohort,
      is_stock = is_stock
    )
  )
  ans <- Reduce(full_join_time, ans)
  ans <- tibble::tibble(ans)
  ans
}


## HAS_TESTS
#' Calculate increments within a single cohort
#'
#' @param x Object of class 'dpmaccount_comod'
#'
#' @returns A tibble
#'
#' @noRd
increments_comod <- function(x) {
  cohort <- x$cohort
  count_bthdth <- x$count_bthdth
  datamods_stk <- x$datamods_stk
  datamods_ins <- x$datamods_ins
  datamods_outs <- x$datamods_outs
  incr_dth <- increments_deaths(
    count_bthdth = count_bthdth,
    cohort = cohort
  )
  incr_stk <- increments_codatamod_all(datamods_stk,
    cohort = cohort,
    is_stock = TRUE
  )
  incr_ins <- increments_codatamod_all(datamods_ins,
    cohort = cohort,
    is_stock = FALSE
  )
  incr_outs <- increments_codatamod_all(datamods_outs,
    cohort = cohort,
    is_stock = FALSE
  )
  ans <- list(
    incr_stk,
    incr_dth,
    incr_ins,
    incr_outs
  )
  ans <- Reduce(full_join_time, ans)
  ans <- tibble::tibble(ans)
  ans
}


## HAS_TESTS
#' Calculate increments for deaths
#'
#' @param count_bthdth Data frame
#' @param cohort Integer
#'
#' @returns A tibble
#'
#' @noRd
increments_deaths <- function(count_bthdth, cohort) {
  time <- count_bthdth$time
  age <- count_bthdth$age
  val_dth <- count_bthdth$val_dth
  nm_data_dth <- count_bthdth$nm_data_dth[[1L]]
  is_new_cohort <- time[[1L]] == cohort
  is_upper <- age == time - cohort - 1L
  is_extinct_cohort <- is_upper[length(time)]
  if (is_new_cohort) {
    time <- time[-1L]
    val_dth <- val_dth[-1L]
  }
  if (is_extinct_cohort) {
    time <- time[-length(time)]
    val_dth <- val_dth[-length(val_dth)]
  }
  if (length(time) >= 2L) {
    time <- time[c(TRUE, FALSE)]
    idx <- rep(seq_along(time), each = 2L)
    val_dth <- tapply(val_dth, idx, sum)
    val_dth <- as.double(val_dth)
  }
  ans <- tibble::tibble(
    time = time,
    val_dth = val_dth
  )
  names(ans)[[2L]] <- nm_data_dth
  ans
}
