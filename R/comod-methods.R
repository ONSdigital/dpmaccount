## Note that methods require an '@export' tag,
## even when the generic function is not exported
## https://github.com/r-lib/devtools/issues/2293


## 'fit' ----------------------------------------------------------------------

#' @importFrom generics fit
#' @export
generics::fit

#' Fit a cohort model
#'
#' For details on the algorithm used by
#' the optimiser [stats::nlminb()], see the discussion
#' [https://stackoverflow.com/questions/49375840/algorithm-name-in-nlminbs-port-routines](here).
#' Note that although the online help for `nlminb()` suggests
#' using function [stats::optim()] instead,
#' most applications of TMB (eg the glmmTMB package)
#' seem to use `nlminb()`.
#'
#' @section Warning:
#' The optimiser currently fails for some cases.
#'
#' @param object A `dpmaccount_comod` object.
#' @param keep_adfun Whether to keep function created
#' by TMB::MakeADFun.
#' @param ... Not currently used.
#'
#' @returns A fitted `dpmaccount_comod` object
#'
#' @keywords internal
#'
#' @export
fit.dpmaccount_comod <- function(object, keep_adfun = FALSE, ...) {
  prior_stk_init <- object$prior_stk_init
  count_bthdth <- object$count_bthdth
  sysmod_bth <- object$sysmod_bth
  sysmod_dth <- object$sysmod_dth
  sysmod_ins <- object$sysmod_ins
  sysmod_outs <- object$sysmod_outs
  datamods_stk <- object$datamods_stk
  datamods_ins <- object$datamods_ins
  datamods_outs <- object$datamods_outs
  val_bth <- vapply(count_bthdth$val_bth, sum, 0) ## sum across sexes
  data <- list(
    mean_stk_init = prior_stk_init$mean,
    sd_stk_init = prior_stk_init$sd,
    has_bth = count_bthdth$has_bth,
    val_bth = val_bth,
    val_dth = count_bthdth$val_dth,
    mean_bth = sysmod_bth$mean,
    mean_dth = sysmod_dth$mean,
    mean_ins = sysmod_ins$mean,
    mean_outs = sysmod_outs$mean,
    disp_bth = sysmod_bth$disp,
    disp_dth = sysmod_dth$disp,
    disp_ins = sysmod_ins$disp,
    disp_outs = sysmod_outs$disp,
    data_all_stk = get_data_all(datamods_stk),
    data_all_ins = get_data_all(datamods_ins),
    data_all_outs = get_data_all(datamods_outs),
    is_obs_all_stk = get_is_obs_all(datamods_stk),
    is_obs_all_ins = get_is_obs_all(datamods_ins),
    is_obs_all_outs = get_is_obs_all(datamods_outs),
    idx_data_all_stk = get_idx_data_all(datamods_stk),
    idx_data_all_ins = get_idx_data_all(datamods_ins),
    idx_data_all_outs = get_idx_data_all(datamods_outs),
    idx_par_all_stk = get_idx_par_all(datamods_stk),
    idx_par_all_ins = get_idx_par_all(datamods_ins),
    idx_par_all_outs = get_idx_par_all(datamods_outs),
    const_all_stk = get_const_all(datamods_stk),
    const_all_ins = get_const_all(datamods_ins),
    const_all_outs = get_const_all(datamods_outs),
    idx_const_all_stk = get_idx_const_all(datamods_stk),
    idx_const_all_ins = get_idx_const_all(datamods_ins),
    idx_const_all_outs = get_idx_const_all(datamods_outs),
    i_mod_all_stk = get_i_mod_all(datamods_stk),
    i_mod_all_ins = get_i_mod_all(datamods_ins),
    i_mod_all_outs = get_i_mod_all(datamods_outs),
    has_par_all_stk = get_has_par_all(datamods_stk),
    has_par_all_ins = get_has_par_all(datamods_ins),
    has_par_all_outs = get_has_par_all(datamods_outs)
  )
  parameters <- make_parameters(
    mean_stk_init = prior_stk_init$mean,
    sd_stk_init = prior_stk_init$sd,
    val_dth = count_bthdth$val_dth,
    mean_ins = sysmod_ins$mean,
    mean_outs = sysmod_outs$mean,
    datamods_stk = datamods_stk,
    datamods_ins = datamods_ins,
    datamods_outs = datamods_outs
  )
  map <- make_map(prior_stk_init)
  f <- TMB::MakeADFun(
    data = data,
    parameters = parameters,
    map = map,
    DLL = "dpmaccount",
    silent = TRUE
  )
  ## 'iter.max' and 'eval.max' copied from from glmmTMB:::glmTMBControl;
  ## abs.tol from online help for nlminb
  control <- list(
    iter.max = 300,
    eval.max = 400,
    abs.tol = 1e-20
  )
  suppressWarnings(
    val_nlminb <- tryCatch(
      stats::nlminb(
        start = f$par,
        objective = f$fn,
        gradient = f$gr,
        hessian = f$he,
        control = control
      ),
      error = function(e) NULL
    )
  )
  has_val_nlminb <- !is.null(val_nlminb)
  if (has_val_nlminb) {
    nlminb_conv <- val_nlminb$convergence == 0L
    if (nlminb_conv) {
      rep <- TMB::sdreport(f, getReportCovariance = TRUE)
      mean <- rep$par.fixed
      var <- rep$cov.fixed
      var_nona <- !anyNA(var) && all(is.finite(var))
      if (var_nona) {
        var_pos <- is_positive_definite(var)
        if (var_pos) {
          var_nearpos <- NA
        } else {
          val_nearPD <- Matrix::nearPD(
            x = var,
            base.matrix = TRUE
          )
          var_nearpos <- val_nearPD$converged
          if (var_nearpos) {
            var <- val_nearPD$mat
            dimnames(var) <- dimnames(rep$cov.fixed)
          } else {
            var <- make_fail_var(parameters)
          }
        }
      } else {
        mean <- make_fail_mean(parameters)
        var <- make_fail_var(parameters)
        var_pos <- NA
        var_nearpos <- NA
      }
    } else {
      mean <- make_fail_mean(parameters)
      var <- make_fail_var(parameters)
      var_nona <- NA
      var_pos <- NA
      var_nearpos <- NA
    }
    object$val_nlminb <- val_nlminb ## requires special treatment: see below
  } else {
    mean <- make_fail_mean(parameters)
    var <- make_fail_var(parameters)
    nlminb_conv <- NA
    var_nona <- NA
    var_pos <- NA
    var_nearpos <- NA
    object["val_nlminb"] <- list(NULL) ## to set element of list to NULL
  }
  object$mean <- mean
  object$var <- var
  object$nlminb_conv <- nlminb_conv
  object$var_nona <- var_nona
  object$var_pos <- var_pos
  object$var_nearpos <- var_nearpos
  if (keep_adfun) {
    object$adfun <- f
  }
  object
}


## 'get_nm_data_bth' ----------------------------------------------------------

#' Get the name of the dataset holding information about births
#'
#' @param mod An object of class 'dpmaccount_comod'
#'
#' @returns A string
#'
#' @noRd
get_nm_data_bth <- function(mod) {
  UseMethod("get_nm_data_bth")
}

## HAS_TESTS
#' @export
get_nm_data_bth.dpmaccount_comod <- function(mod) {
  mod$count_bthdth$nm_data_bth[[1L]]
}


## 'get_nm_data_dth' ----------------------------------------------------------

#' Get the name of the dataset holding information about deaths
#'
#' @param mod An object of class 'dpmaccount_comod'
#'
#' @returns A string
#'
#' @noRd
get_nm_data_dth <- function(mod) {
  UseMethod("get_nm_data_dth")
}

## HAS_TESTS
#' @export
get_nm_data_dth.dpmaccount_comod <- function(mod) {
  mod$count_bthdth$nm_data_dth[[1L]]
}


## 'is_at_risk_bth' -----------------------------------------------------------

#' Find out if members of a cohort are at risk of having a birth
#'
#' Find out whether a cohort model allows for the cohort
#' to have births, by looking in the 'count_bthdth'
#' component.
#'
#' @param mod An object of class 'dpmaccount_comod'
#'
#' @returns TRUE OR FALSE
#'
#' @noRd
is_at_risk_bth <- function(mod) {
  UseMethod("is_at_risk_bth")
}

## HAS_TESTS
#' @export
is_at_risk_bth.dpmaccount_comod <- function(mod) {
  has_bth <- mod$count_bthdth$has_bth
  any(has_bth)
}


## 'is_fittted' ---------------------------------------------------------------

#' Find out if a cohort model has been fitted
#'
#' Find out whether a cohort model has been fitted,
#' by checking whether the `mean` component
#' is non-NULL.
#'
#' @param mod An object of class 'dpmaccount_comod'
#'
#' @returns TRUE OR FALSE
#'
#' @noRd
is_fitted <- function(mod) {
  UseMethod("is_fitted")
}

## HAS_TESTS
#' @export
is_fitted.dpmaccount_comod <- function(mod) {
  !is.null(mod$mean)
}


## 'print' --------------------------------------------------------------------


#' @export
print.dpmaccount_comod <- function(x, ...) {
  is_fitted <- is_fitted(x)
  cat("<")
  if (is_fitted) {
    cat("Fitted")
  } else {
    cat("Unfitted")
  }
  cat(" object of class \"", class(x), "\">\n", sep = "")
  invisible(x)
}


## summary --------------------------------------------------------------------

## HAS_TESTS
#' Summarise a cohort model
#'
#' @param object A `dpmaccount_comod` object, not necessarily fitted.
#' @param ... Not currently used.
#'
#' @returns An object of class `"dpmaccount_comod_summary"`.
#'
#' @examples
#' results <- dpmaccount::results_greenland
#' fitted <- results$fitted
#' fitted[[12]]
#' summary(fitted[[12]])
#' @export
summary.dpmaccount_comod <- function(object, ...) {
  prior_stk_init <- object$prior_stk_init
  count_bthdth <- object$count_bthdth
  sysmods <- list(
    deaths = object$sysmod_dth,
    ins = object$sysmod_ins,
    outs = object$sysmod_outs
  )
  datamods_stk <- object$datamods_stk
  datamods_ins <- object$datamods_ins
  datamods_outs <- object$datamods_outs
  is_fitted <- is_fitted(object)
  deaths <- count_bthdth[c("time", "age", "val_dth")]
  names(deaths)[[3L]] <- count_bthdth$nm_data_dth[[1L]]
  rates <- summarise_sysmods(sysmods)
  data_stk <- summarise_codatamods(datamods_stk)
  data_ins <- summarise_codatamods(datamods_ins)
  data_outs <- summarise_codatamods(datamods_outs)
  data <- list(
    deaths,
    data_stk,
    data_ins,
    data_outs
  )
  data <- data[!vapply(data, is.null, FALSE)]
  data <- Reduce(full_join_timeage, data)
  if (is_fitted) {
    val_nlminb <- object$val_nlminb
    converged <- val_nlminb$convergence == 0L
    message <- val_nlminb$message
    if (converged) {
      mean <- object$mean
      estimates <- summarise_estimated_means(
        mean = mean,
        prior_stk_init = prior_stk_init,
        dths = count_bthdth$val_dth
      )
      account <- data.frame(deaths[c("time", "age")],
        estimates["mean_stk_end"],
        deaths = deaths[[3L]],
        estimates[c("mean_ins", "mean_outs")]
      )
      if (nrow(account) > 1L) {
        time0 <- account$time[[2L]] - 1L
        age0 <- account$age[[2L]] - 1L
      } else {
        time0 <- NA
        age0 <- NA
      }
      stk0 <- with(
        account,
        mean_stk_end[[1]] + deaths[[1L]]
          - mean_ins[[1L]] + mean_outs[[1L]]
      )
      account <- rbind(
        data.frame(
          time = time0,
          age = age0,
          mean_stk_end = stk0,
          deaths = NA,
          mean_ins = NA,
          mean_outs = NA
        ),
        account
      )
    }
  }
  ans <- list(
    is_fitted = is_fitted,
    data = data,
    rates = rates,
    prior_stk_init = prior_stk_init
  )
  if (is_fitted) {
    ans <- c(
      ans,
      list(
        converged = converged,
        message = message
      )
    )
    if (converged) {
      ans <- c(ans, list(account = account))
    }
  }
  class(ans) <- "dpmaccount_comod_summary"
  ans
}

#' @export
print.dpmaccount_comod_summary <- function(x, digits = 2, ...) {
  title <- sprintf(
    "Summary of %s object of class \"dpmaccount_comod\"\n\n",
    if (x$is_fitted) "Fitted" else "Unfitted"
  )
  cat(title)
  cat("data:\n")
  print(x$data)
  cat("\n")
  cat("rates:\n")
  print(x$rates, digits = digits)
  cat("\n")
  cat("prior for initial stock:\n")
  cat("    mean: ", x$prior_stk_init$mean,
    ", sd: ", x$prior_stk_init$sd, "\n",
    sep = ""
  )
  cat("\n")
  if (x$is_fitted) {
    cat("message from optimiser: ")
    cat(x$message)
    cat("\n")
    if (x$converged) {
      cat("\n")
      cat("account:\n")
      print(x$account, digits = digits)
    }
  }
  invisible(x)
}
