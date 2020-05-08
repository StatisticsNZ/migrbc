#' @title Process RBC
#' 
#' @description This function is the key function to do the rules 
#'  based classification.
#'  
#' @param grouped_data A list of data frame objects.
#' @param window_size The maximum length of the scanning period.
#' Can be an integer giving the number of days, the result
#' of a call to function \code{\link[base]{difftime}}, or an object of
#' class \code{\link[lubridate:Duration-class]{Duration}}.
#' @param threshold_year The length of the yearly test period.
#' Can be an integer giving the number of days, the result
#' of a call to function \code{\link[base]{difftime}}, or an object of
#' class \code{\link[lubridate:Duration-class]{Duration}}.
#' @param parallel Optional, if it is TRUE, run on parallel.
#' @param n_core if \code{parallel} set to TRUE, this will specify
#' the number of computer cores required.
#' @param include_error_columns Optional, if it is TRUE, the returned 
#' result of \code{error_data} will contain two extra columns
#'  \code{error_code} and \code{error_message}.
#' @param mc.cleanup if set to TRUE then all children that have been 
#' forked by this function will be killed (by sending SIGTERM) before
#' this function returns. Under normal circumstances mclapply waits
#' for the children to deliver results, so this option usually has 
#' only effect when mclapply is interrupted. If set to FALSE then 
#' child processes are collected, but not forcefully terminated.
#' As a special case this argument can be set to the number of the
#' signal that should be used to kill the children instead of SIGTERM.
#' 
#' @return A list type of object that contains a classified journey 
#' dataframe object and a error dataframe object.
#' @examples 
#' ## to suppresse log messages to the console
#' migrbc::initialize_logger(log_level = 1)
#' 
#' number_of_people = 10
#' person_data <- migrbc::setup_random_test_data(number_of_people, 
#'                                  initial_date = '2001-01-01', 
#'                                  numJourneys = 5,
#'                                  min = 0, 
#'                                  max = 10)
#' crossings <- migrbc::pre_process(person_data, n_groups = 10)
#' crossings
#' cross_spaces <- migrbc::resolve_data(crossings)
#' cross_spaces
#' 
#' @export
resolve_data <- function(grouped_data,
                         window_size = 487, 
                         threshold_year = 365,
                         parallel = FALSE,
                         n_core = 2,
                         include_error_columns = FALSE, 
                         mc.cleanup = FALSE) {
  futile.logger::flog.info("entering 'resolve_data' :",
                           name = "migrbc")
  check_integer("window_size", window_size)
  check_integer("threshold_year", threshold_year)
  check_integer("n_core", n_core)
  check_is_logic(parallel)
  check_is_logic(include_error_columns)

  if (is.null(grouped_data) || !is.list(grouped_data)) {
    stop(paste0("The parameter 'grouped_data' must be", 
                " a list of data frame objects"))
  }
  is_unix <- .Platform$OS.type == "unix"
  temp_env <- new.env(hash = TRUE, parent = parent.frame())
  temp_env$crossingWorkspaces <- grouped_data
  len_work_spaces <- length(temp_env$crossingWorkspaces)
  
  if (parallel && is_unix) {
    futile.logger::flog.info("Running with parallel:
                 n_cores = %s", n_core, name = "migrbc")
    res <- parallel::mclapply(seq_len(len_work_spaces), 
                              function(i, env, window_size, threshold_year) {
      futile.logger::flog.info("entering 'resolve_data, chunk =%d':
               window_size=%d, 
               threshold_year=%d", 
        i, window_size, threshold_year, name = "migrbc")
      internal_process(env[[i]],
                       window_size = window_size,
                       threshold_year = threshold_year)
    }, 
    temp_env$crossingWorkspaces,
    window_size,
    threshold_year,
    mc.cores = n_core, 
    mc.cleanup = mc.cleanup)
  } else {
    if (parallel && !is_unix) {
      futile.logger::flog.warn("Parallel using mclapply not supported on 
                   Windows R server", name = "migrbc")
    }
    futile.logger::flog.info("Running without parallel", name = "migrbc")
    res <- lapply(seq_len(len_work_spaces), 
                  function(i, env, window_size, threshold_year) {
      futile.logger::flog.info("entering 'resolve_data, chunk =%d':
               window_size=%d, 
               threshold_year=%d", 
        i, window_size, threshold_year, name = "migrbc")
      internal_process(env[[i]], 
                       window_size = window_size,
                       threshold_year = threshold_year)
    }, 
    temp_env$crossingWorkspaces,
    window_size, threshold_year)
  }
  res <- dplyr::bind_rows(res)
  res <- as.data.frame(res, stringsAsFactors = FALSE)
  ncols <- ncol(res)
  journeys <- res[res$error_code == 0, ]
  journeys <- journeys[, - ((ncols - 1):ncols)]
  error_data <- res[res$error_code > 0, ]
  if (!include_error_columns) {
    error_data <- error_data[, - ((ncols - 1):ncols)]
  }
  futile.logger::flog.info("leaving 'resolve_data' :", name = "migrbc")
  list(journeys = journeys, error_data = error_data)
}

#' Internal function
#' 
#' @param subgroup A subgroup of the pre-processed data groups,
#' generated by \code{migrbc::pre_process}.
#' @param window_size The maximum length of the scanning period.
#' Can be an integer giving the number of days, the result
#' of a call to function \code{\link[base]{difftime}}, or an object of
#' class \code{\link[lubridate:Duration-class]{Duration}}.
#' @param threshold_year The length of the yearly test period.
#' Can be an integer giving the number of days, the result
#' of a call to function \code{\link[base]{difftime}}, or an object of
#' class \code{\link[lubridate:Duration-class]{Duration}}.
#' @return A data frame object of classified / labelled journeys.
internal_process <- function(subgroup,
                             window_size,
                             threshold_year) {
  check_data_columns(subgroup$group_data)
  check_ini_res_data_columns(subgroup$initial_status_group)
  subgroup$group_data$journeyId <- 
    as.numeric(subgroup$group_data$journeyId)
  subgroup$group_data$personId <- 
    as.numeric(subgroup$group_data$personId)
  subgroup$group_data$is_arrival <- 
    as.numeric(subgroup$group_data$is_arrival)
  subgroup$group_data$date_crossing <- 
    as.character(subgroup$group_data$date_crossing)
  subgroup$group_data$journey_sequence <- 
    as.numeric(subgroup$group_data$journey_sequence)
  subgroup$group_data$journeyId_prev <- 
    as.numeric(subgroup$group_data$journeyId_prev)
  
  ## remove potential unwanted columns
  this_colnames <- colnames(subgroup$group_data)
  subgroup$group_data <- 
    subgroup$group_data[, which(!this_colnames %in% c("res_status_before",
                                                      "res_status_after"))]
  
  subgroup$initial_status_group$personId <- 
    as.numeric(subgroup$initial_status_group$personId)
  subgroup$initial_status_group$res_status_initial <- 
    as.numeric(subgroup$initial_status_group$res_status_initial)
  subgroup$initial_status_group$date_finalised <- 
    as.character(subgroup$initial_status_group$date_finalised)
  
 tryCatch(
    migrbc::run_rbc_process_core(subgroup$group_data,
                                 subgroup$initial_status_group, 
                                 window_size,
                                 threshold_year),
    warning = function(w) {
      futile.logger::flog.warn("warning in 'run_rbc_process_core' : %s",
                               w$message)
      warning(w$message)
    },
    error = function(e) {
      futile.logger::flog.error("error in 'run_rbc_process_core' : %s",
                                e$message)
      stop(e$message)
    })
  
}
