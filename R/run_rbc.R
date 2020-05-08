# main functions

#' @title Run RBC  
#' @description A function that attempts to determine long-term 
#' migration statuses, and pre-crossing and post-crossing residence
#'  statuses, for all border crossings where these statuses are not known. 
#' @param crossing_data A pre-processed group data contain
#' journeys, movements and other raw crossing data. The data should 
#' contain columns in the set of 'journeyId', 'personId', 'date_crossing',
#'  'is_arrival', 'journey_sequence', and 'journeyId_prev'.
#' @param init_res_status_data Optional, the raw data of the initial residence
#'  status in the format of data frame. The journey data should contain 
#'  columns in the set of 'personId', 'res_status_initial', and
#'  'date_finalised' if applied. The initial data is a supplementary
#'  to the \code{crossing_data} that provides the initial residence 
#'  status of the target people who made the border crossing (journey).
#' @param window_size The maximum length of the scanning period.
#' Can be an integer giving the number of days, the result
#' of a call to function \code{\link[base]{difftime}}, or an object of
#' class \code{\link[lubridate:Duration-class]{Duration}}.
#' @param threshold_year The length of the yearly test period.
#' It can be an integer giving the number of days, the result
#' of a call to function \code{\link[base]{difftime}}, or an object of
#' class \code{\link[lubridate:Duration-class]{Duration}}.
#' @param parallel Logical. Whether to use parallel processing, to
#' speed up the calculation of migration statuses.
#' Defaults to \code{TRUE}.
#' @param n_core The number of cores to use, if \code{parallel} is
#' \code{TRUE}. Defaults to \code{2}. Higher values will
#' typically result in faster calculations on computers
#' with more than two cores.
#' @param max_ram Optional, it is used to limit the RAM that can be 
#' used by this function. The default value is 2 Gb.
#' @param include_error_columns Optional, if it is TRUE, the returned 
#' result of \code{error_data} will contain two extra columns
#'  \code{error_code} and \code{error_message}.
#' @param mc.cleanup Optional, if set to TRUE then all children that have been 
#' forked by this function will be killed (by sending SIGTERM) before
#' this function returns. Under normal circumstances mclapply waits
#' for the children to deliver results, so this option usually has 
#' only effect when mclapply is interrupted. If set to FALSE then 
#' child processes are collected, but not forcefully terminated.
#' As a special case this argument can be set to the number of the
#' signal that should be used to kill the children instead of SIGTERM.
#' 
#' @return A list type of object that contains two items: 
#' one is a data frame object that contains classified journeys and the other
#'  contains journeys that have been marked as error. 
#' Both items contain the same table structure in the set of 
#' 'journeyId', 'journeyId_prev', 'personId', 'date_crossing', 'is_arrival', 
#' 'journey_sequence','days_to_next_crossing', 'res_status_before', 
#' 'res_status_after', 'is_long_term_mig', 'date_finalised_res_before', 
#' 'date_finalised_res_after' and 'date_finalised_LTM'. 
#' The Boolean value (0, and 1) in the column 
#' 'is_long_term_mig' is the key classified result that tells us 
#' which journey derived the person to be a long term migrant.
#' 
#' @examples
#' 
#' ## generate test data 100 people and each person has 
#' ## 10 journeys
#' 
#' ## to suppresse log messages on the screen
#' migrbc::initialize_logger(log_level = 1)
#' 
#' number_of_people <- 100
#' person_data <- migrbc::setup_random_test_data(
#'     number_of_people, 
#'     initial_date = '2001-01-01', 
#'     numJourneys = 10,
#'     min = 0, 
#'     max = 100)
#' head(person_data)
#' 
#' cross_spaces <- migrbc::pre_process(person_data)
#' 
#' ## run in non-parallel
#' res <- migrbc::run_rbc(cross_spaces, 
#'                        window_size = 487, 
#'                        threshold_year = 365, 
#'                        parallel=FALSE)
#' 
#' ## run in parallel with n_core = 2
#' cross_spaces <- migrbc::pre_process(person_data, n_groups = 2)
#' res <- migrbc::run_rbc(cross_spaces, 
#'                        window_size = 487, 
#'                        threshold_year = 365, 
#'                        parallel=TRUE,
#'                        n_core = 2)
#' 
#' head(res$journeys)
#' head(res$error_data)
#' 
#' @export
## HAS_TESTS
run_rbc <- function(crossing_data,
                    init_res_status_data = NULL,
                    window_size = 487,
                    threshold_year = 365,
                    parallel = FALSE,
                    n_core = 2, 
                    max_ram = 2,
                    include_error_columns = FALSE, 
                    mc.cleanup = FALSE) {
  ## parameter validation
  check_integer("window_size", window_size)
  check_integer("threshold_year", threshold_year)
  check_integer("n_core", n_core)
  futile.logger::flog.info("entering 'run_rbc' :
                           window_size = %d, 
                           threshold_year = %d, 
                           parallel = %s, 
                           n_core = %d,
                           max_ram = %d,
                           include_error_columns = %s,
                           mc.cleanup = %s", 
    window_size,
    threshold_year,
    parallel,
    n_core, 
    max_ram,
    include_error_columns,
    mc.cleanup,
    name = "migrbc")
  ## check if the input data is RAW DATA not the pre_processed data
  if (is.data.frame(crossing_data)) {
    check_data_columns(crossing_data)
    pre_processed_data <- pre_process(
      crossing_data,
      init_res_status_data = init_res_status_data,
      n_groups = n_core)
  } else {
    pre_processed_data <- crossing_data
  }
  # not more than max_ram Gb in total
  check_work_spaces(pre_processed_data, 
                    max_ram = max_ram,
                    target_unit = "Gb")
  ans <- tryCatch(
    resolve_data(pre_processed_data,
                        window_size = window_size,
                        threshold_year = threshold_year,
                        parallel = parallel, 
                        n_core = n_core,
                        include_error_columns = include_error_columns, 
                        mc.cleanup = mc.cleanup),
    warning = function(w) {
             futile.logger::flog.warn("warning in 'run_rbc' : %s", w$message)
             warning(w$message)
           },
    error = function(e) {
             futile.logger::flog.error("error in 'run_rbc' : %s", e$message)
             stop(e$message)
           })

  futile.logger::flog.info("leaving 'run_rbc'", name = "migrbc")
  ## only Turn the following on if debug
  ## on.exit(message("Run RBC completed = ", add = TRUE))
  ans
}
