#' @title Produce Error Result
#' 
#' @description This function is used to produce error result.

#' @param data_with_error The personal crossing data for RBC process
#' with error.
#' @param initial_res_status_data the initial residence status data.
#' @param error_message The error message.
#' @param include_error_columns Optional, if it is TRUE, the returned 
#' result of \code{error_data} will contain two extra columns
#'  \code{error_code} and \code{error_message}. 
#' @param window_size The maximum length of the scanning period.
#' Can be an integer giving the number of days, the result
#' of a call to function \code{\link[base]{difftime}}, or an object of
#' class \code{\link[lubridate:Duration-class]{Duration}}.
#'   
#' @return A dataframe object.
#' @examples 
#' 
#' j1 <-       c(journeyId = 1,
#'               personId = 1,
#'               is_arrival = 1,
#'               date_crossing = '2017-01-01',
#'               journey_sequence = 1,
#'               journeyId_prev = NA)
#' 
#' j2 <-       c(journeyId = 2,
#'               personId = 1,
#'               is_arrival = 1,
#'               date_crossing = '2018-01-06',
#'               journey_sequence = 2,
#'               journeyId_prev = 1)
#' 
#' j3 <-       c(journeyId = 3,
#'               personId = 1,
#'               is_arrival = 1,
#'               date_crossing = '2018-01-16',
#'               journey_sequence = 3,
#'               journeyId_prev = 2)
#' 
#' 
#' j4 <-       c(journeyId = 4,
#'               personId = 2,
#'               is_arrival = 0,
#'               date_crossing = '2017-01-01',
#'               journey_sequence = 1,
#'               journeyId_prev = NA)
#' 
#' j5 <-       c(journeyId = 5,
#'               personId = 2,
#'               is_arrival = 0,
#'               date_crossing = '2018-01-06',
#'               journey_sequence = 2,
#'               journeyId_prev = 4)
#' 
#' j6 <-       c(journeyId = 6,
#'               personId = 2,
#'               is_arrival = 0,
#'               date_crossing = '2018-01-16',
#'               journey_sequence = 3,
#'               journeyId_prev = 5)
#' 
#' person_data <- as.data.frame(rbind(j1, j2, j3, j4, j5, j6),
#'                              stringsAsFactors = FALSE)
#' i1 <- c(personId = 1, 
#'         res_status_initial = 1, 
#'         date_finalised = '2017-01-01')
#' ini_data <- as.data.frame(t(i1), stringsAsFactors = FALSE)  
#' 
#' person_data$journeyId <- as.numeric(person_data$journeyId)
#' person_data$personId <- as.numeric(person_data$personId)
#' person_data$is_arrival <- as.numeric(person_data$is_arrival)
#' person_data$journey_sequence <- 
#'   as.numeric(person_data$journey_sequence)
#' person_data$journeyId_prev <- 
#'   as.numeric(person_data$journeyId_prev)
#' 
#' ini_data$personId <- as.numeric(ini_data$personId)
#' ini_data$res_status_initial <- 
#'   as.numeric(ini_data$res_status_initial)
#' ini_data$date_finalised <- 
#'   as.character(ini_data$date_finalised)
#'   
#' res <- migrbc::resolve_data_with_error(person_data, 
#'                                        initial_res_status_data = ini_data,
#'                                        error_message = 'custom error',
#'                                        include_error_columns = TRUE)
#' res
#' 
#' @export
resolve_data_with_error <- function(data_with_error,
                                    initial_res_status_data,
                                    error_message = "",
                                    include_error_columns = FALSE, 
                                    window_size = 487) {
  futile.logger::flog.info("entering 'resolve_data_with_error' :", 
                           name = "migrbc")
  check_integer("window_size", window_size)
  check_data_columns(data_with_error)
  check_ini_res_data_columns(initial_res_status_data)
  check_is_logic(include_error_columns)
  
  if (is.null(data_with_error) || !is.data.frame(data_with_error)) {
    stop("'data_with_error' must be a data frame object.")
  }
  if (is.null(initial_res_status_data) || 
      !is.data.frame(initial_res_status_data)) {
    stop("'initial_res_status_data' must be a data frame object.")
  }
  
  error_personIds <- unique(data_with_error$personId)
  
  initial_res_status_data <- with(initial_res_status_data, {
    dplyr::filter(initial_res_status_data, 
                  personId %in% error_personIds)
  })
  
  res <- tryCatch(
    migrbc::run_rbc_process_with_error(
      data_with_error,
      initial_res_status_data, 
      error_message, window_size),
    warning = function(w) {
      futile.logger::flog.warn("warning in 'run_rbc_process_with_error' : %s",
                               w$message)
      warning(w$message)
    },
    error = function(e) {
      futile.logger::flog.error("error in 'run_rbc_process_with_error' : %s",
                                e$message)
      stop(e$message)
    })
  
  ncols <- ncol(res)
  if (!include_error_columns) {
    res <- res[, - ((ncols - 1):ncols)]
  }
  futile.logger::flog.info("leaving 'resolve_data_with_error' :",
                           name = "migrbc")
  res
}

