#' @title A function to convert a large data into a number of sub groups
#' @description This function provides a mechanism to divide large data into
#' small chunks.
#' @param data A dataframe object.
#' @param init_res_status_data The raw data of the initial residence status
#'  in the format of data frame.
#' @param n_groups The number of groups required to be returned.
#' @return A list object contains reformatted raw data.
#' @examples 
#' 
#' number_of_people = 100
#' person_data <- migrbc::setup_random_test_data(number_of_people, 
#'                                               initial_date = '2001-01-01', 
#'                                               numJourneys = 5,
#'                                               min = 0, 
#'                                               max = 100)
#' crossings <- migrbc::pre_process(person_data, n_groups = 10)
#' crossings
#' 
#' @export
pre_process <- function(data, 
                        init_res_status_data = NULL,
                        n_groups = 1) {
  
  futile.logger::flog.info("entering 'pre_process' :",
                           name = "migrbc")
  check_data_columns(data)
  check_integer("n_groups", n_groups)
  person_id_list <- unique(data$personId)
  n_person_id_list <- length(person_id_list)
  if (n_groups > n_person_id_list) {
    n_groups <- n_person_id_list
  }
  num_group_elements <- ceiling(n_person_id_list/n_groups)
  id_groups <- split(person_id_list, 
                     rep(1:n_groups, 
                         each = num_group_elements)[1:n_person_id_list])
  ## get initial res status
  if (is.null(init_res_status_data)) {
    init_res_status_data <- data.frame(res_status_initial = logical(), 
                                       personId = numeric(), 
                                       date_finalised = character(),
                                       stringsAsFactors = FALSE)
  }
  
  res <- tryCatch(
    lapply(id_groups, function(id_group) {
      subdata <- with(data, {
        subset(data, personId %in% id_group)
      })
      initial_status_group <- with(init_res_status_data, {
        subset(init_res_status_data, personId %in% id_group)
      })
      list(group_data = subdata, initial_status_group = initial_status_group)
    }),
    warning = function(w) {
      futile.logger::flog.warn("warning in 'pre_process' : %s", w$message)
      warning(w$message)
    },
    error = function(e) {
      futile.logger::flog.error("error in 'pre_process' : %s", e$message)
      stop(e$message)
    })
  
  class(res) <- c("migrbc_preprocess", "list")
  
  futile.logger::flog.info("leaving 'pre_process' : 
              return %d groups of raw data.", length(res),
                           name = "migrbc")
  res
}

