#############################################################################
context("plot_mig_hist")
#############################################################################

setup_data <- function() {
    number_of_people <- 1
    person_data <- migrbc::setup_random_test_data(number_of_people, 
                                                  initial_date = "2001-01-01", 
                                                  numJourneys = 3,
                                                  min = 0, 
                                                  max = 100)
    cross_spaces <- migrbc::pre_process(person_data, n_groups = 1)

    ## run in non-parallel
    post_data <- migrbc::run_rbc(cross_spaces, 
                           window_size = 487, 
                           threshold_year = 365, 
                           parallel = FALSE)
    post_data
}

describe("run rbc of happy cases", {
    
    post_data <- setup_data()
    it("plot mig hist succeeded", {
        ## plot good result
        old_par <- par(mfrow = c(1, 1))
        mig_hist <- post_data$journeys
        expect_error(
          plot_mig_hist(date_crossing = as.character(mig_hist$date_crossing), 
                  is_arrival = mig_hist$is_arrival,
                  days_to_next_crossing = mig_hist$days_to_next_crossing,
                  show_date = FALSE, 
                  cex = 0.8), NA)
        expect_error(
            plot_mig_hist(date_crossing = as.character(mig_hist$date_crossing), 
                  is_arrival = mig_hist$is_arrival,
                  res_status_before_str ="t1",
                  res_status_after_str ="t1",
                  days_to_next_crossing = mig_hist$days_to_next_crossing,
                  show_date = FALSE, 
                  cex = 0.8), NA)
        par(old_par)
    })

    it("plot mig hist succeeded with a single date ", {
        number_of_people <- 1
        person_data <- migrbc::setup_random_test_data(
            number_of_people, 
            initial_date = "2001-01-01", 
            numJourneys = 1,
            min = 0, 
            max = 100)
        cross_spaces <- migrbc::pre_process(person_data, n_groups = 1)
        ## run in non-parallel
        post_data <- migrbc::run_rbc(cross_spaces, 
                               window_size = 487, 
                               threshold_year = 365, 
                               parallel = FALSE)
        ## plot good result
        old_par <- par(mfrow = c(1, 1))
        mig_hist <- post_data$journeys
        expect_error(
            plot_mig_hist(
                date_crossing = as.character(mig_hist$date_crossing), 
                is_arrival = mig_hist$is_arrival,
                days_to_next_crossing = mig_hist$days_to_next_crossing,
                show_date = FALSE, 
                cex = 0.8), NA)
        expect_error(
            plot_mig_hist(
                date_crossing = as.character(mig_hist$date_crossing), 
                is_arrival = mig_hist$is_arrival,
                days_to_next_crossing = mig_hist$days_to_next_crossing,
                show_date = TRUE, 
                cex = 0.8), NA)
        par(old_par)
    })
})

describe("run rbc of unhappy cases", {
    post_data <- setup_data()
    it("plot mig hist failed with NULL to date_crossing", {
        ## plot good result
        old_par <- par(mfrow = c(1, 1))
        mig_hist <- post_data$journeys
        expect_error(
            plot_mig_hist(date_crossing = NULL, 
                      is_arrival = mig_hist$is_arrival,
                      days_to_next_crossing = mig_hist$days_to_next_crossing,
                      show_date = FALSE, 
                      cex = 0.8), "*has class*")
        expect_error(
            plot_mig_hist(date_crossing = c(), 
                      is_arrival = mig_hist$is_arrival,
                      days_to_next_crossing = mig_hist$days_to_next_crossing,
                      show_date = FALSE, 
                      cex = 0.8), "*has class*")
        par(old_par)
    })
    
    it("plot mig hist failed with invalid value to is_arrival", {
        ## plot good result
        old_par <- par(mfrow = c(1, 1))
        mig_hist <- post_data$journeys
        expect_error(
            plot_mig_hist(date_crossing = as.character(mig_hist$date_crossing), 
                      is_arrival = as.character(mig_hist$date_crossing),
                      days_to_next_crossing = mig_hist$days_to_next_crossing,
                      show_date = FALSE, 
                      cex = 0.8), "*has class*")
        par(old_par)
    })
    
    it("plot mig hist failed with invalid value to days_to_next_crossing", {
        ## plot good result
        old_par <- par(mfrow = c(1, 1))
        mig_hist <- post_data$journeys
        expect_error(
            plot_mig_hist(date_crossing = as.character(mig_hist$date_crossing), 
                  is_arrival = mig_hist$is_arrival,
                  days_to_next_crossing = as.character(mig_hist$date_crossing),
                  show_date = FALSE, 
                  cex = 0.8), "*non-numeric argument to binary operator*")
        par(old_par)
    })
    
    it("plot mig hist failed with invalid value to show_date", {
        ## plot good result
        old_par <- par(mfrow = c(1, 1))
        mig_hist <- post_data$journeys
        expect_error(
            plot_mig_hist(date_crossing = as.character(mig_hist$date_crossing), 
                      is_arrival = mig_hist$is_arrival,
                      days_to_next_crossing = mig_hist$days_to_next_crossing,
                      show_date = "FALSE", 
                      cex = 0.8), "*has class*")
        par(old_par)
    })
    
    
    it("plot mig hist failed with invalid value to cex", {
        ## plot good result
        old_par <- par(mfrow = c(1, 1))
        mig_hist <- post_data$journeys
        expect_error(
            plot_mig_hist(date_crossing = as.character(mig_hist$date_crossing), 
                      is_arrival = mig_hist$is_arrival,
                      days_to_next_crossing = mig_hist$days_to_next_crossing,
                      show_date = FALSE, 
                      cex = "affa"), "*is non-numeric*")
        par(old_par)
    })
})
