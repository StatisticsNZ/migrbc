#############################################################################
context("run_rbc")
#############################################################################

setup_data <- function(rows_data, n_groups) {
    person_data <- setup_random_test_data(rows_data, 
                                          initial_date = "2001-01-01", 
                                          numJourneys = 10, 
                                          min = 0, 
                                          max = 100)
    pre_process_data <- pre_process(person_data, n_groups = n_groups)
    list(data = person_data, pre_process_data = pre_process_data)
}

describe("test initialize_logger", {
    test_data <- setup_data(100, 10)
    it("Successful cases", {
        ## futile.logger::FATAL: 1
        ## futile.logger::ERROR: 2
        ## futile.logger::WARN:  4
        ## futile.logger::INFO:  6
        ## futile.logger::DEBUG: 8
        ## futile.logger::TRACE: 9
        initialize_logger(log_level = 1)
        expect_error(res <- run_rbc(test_data$pre_process_data, 
                                    window_size = 487, 
                                    threshold_year = 365), 
                     NA)
        initialize_logger(log_level = 2)
        expect_error(res <- run_rbc(test_data$pre_process_data, 
                                    window_size = 487, 
                                    threshold_year = 365), 
                     NA)
        initialize_logger(log_level = 4)
        expect_error(res <- run_rbc(test_data$pre_process_data, 
                                    window_size = 487, 
                                    threshold_year = 365), 
                     NA)
        initialize_logger(log_level = 6)
        expect_error(res <- run_rbc(test_data$pre_process_data, 
                                    window_size = 487, 
                                    threshold_year = 365), 
                     NA)
        initialize_logger(log_level = 8)
        expect_error(res <- run_rbc(test_data$pre_process_data, 
                                    window_size = 487, 
                                    threshold_year = 365), 
                     NA)
        initialize_logger(log_level = 9)
        expect_error(res <- run_rbc(test_data$pre_process_data, 
                                    window_size = 487, 
                                    threshold_year = 365), 
                     NA)
    })
    
    it("fail cases", {
        expect_error(res <- initialize_logger(log_level = NA), 
                     "*is missing*")
        expect_error(res <- initialize_logger(log_level = 10), 
                     "*invalid value*")
        expect_error(res <- initialize_logger(log_level = "xyz"), 
                     "*is non-numeric*")
    })

})

describe("run rbc of happy cases", {
    test_data <- setup_data(100, 10)
    it("Without parallel", {
        expect_error(res <- run_rbc(test_data$pre_process_data, 
                                    window_size = 487, 
                                    threshold_year = 365), 
                     NA)
    })
    
    it("With parallel", {
        expect_error(res <- run_rbc(test_data$pre_process_data, 
                                    window_size = 487, 
                                    threshold_year = 365,
                                    parallel = TRUE,
                                    n_core = 1), 
                     NA)
    })
    
    it("Without pre_process", {
        expect_error(res <- run_rbc(test_data$data, 
                                    window_size = 487, 
                                    threshold_year = 365), 
                     NA)
    })
})

describe("run rbc of unhappy cases", {
    test_data <- setup_data(100, 10)
    it("Invalid window_size", {
        expect_error(res <- run_rbc(test_data$pre_process_data, 
                                    window_size = "fddsaf",
                                    threshold_year = 365), 
                     "*is non-numeric*")
    })
    it("Invalid threshold_year", {
        expect_error(res <- run_rbc(test_data$pre_process_data,
                                    window_size = 487, 
                                    threshold_year = "fddsaf"), 
                     "*is non-numeric*")
    })
    
    it("Invalid data object - NULL", {
        expect_error(res <- run_rbc(NULL,
                                    window_size = 487, 
                                    threshold_year = 365), 
                     "*must be an object of 'migrbc_preprocess'*")
    })
    
    it("Invalid data object - List NULL", {
        expect_error(res <- run_rbc(list(NULL),
                                    window_size = 487, 
                                    threshold_year = 365), 
                     "*must be an object of 'migrbc_preprocess'*")
    })
    
    it("Invalid data object - List 'xyz'", {
        expect_error(res <- run_rbc(list("xyz"),
                                    window_size = 487, 
                                    threshold_year = 365), 
                     "*must be an object of 'migrbc_preprocess'*")
    })

})
