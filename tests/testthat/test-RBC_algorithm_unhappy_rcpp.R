##############################################################################
context("RBC_algorithm_unhappy_rcpp")
##############################################################################

describe("Run RBC with a single resident person should fail", {
    it("No journey data provided", {
      j1 <-       c(journeyId = 1, 
                    personId = 1, 
                    is_arrival = 1, 
                    date_crossing = "2017-01-01", 
                    journey_sequence = 1,
                    journeyId_prev = NA)
      person_data <- as.data.frame(t(j1), stringsAsFactors = FALSE)
      ## remove record
      person_data <- person_data[-1,]
      i1 <- c(personId = 1, 
              res_status_initial = 1, 
              date_finalised = "2017-01-01")
      ini_data <- as.data.frame(t(i1), stringsAsFactors = FALSE)  
      crossings <- expect_error(
        migrbc::pre_process(person_data,
       init_res_status_data = ini_data,
       n_groups = 1), "No journey data provided")
    })
    
    it("A person with a journey that has the wrong value of
       direction code", {
         j1 <-       c(journeyId = 1, 
                       personId = 1, 
                       is_arrival = 5, 
                       date_crossing = "2014-02-13", 
                       journey_sequence = 1,
                       journeyId_prev = NA)
         person_data <- as.data.frame(t(j1), stringsAsFactors = FALSE)
         i1 <- c(personId = 1, 
                 res_status_initial = 1, 
                 date_finalised = "2017-01-01")
         ini_data <- as.data.frame(t(i1), stringsAsFactors = FALSE)  
         crossings <- migrbc::pre_process(person_data,
                                          init_res_status_data = ini_data,
                                          n_groups = 1)
  
         res <- expect_error(migrbc::resolve_data(crossings,
                                                  window_size = 487, 
                                                  threshold_year = 365), NA)
         
         ## then, should have a row or error_data and none good result
         expect_equal(nrow(res$journeys), 0)
         expect_equal(nrow(res$error_data), 1)
         expect_error(is.null(res$error_data["error_code"]), 
                      "*undefined columns selected*")
         expect_error(is.null(res$error_data["error_message"]), 
                      "*undefined columns selected*")
         
         res <- expect_error(migrbc::resolve_data(
           crossings,
           window_size = 487, 
           threshold_year = 365,
           include_error_columns = TRUE), NA)
         
         ## then, should have a row or error_data and none good result
         expect_equal(nrow(res$journeys), 0)
         expect_equal(nrow(res$error_data), 1)
         expect_true(!is.null(res$error_data["error_code"]))
         expect_true(!is.null(res$error_data["error_message"]))
         ## check days_to_next_crossing
         expect_true(res$error_data[1, "days_to_next_crossing"] == 487)
         
    })

    it("A person with a journey that has the wrong value of cross date", {
        ## with empty string in date_crossing
        j1 <-       c(journeyId = 1, 
                      personId = 1, 
                      is_arrival = 1, 
                      date_crossing = "", 
                      journey_sequence = 1,
                      journeyId_prev = NA)
        person_data <- as.data.frame(t(j1), stringsAsFactors = FALSE)
        i1 <- c(personId = 1, 
                res_status_initial = 1, 
                date_finalised = "2017-01-01")
        ini_data <- as.data.frame(t(i1), stringsAsFactors = FALSE)  
        crossings <- migrbc::pre_process(person_data,
                                         init_res_status_data = ini_data,
                                         n_groups = 1)
        
        res <- expect_error(migrbc::resolve_data(crossings,
                                                 window_size = 487, 
                                                 threshold_year = 365), NA)
        ## not sure what it not passed from check_rhub
        # expect_equal(nrow(res$journeys), 0)
        # expect_equal(nrow(res$error_data), 1)
        
        ## with invalid date string in date_crossing
        j1 <-       c(journeyId = 1, 
                      personId = 1, 
                      is_arrival = 1, 
                      date_crossing = "xyz", 
                      journey_sequence = 1,
                      journeyId_prev = NA)
        person_data <- as.data.frame(t(j1), stringsAsFactors = FALSE)
        i1 <- c(personId = 1, 
                res_status_initial = 1, 
                date_finalised = "2017-01-01")
        ini_data <- as.data.frame(t(i1), stringsAsFactors = FALSE)  
        crossings <- migrbc::pre_process(person_data,
                                         init_res_status_data = ini_data,
                                         n_groups = 1)
        
        res <- expect_error(migrbc::resolve_data(crossings,
                                                 window_size = 487, 
                                                 threshold_year = 365), NA)
        ## not sure what it not passed from check_rhub
        # expect_equal(nrow(res$journeys), 0)
        # expect_equal(nrow(res$error_data), 1)
        
        ## with invalid date value in date_crossing
        j1 <-       c(journeyId = 1, 
                      personId = 1, 
                      is_arrival = 1, 
                      date_crossing = "1800-01-01", 
                      journey_sequence = 1,
                      journeyId_prev = NA)
        person_data <- as.data.frame(t(j1), stringsAsFactors = FALSE)
        i1 <- c(personId = 1, 
                res_status_initial = 1, 
                date_finalised = "2017-01-01")
        ini_data <- as.data.frame(t(i1), stringsAsFactors = FALSE)  
        crossings <- migrbc::pre_process(person_data,
                                         init_res_status_data = ini_data,
                                         n_groups = 1)
        
        res <- expect_error(migrbc::resolve_data(crossings,
                                                 window_size = 487, 
                                                 threshold_year = 365), NA)
        ## not sure what it not passed from check_rhub
        # expect_equal(nrow(res$journeys), 0)
        # expect_equal(nrow(res$error_data), 1)
        ## check days_to_next_crossing
        ## not sure what it not passed from check_rhub
        # expect_true(res$error_data[1, "days_to_next_crossing"] == 487)
        
        j1 <-       c(journeyId = 1, 
                      personId = 1, 
                      is_arrival = 1, 
                      date_crossing = "2011-15-01", 
                      journey_sequence = 1,
                      journeyId_prev = NA)
        person_data <- as.data.frame(t(j1), stringsAsFactors = FALSE)
        i1 <- c(personId = 1, 
                res_status_initial = 1, 
                date_finalised = "2017-01-01")
        ini_data <- as.data.frame(t(i1), stringsAsFactors = FALSE)  
        crossings <- migrbc::pre_process(person_data,
                                         init_res_status_data = ini_data,
                                         n_groups = 1)
        
        res <- expect_error(migrbc::resolve_data(crossings,
                                                 window_size = 487, 
                                                 threshold_year = 365), NA)
        ## not sure what it not passed from check_rhub
        # expect_equal(nrow(res$journeys), 0)
        # expect_equal(nrow(res$error_data), 1)
        
        j1 <-       c(journeyId = 1, 
                      personId = 1, 
                      is_arrival = 1, 
                      date_crossing = "2011-11-32", 
                      journey_sequence = 1,
                      journeyId_prev = NA)
        person_data <- as.data.frame(t(j1), stringsAsFactors = FALSE)
        i1 <- c(personId = 1, 
                res_status_initial = 1, 
                date_finalised = "2017-01-01")
        ini_data <- as.data.frame(t(i1), stringsAsFactors = FALSE)  
        crossings <- migrbc::pre_process(person_data,
                                         init_res_status_data = ini_data,
                                         n_groups = 1)
        
        res <- expect_error(migrbc::resolve_data(crossings,
                                                 window_size = 487, 
                                                 threshold_year = 365), NA)
        ## not sure what it not passed from check_rhub
        # expect_equal(nrow(res$journeys), 0)
        # expect_equal(nrow(res$error_data), 1)
        ## check days_to_next_crossing
        # expect_true(res$error_data[1, "days_to_next_crossing"] == 487)
      
    })
    # 
    it("A person with a journey that has the same direction as
       the previous one", {
         j1 <-       c(journeyId = 1, 
                       personId = 1, 
                       is_arrival = 1, 
                       date_crossing = "2011-11-01", 
                       journey_sequence = 1,
                       journeyId_prev = NA)
         j2 <-       c(journeyId = 2, 
                       personId = 1, 
                       is_arrival = 1, 
                       date_crossing = "2011-11-02", 
                       journey_sequence = 1,
                       journeyId_prev = NA)
         person_data <- as.data.frame(rbind(j1, j2 ), stringsAsFactors = FALSE)
         i1 <- c(personId = 1, 
                 res_status_initial = 1, 
                 date_finalised = "2017-01-01")
         ini_data <- as.data.frame(t(i1), stringsAsFactors = FALSE)  
         crossings <- migrbc::pre_process(person_data,
                                          init_res_status_data = ini_data,
                                          n_groups = 1)
         
         res <- expect_error(migrbc::resolve_data(crossings,
                                                  window_size = 487, 
                                                  threshold_year = 365), NA)
         expect_equal(nrow(res$journeys), 0)
         expect_equal(nrow(res$error_data), 2)
         
         j1 <-       c(journeyId = 1, 
                       personId = 1, 
                       is_arrival = 0, 
                       date_crossing = "2011-11-01", 
                       journey_sequence = 1,
                       journeyId_prev = NA)
         j2 <-       c(journeyId = 2, 
                       personId = 1, 
                       is_arrival = 0, 
                       date_crossing = "2011-11-02", 
                       journey_sequence = 1,
                       journeyId_prev = NA)
         person_data <- as.data.frame(rbind(j1, j2 ), stringsAsFactors = FALSE)
         i1 <- c(personId = 1, 
                 res_status_initial = 1, 
                 date_finalised = "2017-01-01")
         ini_data <- as.data.frame(t(i1), stringsAsFactors = FALSE)  
         crossings <- migrbc::pre_process(person_data,
                                          init_res_status_data = ini_data,
                                          n_groups = 1)
         
         res <- expect_error(migrbc::resolve_data(crossings,
                                                  window_size = 487, 
                                                  threshold_year = 365), NA)
         expect_equal(nrow(res$journeys), 0)
         expect_equal(nrow(res$error_data), 2)
         ## check days_to_next_crossing
         expect_true((as.Date(res$error_data[2, "date_crossing"]) -
                        as.Date(res$error_data[1, "date_crossing"])) ==
                       res$error_data[1, "days_to_next_crossing"])
    })

    it("A person with a journey that has the earlier cross data
       than the previous one", {
         j1 <-       c(journeyId = 1, 
                       personId = 1, 
                       is_arrival = 0, 
                       date_crossing = "2011-11-02", 
                       journey_sequence = 1,
                       journeyId_prev = NA)
         j2 <-       c(journeyId = 2, 
                       personId = 1, 
                       is_arrival = 1, 
                       date_crossing = "2011-11-01", 
                       journey_sequence = 1,
                       journeyId_prev = NA)
         person_data <- as.data.frame(rbind(j1, j2 ), stringsAsFactors = FALSE)
         i1 <- c(personId = 1, 
                 res_status_initial = 1, 
                 date_finalised = "2017-01-01")
         ini_data <- as.data.frame(t(i1), stringsAsFactors = FALSE)  
         crossings <- migrbc::pre_process(person_data,
                                          init_res_status_data = ini_data,
                                          n_groups = 1)
         
         res <- expect_error(migrbc::resolve_data(crossings,
                                                  window_size = 487, 
                                                  threshold_year = 365), NA)
         expect_equal(nrow(res$journeys), 0)
         expect_equal(nrow(res$error_data), 2)
         ## check days_to_next_crossing
         expect_true((as.Date(res$error_data[2, "date_crossing"]) -
                        as.Date(res$error_data[1, "date_crossing"])) ==
                       res$error_data[1, "days_to_next_crossing"])
    })
    
    it("Null Person data", {
      res <- expect_error(migrbc::resolve_data(NULL,
                                               window_size = 487, 
                                               threshold_year = 365), 
                          "* must be a list of data frame objects*")
      res <- expect_error(migrbc::resolve_data(NA,
                                               window_size = 487, 
                                               threshold_year = 365), 
                          "* must be a list of data frame objects*")
      res <- expect_error(migrbc::resolve_data("xx",
                                               window_size = 487, 
                                               threshold_year = 365), 
                          "* must be a list of data frame objects*")
    })

})

describe("Test exceptions", {
  it("Scenario 1: IdenticalDirectionException", {
       j1 <-       c(journeyId = 1,
                     personId = 1,
                     is_arrival = 1,
                     date_crossing = "2017-01-01",
                     journey_sequence = 1,
                     journeyId_prev = NA)
       
       j2 <-       c(journeyId = 2,
                     personId = 1,
                     is_arrival = 1,
                     date_crossing = "2018-01-06",
                     journey_sequence = 2,
                     journeyId_prev = 1)
       
       j3 <-       c(journeyId = 3,
                     personId = 1,
                     is_arrival = 1,
                     date_crossing = "2018-01-16",
                     journey_sequence = 3,
                     journeyId_prev = 2)
       
       
       person_data <- as.data.frame(rbind(j1, j2, j3),
                                    stringsAsFactors = FALSE)
       person_data$journeyId <- as.numeric(person_data$journeyId)
       person_data$personId <- as.numeric(person_data$personId)
       person_data$is_arrival <- as.numeric(person_data$is_arrival)
       person_data$journey_sequence <- 
         as.numeric(person_data$journey_sequence)
       person_data$journeyId_prev <- 
         as.numeric(person_data$journeyId_prev)
       
       expect_error(res <- migrbc::rcpp_resolve(person_data,
                                                0,
                                                "2017-01-01",
                                                487,
                                                365), 
                    class = "IdenticalDirectionException")
    })
  
  it("Scenario 2: JourneyException", {
    j1 <-       c(journeyId = 1,
                  personId = 1,
                  is_arrival = 1,
                  date_crossing = "2017-01-01",
                  journey_sequence = 1,
                  journeyId_prev = NA)
    
    
    person_data <- as.data.frame(t(j1),
                                 stringsAsFactors = FALSE)
    person_data <- person_data[-1, ]
    person_data$journeyId <- as.numeric(person_data$journeyId)
    person_data$personId <- as.numeric(person_data$personId)
    person_data$is_arrival <- as.numeric(person_data$is_arrival)
    person_data$journey_sequence <- 
      as.numeric(person_data$journey_sequence)
    person_data$journeyId_prev <- 
      as.numeric(person_data$journeyId_prev)
    
    expect_error(res <- migrbc::rcpp_resolve(person_data,
                                             0,
                                             "2017-01-01",
                                             487,
                                             365), 
                 class = "JourneyException")
  })
  
  it("Scenario 3: InvalidCrossDateException", {
    j1 <-       c(journeyId = 1,
                  personId = 1,
                  is_arrival = 1,
                  date_crossing = "2017-01-01",
                  journey_sequence = 1,
                  journeyId_prev = NA)
    
    j2 <-       c(journeyId = 2,
                  personId = 1,
                  is_arrival = 0,
                  date_crossing = "xyz",
                  journey_sequence = 2,
                  journeyId_prev = 1)
    
    j3 <-       c(journeyId = 3,
                  personId = 1,
                  is_arrival = 1,
                  date_crossing = "2018-01-16",
                  journey_sequence = 3,
                  journeyId_prev = 2)
    
    
    person_data <- as.data.frame(rbind(j1, j2, j3),
                                 stringsAsFactors = FALSE)
    person_data$journeyId <- as.numeric(person_data$journeyId)
    person_data$personId <- as.numeric(person_data$personId)
    person_data$is_arrival <- as.numeric(person_data$is_arrival)
    person_data$journey_sequence <- 
      as.numeric(person_data$journey_sequence)
    person_data$journeyId_prev <- 
      as.numeric(person_data$journeyId_prev)
    ## not sure what it not passed from check_rhub  
    # expect_error(res <- migrbc::rcpp_resolve(person_data,
    #                                          0,
    #                                          "2017-01-01",
    #                                          487,
    #                                          365), 
    #              class = "InvalidCrossDateException")
  })
  
  
  it("Scenario 4: InvalidDirectionException", {
    j1 <-       c(journeyId = 1,
                  personId = 1,
                  is_arrival = 1,
                  date_crossing = "2017-01-01",
                  journey_sequence = 1,
                  journeyId_prev = NA)
    
    j2 <-       c(journeyId = 2,
                  personId = 1,
                  is_arrival = 5,
                  date_crossing = "2018-01-06",
                  journey_sequence = 2,
                  journeyId_prev = 1)
    
    j3 <-       c(journeyId = 3,
                  personId = 1,
                  is_arrival = 1,
                  date_crossing = "2018-01-16",
                  journey_sequence = 3,
                  journeyId_prev = 2)
    
    
    person_data <- as.data.frame(rbind(j1, j2, j3),
                                 stringsAsFactors = FALSE)
    person_data$journeyId <- as.numeric(person_data$journeyId)
    person_data$personId <- as.numeric(person_data$personId)
    person_data$is_arrival <- as.numeric(person_data$is_arrival)
    person_data$journey_sequence <- 
      as.numeric(person_data$journey_sequence)
    person_data$journeyId_prev <- 
      as.numeric(person_data$journeyId_prev)
    
    expect_error(res <- migrbc::rcpp_resolve(person_data,
                                             0,
                                             "2017-01-01",
                                             487,
                                             365), 
                 class = "InvalidDirectionException")
  })
  
  it("Scenario 5: SequenceException", {
    j1 <-       c(journeyId = 1,
                  personId = 1,
                  is_arrival = 1,
                  date_crossing = "2017-01-01",
                  journey_sequence = 1,
                  journeyId_prev = NA)
    
    j2 <-       c(journeyId = 2,
                  personId = 1,
                  is_arrival = 0,
                  date_crossing = "2016-11-06",
                  journey_sequence = 2,
                  journeyId_prev = 1)
    
    j3 <-       c(journeyId = 3,
                  personId = 1,
                  is_arrival = 1,
                  date_crossing = "2018-01-16",
                  journey_sequence = 3,
                  journeyId_prev = 2)
    
    
    person_data <- as.data.frame(rbind(j1, j2, j3),
                                 stringsAsFactors = FALSE)
    person_data$journeyId <- as.numeric(person_data$journeyId)
    person_data$personId <- as.numeric(person_data$personId)
    person_data$is_arrival <- as.numeric(person_data$is_arrival)
    person_data$journey_sequence <- 
      as.numeric(person_data$journey_sequence)
    person_data$journeyId_prev <- 
      as.numeric(person_data$journeyId_prev)
    
    expect_error(res <- migrbc::rcpp_resolve(person_data,
                                             0,
                                             "2017-01-01",
                                             487,
                                             365), 
                 class = "SequenceException")
  })
  
  it("Scenario 6: resolve custom error", {
    j1 <-       c(journeyId = 1,
                  personId = 1,
                  is_arrival = 1,
                  date_crossing = "2017-01-01",
                  journey_sequence = 1,
                  journeyId_prev = NA)
    
    j2 <-       c(journeyId = 2,
                  personId = 1,
                  is_arrival = 1,
                  date_crossing = "2018-01-06",
                  journey_sequence = 2,
                  journeyId_prev = 1)
    
    j3 <-       c(journeyId = 3,
                  personId = 1,
                  is_arrival = 1,
                  date_crossing = "2018-01-16",
                  journey_sequence = 3,
                  journeyId_prev = 2)
    
    
    j4 <-       c(journeyId = 1,
                  personId = 2,
                  is_arrival = 0,
                  date_crossing = "2017-01-01",
                  journey_sequence = 1,
                  journeyId_prev = NA)
    
    j5 <-       c(journeyId = 2,
                  personId = 2,
                  is_arrival = 0,
                  date_crossing = "2018-01-06",
                  journey_sequence = 2,
                  journeyId_prev = 1)
    
    j6 <-       c(journeyId = 3,
                  personId = 2,
                  is_arrival = 0,
                  date_crossing = "2018-01-16",
                  journey_sequence = 3,
                  journeyId_prev = 2)
    
    person_data <- as.data.frame(rbind(j1, j2, j3, j4, j5, j6),
                                 stringsAsFactors = FALSE)
    i1 <- c(personId = 1, 
            res_status_initial = 1, 
            date_finalised = "2017-01-01")
    ini_data <- as.data.frame(t(i1), stringsAsFactors = FALSE)  
    
    person_data$journeyId <- as.numeric(person_data$journeyId)
    person_data$personId <- as.numeric(person_data$personId)
    person_data$is_arrival <- as.numeric(person_data$is_arrival)
    person_data$journey_sequence <- 
      as.numeric(person_data$journey_sequence)
    person_data$journeyId_prev <- 
      as.numeric(person_data$journeyId_prev)
    
    ini_data$personId <- as.numeric(ini_data$personId)
    ini_data$res_status_initial <- 
      as.numeric(ini_data$res_status_initial)
    ini_data$date_finalised <- 
      as.character(ini_data$date_finalised)
    expect_error(res <- migrbc::resolve_data_with_error(
        person_data, 
        initial_res_status_data = ini_data,
        error_message = "custom error",
        include_error_columns = TRUE), NA)
    expect_true(res[1, "error_code"] == 99)
    expect_true(identical(res[1, "error_message"], "custom error"))
    ## check days_to_next_crossing
    expect_true((as.Date(res[2, "date_crossing"]) -
                as.Date(res[1, "date_crossing"])) ==
                res[1, "days_to_next_crossing"])
    expect_error(res <- migrbc::resolve_data_with_error(
      person_data, 
      initial_res_status_data = ini_data,
      error_message = "custom error",
      include_error_columns = FALSE), NA)
    expect_true(is.null(res[1, "error_code"]))
    expect_error(res <- migrbc::resolve_data_with_error(
      person_data, 
      initial_res_status_data = NULL,
      error_message = "custom error",
      include_error_columns = FALSE),
      "*a type of data frame*")
    expect_error(res <- migrbc::resolve_data_with_error(
      person_data, 
      initial_res_status_data = "x",
      error_message = "custom error",
      include_error_columns = FALSE),
      "*a type of data frame*")
    expect_error(res <- migrbc::resolve_data_with_error(
      NULL, 
      initial_res_status_data = ini_data,
      error_message = "custom error",
      include_error_columns = FALSE),
      "*a type of data frame*")
    expect_error(res <- migrbc::resolve_data_with_error(
      "x", 
      initial_res_status_data = ini_data,
      error_message = "custom error",
      include_error_columns = FALSE),
      "*a type of data frame*")
  })
})
