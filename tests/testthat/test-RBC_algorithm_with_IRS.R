##############################################################################
context("RBC_algorithm_with_IRS")
##############################################################################


describe("Run RBC with a single person should succeed", {
  it("Scenario 1: with IRS at the beginning, IRS = TRUE", {
      j1 <-       c(journeyId = 1,
                    personId = 1,
                    is_arrival = 0,
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
                    is_arrival = 0,
                    date_crossing = "2018-01-16",
                    journey_sequence = 3,
                    journeyId_prev = 2)
      
      j4 <-       c(journeyId = 4,
                    personId = 1,
                    is_arrival = 1,
                    date_crossing = "2018-11-02",
                    journey_sequence = 4,
                    journeyId_prev = 3)
      
      j5 <-       c(journeyId = 5,
                    personId = 1,
                    is_arrival = 0,
                    date_crossing = "2018-11-12",
                    journey_sequence = 5,
                    journeyId_prev = 4)
      
      person_data <- as.data.frame(rbind(j1, j2, j3, j4, j5),
                                   stringsAsFactors = FALSE)
      
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
      res <- res$journeys
 
      ## check rcpp_resolve jouney 1
      expect_true(identical(as.numeric(res$res_status_before[[1]]), 1))
      expect_true(identical(as.numeric(res$res_status_after[[1]]), 0))
      expect_true(identical(as.numeric(res$is_long_term_mig[[1]]), 1))
      
      expect_true(as.numeric(as.Date(res$date_finalised_res_before[[1]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 0)
      expect_true(as.numeric(as.Date(res$date_finalised_res_after[[1]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 365)
      expect_true(as.numeric(as.Date(res$date_finalised_LTM[[1]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 365)
      
      expect_true(identical(as.numeric(res$res_status_before[[2]]), 0))
      expect_true(identical(as.numeric(res$res_status_after[[2]]), 0))
      expect_true(identical(as.numeric(res$is_long_term_mig[[2]]), 0))
      
      expect_true(as.numeric(as.Date(res$date_finalised_res_before[[2]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 370)
      expect_true(as.numeric(as.Date(res$date_finalised_res_after[[2]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 502)
      expect_true(as.numeric(as.Date(res$date_finalised_LTM[[2]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 502)
      
      expect_true(identical(as.numeric(res$res_status_before[[3]]), 0))
      expect_true(identical(as.numeric(res$res_status_after[[3]]), 0))
      expect_true(identical(as.numeric(res$is_long_term_mig[[3]]), 0))
      
      expect_true(as.numeric(as.Date(res$date_finalised_res_before[[3]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 502)
      expect_true(as.numeric(as.Date(res$date_finalised_res_after[[3]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 502)
      expect_true(as.numeric(as.Date(res$date_finalised_LTM[[3]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 380)
      
      expect_true(identical(as.numeric(res$res_status_before[[4]]), 0))
      expect_true(identical(as.numeric(res$res_status_after[[4]]), 0))
      expect_true(identical(as.numeric(res$is_long_term_mig[[4]]), 0))
      
      expect_true(as.numeric(as.Date(res$date_finalised_res_before[[4]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 670)
      expect_true(as.numeric(as.Date(res$date_finalised_res_after[[4]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 802)
      expect_true(as.numeric(as.Date(res$date_finalised_LTM[[4]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 802)
      
      expect_true(identical(as.numeric(res$res_status_before[[5]]), 0))
      expect_true(identical(as.numeric(res$res_status_after[[5]]), 0))
      expect_true(identical(as.numeric(res$is_long_term_mig[[5]]), 0))
      
      expect_true(as.numeric(as.Date(res$date_finalised_res_before[[5]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 802)
      expect_true(as.numeric(as.Date(res$date_finalised_res_after[[5]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 802)
      expect_true(as.numeric(as.Date(res$date_finalised_LTM[[5]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 680)
      
        
  })
  
  it("Scenario 2: with IRS at the beginning, IRS = TRUE,
     finalised_date = '2018-05-18'", {
       j1 <-       c(journeyId = 1,
                     personId = 1,
                     is_arrival = 0,
                     date_crossing = "2017-01-01",
                     journey_sequence = 1,
                     journeyId_prev = NA)
       
       j2 <-       c(journeyId = 12,
                     personId = 1,
                     is_arrival = 1,
                     date_crossing = "2018-01-06",
                     journey_sequence = 2,
                     journeyId_prev = 1)
       
       j3 <-       c(journeyId = 3,
                     personId = 1,
                     is_arrival = 0,
                     date_crossing = "2018-01-16",
                     journey_sequence = 3,
                     journeyId_prev = 2)
       
       j4 <-       c(journeyId = 4,
                     personId = 1,
                     is_arrival = 1,
                     date_crossing = "2018-11-02",
                     journey_sequence = 4,
                     journeyId_prev = 3)
       
       j5 <-       c(journeyId = 5,
                     personId = 1,
                     is_arrival = 0,
                     date_crossing = "2018-11-12",
                     journey_sequence = 5,
                     journeyId_prev = 4)
       
       person_data <- as.data.frame(rbind(j1, j2, j3, j4, j5),
                                    stringsAsFactors = FALSE)
       
       i1 <- c(personId = 1,
               res_status_initial = 1,
               date_finalised = "2018-05-18")
       
       ini_data <- as.data.frame(t(i1), stringsAsFactors = FALSE)
       crossings <- migrbc::pre_process(person_data,
                                        init_res_status_data = ini_data,
                                        n_groups = 1)
       res <- expect_error(migrbc::resolve_data(crossings,
                                                window_size = 487,
                                                threshold_year = 365), NA)
       res <- res$journeys
       
       ## check rcpp_resolve jouney 1
       expect_true(identical(as.numeric(res$res_status_before[[1]]), 1))
       expect_true(identical(as.numeric(res$res_status_after[[1]]), 0))
       expect_true(identical(as.numeric(res$is_long_term_mig[[1]]), 1))
       
       expect_true(as.numeric(as.Date(res$date_finalised_res_before[[1]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 0)
       expect_true(as.numeric(as.Date(res$date_finalised_res_after[[1]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 365)
       expect_true(as.numeric(as.Date(res$date_finalised_LTM[[1]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 365)
       
       expect_true(identical(as.numeric(res$res_status_before[[2]]), 0))
       expect_true(identical(as.numeric(res$res_status_after[[2]]), 0))
       expect_true(identical(as.numeric(res$is_long_term_mig[[2]]), 0))
       
       expect_true(as.numeric(as.Date(res$date_finalised_res_before[[2]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 370)
       expect_true(as.numeric(as.Date(res$date_finalised_res_after[[2]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 502)
       expect_true(as.numeric(as.Date(res$date_finalised_LTM[[2]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 502)
       
       ## check rcpp_resolve jouney 3
       ## The follow residentBefore has been updated by the initial resident
       ## status because the previous residentAfter finalised date is equal to
       ## the initial finalised date.
       expect_true(identical(as.numeric(res$res_status_before[[3]]), 1))
       expect_true(identical(as.numeric(res$res_status_after[[3]]), 0))
       expect_true(identical(as.numeric(res$is_long_term_mig[[3]]), 1))
       
       expect_true(as.numeric(as.Date(res$date_finalised_res_before[[3]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 502)
       expect_true(as.numeric(as.Date(res$date_finalised_res_after[[3]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 755)
       expect_true(as.numeric(as.Date(res$date_finalised_LTM[[3]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 755)
       
       expect_true(identical(as.numeric(res$res_status_before[[4]]), 0))
       expect_true(identical(as.numeric(res$res_status_after[[4]]), 0))
       expect_true(identical(as.numeric(res$is_long_term_mig[[4]]), 0))
       
       expect_true(as.numeric(as.Date(res$date_finalised_res_before[[4]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 755)
       expect_true(as.numeric(as.Date(res$date_finalised_res_after[[4]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 802)
       expect_true(as.numeric(as.Date(res$date_finalised_LTM[[4]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 670)
       
       expect_true(identical(as.numeric(res$res_status_before[[5]]), 0))
       expect_true(identical(as.numeric(res$res_status_after[[5]]), 0))
       expect_true(identical(as.numeric(res$is_long_term_mig[[5]]), 0))
       
       expect_true(as.numeric(as.Date(res$date_finalised_res_before[[5]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 802)
       expect_true(as.numeric(as.Date(res$date_finalised_res_after[[5]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 802)
       expect_true(as.numeric(as.Date(res$date_finalised_LTM[[5]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 755)
  })

  it("Scenario 3: with IRS at the beginning, IRS = FALSE", {
    j1 <-       c(journeyId = 1,
                  personId = 1,
                  is_arrival = 1,
                  date_crossing = "2017-01-01",
                  journey_sequence = 1,
                  journeyId_prev = NA)
    
    j2 <-       c(journeyId = 2,
                  personId = 1,
                  is_arrival = 0,
                  date_crossing = "2018-01-06",
                  journey_sequence = 2,
                  journeyId_prev = 1)
    
    j3 <-       c(journeyId = 3,
                  personId = 1,
                  is_arrival = 1,
                  date_crossing = "2018-01-16",
                  journey_sequence = 3,
                  journeyId_prev = 2)
    
    j4 <-       c(journeyId = 4,
                  personId = 1,
                  is_arrival = 0,
                  date_crossing = "2018-11-02",
                  journey_sequence = 4,
                  journeyId_prev = 3)
    
    j5 <-       c(journeyId = 5,
                  personId = 1,
                  is_arrival = 1,
                  date_crossing = "2018-11-12",
                  journey_sequence = 5,
                  journeyId_prev = 4)
    
    person_data <- as.data.frame(rbind(j1, j2, j3, j4, j5),
                                 stringsAsFactors = FALSE)
    
    i1 <- c(personId = 1,
            res_status_initial = 0,
            date_finalised = "2017-01-01")
    
    ini_data <- as.data.frame(t(i1), stringsAsFactors = FALSE)
    crossings <- migrbc::pre_process(person_data,
                                     init_res_status_data = ini_data,
                                     n_groups = 1)
    res <- expect_error(migrbc::resolve_data(crossings,
                                             window_size = 487,
                                             threshold_year = 365), NA)
    res <- res$journeys
    
    ## check rcpp_resolve jouney 1
    expect_true(identical(as.numeric(res$res_status_before[[1]]), 0))
    expect_true(identical(as.numeric(res$res_status_after[[1]]), 1))
    expect_true(identical(as.numeric(res$is_long_term_mig[[1]]), 1))
    
    expect_true(as.numeric(as.Date(res$date_finalised_res_before[[1]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 0)
    expect_true(as.numeric(as.Date(res$date_finalised_res_after[[1]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 365)
    expect_true(as.numeric(as.Date(res$date_finalised_LTM[[1]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 365)
    
    expect_true(identical(as.numeric(res$res_status_before[[2]]), 1))
    expect_true(identical(as.numeric(res$res_status_after[[2]]), 1))
    expect_true(identical(as.numeric(res$is_long_term_mig[[2]]), 0))
    
    expect_true(as.numeric(as.Date(res$date_finalised_res_before[[2]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 370)
    expect_true(as.numeric(as.Date(res$date_finalised_res_after[[2]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 502)
    expect_true(as.numeric(as.Date(res$date_finalised_LTM[[2]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 502)
    
    expect_true(identical(as.numeric(res$res_status_before[[3]]), 1))
    expect_true(identical(as.numeric(res$res_status_after[[3]]), 1))
    expect_true(identical(as.numeric(res$is_long_term_mig[[3]]), 0))
    
    expect_true(as.numeric(as.Date(res$date_finalised_res_before[[3]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 502)
    expect_true(as.numeric(as.Date(res$date_finalised_res_after[[3]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 502)
    expect_true(as.numeric(as.Date(res$date_finalised_LTM[[3]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 380)
    
    expect_true(identical(as.numeric(res$res_status_before[[4]]), 1))
    expect_true(identical(as.numeric(res$res_status_after[[4]]), 1))
    expect_true(identical(as.numeric(res$is_long_term_mig[[4]]), 0))
    
    expect_true(as.numeric(as.Date(res$date_finalised_res_before[[4]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 670)
    expect_true(as.numeric(as.Date(res$date_finalised_res_after[[4]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 802)
    expect_true(as.numeric(as.Date(res$date_finalised_LTM[[4]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 802)
    
    expect_true(identical(as.numeric(res$res_status_before[[5]]), 1))
    expect_true(identical(as.numeric(res$res_status_after[[5]]), 1))
    expect_true(identical(as.numeric(res$is_long_term_mig[[5]]), 0))
    
    expect_true(as.numeric(as.Date(res$date_finalised_res_before[[5]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 802)
    expect_true(as.numeric(as.Date(res$date_finalised_res_after[[5]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 802)
    expect_true(as.numeric(as.Date(res$date_finalised_LTM[[5]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 680)
  })

  it("Scenario 4: with IRS at the beginning, IRS = FALSE,
     finalised_date = '2018-05-18'", {
       
       j1 <-       c(journeyId = 1,
                     personId = 1,
                     is_arrival = 1,
                     date_crossing = "2017-01-01",
                     journey_sequence = 1,
                     journeyId_prev = NA)
       
       j2 <-       c(journeyId = 2,
                     personId = 1,
                     is_arrival = 0,
                     date_crossing = "2018-01-06",
                     journey_sequence = 2,
                     journeyId_prev = 1)
       
       j3 <-       c(journeyId = 3,
                     personId = 1,
                     is_arrival = 1,
                     date_crossing = "2018-01-16",
                     journey_sequence = 3,
                     journeyId_prev = 2)
       
       j4 <-       c(journeyId = 4,
                     personId = 1,
                     is_arrival = 0,
                     date_crossing = "2018-11-02",
                     journey_sequence = 4,
                     journeyId_prev = 3)
       
       j5 <-       c(journeyId = 5,
                     personId = 1,
                     is_arrival = 1,
                     date_crossing = "2018-11-12",
                     journey_sequence = 5,
                     journeyId_prev = 4)
       
       person_data <- as.data.frame(rbind(j1, j2, j3, j4, j5),
                                    stringsAsFactors = FALSE)
       
       i1 <- c(personId = 1,
               res_status_initial = 0,
               date_finalised = "2018-05-18")
       
       ini_data <- as.data.frame(t(i1), stringsAsFactors = FALSE)
       crossings <- migrbc::pre_process(person_data,
                                        init_res_status_data = ini_data,
                                        n_groups = 1)
       res <- expect_error(migrbc::resolve_data(crossings,
                                                window_size = 487,
                                                threshold_year = 365), NA)
       res <- res$journeys
       
       ## check rcpp_resolve jouney 1
       expect_true(identical(as.numeric(res$res_status_before[[1]]), 0))
       expect_true(identical(as.numeric(res$res_status_after[[1]]), 1))
       expect_true(identical(as.numeric(res$is_long_term_mig[[1]]), 1))
       
       expect_true(as.numeric(as.Date(res$date_finalised_res_before[[1]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 0)
       expect_true(as.numeric(as.Date(res$date_finalised_res_after[[1]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 365)
       expect_true(as.numeric(as.Date(res$date_finalised_LTM[[1]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 365)
       
       expect_true(identical(as.numeric(res$res_status_before[[2]]), 1))
       expect_true(identical(as.numeric(res$res_status_after[[2]]), 1))
       expect_true(identical(as.numeric(res$is_long_term_mig[[2]]), 0))
       
       expect_true(as.numeric(as.Date(res$date_finalised_res_before[[2]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 370)
       expect_true(as.numeric(as.Date(res$date_finalised_res_after[[2]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 502)
       expect_true(as.numeric(as.Date(res$date_finalised_LTM[[2]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 502)
       
       ## check rcpp_resolve jouney 3
       ## The follow residentBefore has been updated by the initial resident
       ## status because the previous residentAfter finalised date is equal to
       ## the initial finalised date.
       expect_true(identical(as.numeric(res$res_status_before[[3]]),0))
       expect_true(identical(as.numeric(res$res_status_after[[3]]), 1))
       expect_true(identical(as.numeric(res$is_long_term_mig[[3]]), 1))
       
       expect_true(as.numeric(as.Date(res$date_finalised_res_before[[3]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 502)
       expect_true(as.numeric(as.Date(res$date_finalised_res_after[[3]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 755)
       expect_true(as.numeric(as.Date(res$date_finalised_LTM[[3]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 755)
       
       expect_true(identical(as.numeric(res$res_status_before[[4]]), 1))
       expect_true(identical(as.numeric(res$res_status_after[[4]]), 1))
       expect_true(identical(as.numeric(res$is_long_term_mig[[4]]), 0))
       
       expect_true(as.numeric(as.Date(res$date_finalised_res_before[[4]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 755)
       expect_true(as.numeric(as.Date(res$date_finalised_res_after[[4]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 802)
       expect_true(as.numeric(as.Date(res$date_finalised_LTM[[4]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 670)
       
       expect_true(identical(as.numeric(res$res_status_before[[5]]), 1))
       expect_true(identical(as.numeric(res$res_status_after[[5]]), 1))
       expect_true(identical(as.numeric(res$is_long_term_mig[[5]]), 0))
       
       expect_true(as.numeric(as.Date(res$date_finalised_res_before[[5]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 802)
       expect_true(as.numeric(as.Date(res$date_finalised_res_after[[5]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 802)
       expect_true(as.numeric(as.Date(res$date_finalised_LTM[[5]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 755)
  })
  
  it("Scenario 5: without IRS, the first direction is departure,
     the result should be same as scenario 1", {
    j1 <-       c(journeyId = 1,
                  personId = 1,
                  is_arrival = 0,
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
                  is_arrival = 0,
                  date_crossing = "2018-01-16",
                  journey_sequence = 3,
                  journeyId_prev = 2)
    
    j4 <-       c(journeyId = 4,
                  personId = 1,
                  is_arrival = 1,
                  date_crossing = "2018-11-02",
                  journey_sequence = 4,
                  journeyId_prev = 3)
    
    j5 <-       c(journeyId = 5,
                  personId = 1,
                  is_arrival = 0,
                  date_crossing = "2018-11-12",
                  journey_sequence = 5,
                  journeyId_prev = 4)
    
    person_data <- as.data.frame(rbind(j1, j2, j3, j4, j5),
                                 stringsAsFactors = FALSE)
    
    i1 <- c(personId = 1,
            res_status_initial = 1,
            date_finalised = "2017-01-01")
    
    ini_data <- as.data.frame(t(i1), stringsAsFactors = FALSE)
    ini_data <- ini_data[-1,]
    ## ensure no initial data
    expect_true(nrow(ini_data) == 0)
    crossings <- migrbc::pre_process(person_data,
                                     init_res_status_data = ini_data,
                                     n_groups = 1)
    res <- expect_error(migrbc::resolve_data(crossings,
                                             window_size = 487,
                                             threshold_year = 365), NA)
    res <- res$journeys
    
    ## check rcpp_resolve jouney 1
    expect_true(identical(as.numeric(res$res_status_before[[1]]), 1))
    expect_true(identical(as.numeric(res$res_status_after[[1]]), 0))
    expect_true(identical(as.numeric(res$is_long_term_mig[[1]]), 1))
    
    expect_true(as.numeric(as.Date(res$date_finalised_res_before[[1]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 0)
    expect_true(as.numeric(as.Date(res$date_finalised_res_after[[1]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 365)
    expect_true(as.numeric(as.Date(res$date_finalised_LTM[[1]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 365)
    
    expect_true(identical(as.numeric(res$res_status_before[[2]]), 0))
    expect_true(identical(as.numeric(res$res_status_after[[2]]), 0))
    expect_true(identical(as.numeric(res$is_long_term_mig[[2]]), 0))
    
    expect_true(as.numeric(as.Date(res$date_finalised_res_before[[2]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 370)
    expect_true(as.numeric(as.Date(res$date_finalised_res_after[[2]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 502)
    expect_true(as.numeric(as.Date(res$date_finalised_LTM[[2]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 502)
    
    expect_true(identical(as.numeric(res$res_status_before[[3]]), 0))
    expect_true(identical(as.numeric(res$res_status_after[[3]]), 0))
    expect_true(identical(as.numeric(res$is_long_term_mig[[3]]), 0))
    
    expect_true(as.numeric(as.Date(res$date_finalised_res_before[[3]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 502)
    expect_true(as.numeric(as.Date(res$date_finalised_res_after[[3]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 502)
    expect_true(as.numeric(as.Date(res$date_finalised_LTM[[3]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 380)
    
    expect_true(identical(as.numeric(res$res_status_before[[4]]), 0))
    expect_true(identical(as.numeric(res$res_status_after[[4]]), 0))
    expect_true(identical(as.numeric(res$is_long_term_mig[[4]]), 0))
    
    expect_true(as.numeric(as.Date(res$date_finalised_res_before[[4]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 670)
    expect_true(as.numeric(as.Date(res$date_finalised_res_after[[4]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 802)
    expect_true(as.numeric(as.Date(res$date_finalised_LTM[[4]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 802)
    
    expect_true(identical(as.numeric(res$res_status_before[[5]]), 0))
    expect_true(identical(as.numeric(res$res_status_after[[5]]), 0))
    expect_true(identical(as.numeric(res$is_long_term_mig[[5]]), 0))
    
    expect_true(as.numeric(as.Date(res$date_finalised_res_before[[5]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 802)
    expect_true(as.numeric(as.Date(res$date_finalised_res_after[[5]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 802)
    expect_true(as.numeric(as.Date(res$date_finalised_LTM[[5]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 680)
  })
  
  it("Scenario 6: without IRS, the first direction is arrival,
     the result should be same as scenario 3", {
    j1 <-       c(journeyId = 1,
                  personId = 1,
                  is_arrival = 1,
                  date_crossing = "2017-01-01",
                  journey_sequence = 1,
                  journeyId_prev = NA)
    
    j2 <-       c(journeyId = 2,
                  personId = 1,
                  is_arrival = 0,
                  date_crossing = "2018-01-06",
                  journey_sequence = 2,
                  journeyId_prev = 1)
    
    j3 <-       c(journeyId = 3,
                  personId = 1,
                  is_arrival = 1,
                  date_crossing = "2018-01-16",
                  journey_sequence = 3,
                  journeyId_prev = 2)
    
    j4 <-       c(journeyId = 4,
                  personId = 1,
                  is_arrival = 0,
                  date_crossing = "2018-11-02",
                  journey_sequence = 4,
                  journeyId_prev = 3)
    
    j5 <-       c(journeyId = 5,
                  personId = 1,
                  is_arrival = 1,
                  date_crossing = "2018-11-12",
                  journey_sequence = 5,
                  journeyId_prev = 4)
    
    person_data <- as.data.frame(rbind(j1, j2, j3, j4, j5),
                                 stringsAsFactors = FALSE)
    
    i1 <- c(personId = 1,
            res_status_initial = 0,
            date_finalised = "2017-01-01")
    
    ini_data <- as.data.frame(t(i1), stringsAsFactors = FALSE)
    ini_data <- ini_data[-1,]
    ## ensure no initial data
    expect_true(nrow(ini_data) == 0)
    crossings <- migrbc::pre_process(person_data,
                                     init_res_status_data = ini_data,
                                     n_groups = 1)
    res <- expect_error(migrbc::resolve_data(crossings,
                                             window_size = 487,
                                             threshold_year = 365), NA)
    res <- res$journeys
    
    ## check rcpp_resolve jouney 1
    expect_true(identical(as.numeric(res$res_status_before[[1]]), 0))
    expect_true(identical(as.numeric(res$res_status_after[[1]]), 1))
    expect_true(identical(as.numeric(res$is_long_term_mig[[1]]), 1))
    
    expect_true(as.numeric(as.Date(res$date_finalised_res_before[[1]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 0)
    expect_true(as.numeric(as.Date(res$date_finalised_res_after[[1]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 365)
    expect_true(as.numeric(as.Date(res$date_finalised_LTM[[1]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 365)
    
    expect_true(identical(as.numeric(res$res_status_before[[2]]), 1))
    expect_true(identical(as.numeric(res$res_status_after[[2]]), 1))
    expect_true(identical(as.numeric(res$is_long_term_mig[[2]]), 0))
    
    expect_true(as.numeric(as.Date(res$date_finalised_res_before[[2]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 370)
    expect_true(as.numeric(as.Date(res$date_finalised_res_after[[2]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 502)
    expect_true(as.numeric(as.Date(res$date_finalised_LTM[[2]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 502)
    
    expect_true(identical(as.numeric(res$res_status_before[[3]]), 1))
    expect_true(identical(as.numeric(res$res_status_after[[3]]), 1))
    expect_true(identical(as.numeric(res$is_long_term_mig[[3]]), 0))
    
    expect_true(as.numeric(as.Date(res$date_finalised_res_before[[3]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 502)
    expect_true(as.numeric(as.Date(res$date_finalised_res_after[[3]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 502)
    expect_true(as.numeric(as.Date(res$date_finalised_LTM[[3]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 380)
    
    expect_true(identical(as.numeric(res$res_status_before[[4]]), 1))
    expect_true(identical(as.numeric(res$res_status_after[[4]]), 1))
    expect_true(identical(as.numeric(res$is_long_term_mig[[4]]), 0))
    
    expect_true(as.numeric(as.Date(res$date_finalised_res_before[[4]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 670)
    expect_true(as.numeric(as.Date(res$date_finalised_res_after[[4]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 802)
    expect_true(as.numeric(as.Date(res$date_finalised_LTM[[4]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 802)
    
    expect_true(identical(as.numeric(res$res_status_before[[5]]), 1))
    expect_true(identical(as.numeric(res$res_status_after[[5]]), 1))
    expect_true(identical(as.numeric(res$is_long_term_mig[[5]]), 0))
    
    expect_true(as.numeric(as.Date(res$date_finalised_res_before[[5]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 802)
    expect_true(as.numeric(as.Date(res$date_finalised_res_after[[5]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 802)
    expect_true(as.numeric(as.Date(res$date_finalised_LTM[[5]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 680)
  })
  
  it("Scenario 7: one without IRS, one with IRS, 
     both direction are arrival", {
       j1 <-       c(journeyId = 1,
                     personId = 1,
                     is_arrival = 1,
                     date_crossing = "2017-01-01",
                     journey_sequence = 1,
                     journeyId_prev = NA)
       
       j2 <-       c(journeyId = 2,
                     personId = 1,
                     is_arrival = 0,
                     date_crossing = "2018-01-06",
                     journey_sequence = 2,
                     journeyId_prev = 1)
       
       j3 <-       c(journeyId = 3,
                     personId = 1,
                     is_arrival = 1,
                     date_crossing = "2018-01-16",
                     journey_sequence = 3,
                     journeyId_prev = 2)
       
       j4 <-       c(journeyId = 4,
                     personId = 1,
                     is_arrival = 0,
                     date_crossing = "2018-11-02",
                     journey_sequence = 4,
                     journeyId_prev = 3)
       
       j5 <-       c(journeyId = 5,
                     personId = 1,
                     is_arrival = 1,
                     date_crossing = "2018-11-12",
                     journey_sequence = 5,
                     journeyId_prev = 4)
       j6 <-       c(journeyId = 6,
                     personId = 2,
                     is_arrival = 1,
                     date_crossing = "2017-01-01",
                     journey_sequence = 5,
                     journeyId_prev = 4)
       j7 <-       c(journeyId = 7,
                     personId = 2,
                     is_arrival = 0,
                     date_crossing = "2018-01-16",
                     journey_sequence = 5,
                     journeyId_prev = 4)
       j8 <-       c(journeyId = 7,
                     personId = 2,
                     is_arrival = 1,
                     date_crossing = "2018-11-12",
                     journey_sequence = 5,
                     journeyId_prev = 4)
       
       person_data <- as.data.frame(rbind(j1, j2, j3, j4, j5, j6, j7, j8),
                                    stringsAsFactors = FALSE)
       
       i1 <- c(personId = 2,
               res_status_initial = 1,
               date_finalised = "2017-01-01")
       
       ini_data <- as.data.frame(t(i1), stringsAsFactors = FALSE)
       crossings <- migrbc::pre_process(person_data,
                                        init_res_status_data = ini_data,
                                        n_groups = 1)
       res <- expect_error(migrbc::resolve_data(crossings,
                                                window_size = 487,
                                                threshold_year = 365), NA)
       res <- res$journeys
       
       ## check rcpp_resolve jouney 1
       expect_true(identical(as.numeric(res$res_status_before[[1]]), 0))
       expect_true(identical(as.numeric(res$res_status_after[[1]]), 1))
       expect_true(identical(as.numeric(res$is_long_term_mig[[1]]), 1))
       
       expect_true(as.numeric(as.Date(res$date_finalised_res_before[[1]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 0)
       expect_true(as.numeric(as.Date(res$date_finalised_res_after[[1]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 365)
       expect_true(as.numeric(as.Date(res$date_finalised_LTM[[1]])
                          - as.Date(res$date_finalised_res_before[[1]])) == 365)
       
       expect_true(identical(as.numeric(res$res_status_before[[2]]), 1))
       expect_true(identical(as.numeric(res$res_status_after[[2]]), 1))
       expect_true(identical(as.numeric(res$is_long_term_mig[[2]]), 0))
       
       expect_true(as.numeric(as.Date(res$date_finalised_res_before[[2]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 370)
       expect_true(as.numeric(as.Date(res$date_finalised_res_after[[2]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 502)
       expect_true(as.numeric(as.Date(res$date_finalised_LTM[[2]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 502)
       
       expect_true(identical(as.numeric(res$res_status_before[[3]]), 1))
       expect_true(identical(as.numeric(res$res_status_after[[3]]), 1))
       expect_true(identical(as.numeric(res$is_long_term_mig[[3]]), 0))
       
       expect_true(as.numeric(as.Date(res$date_finalised_res_before[[3]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 502)
       expect_true(as.numeric(as.Date(res$date_finalised_res_after[[3]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 502)
       expect_true(as.numeric(as.Date(res$date_finalised_LTM[[3]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 380)
       
       expect_true(identical(as.numeric(res$res_status_before[[4]]), 1))
       expect_true(identical(as.numeric(res$res_status_after[[4]]), 1))
       expect_true(identical(as.numeric(res$is_long_term_mig[[4]]), 0))
       
       expect_true(as.numeric(as.Date(res$date_finalised_res_before[[4]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 670)
       expect_true(as.numeric(as.Date(res$date_finalised_res_after[[4]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 802)
       expect_true(as.numeric(as.Date(res$date_finalised_LTM[[4]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 802)
       
       expect_true(identical(as.numeric(res$res_status_before[[5]]), 1))
       expect_true(identical(as.numeric(res$res_status_after[[5]]), 1))
       expect_true(identical(as.numeric(res$is_long_term_mig[[5]]), 0))
       
       expect_true(as.numeric(as.Date(res$date_finalised_res_before[[5]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 802)
       expect_true(as.numeric(as.Date(res$date_finalised_res_after[[5]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 802)
       expect_true(as.numeric(as.Date(res$date_finalised_LTM[[5]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 680)
       
       ## person 2
       expect_true(identical(as.numeric(res$res_status_before[[6]]), 1))
       expect_true(identical(as.numeric(res$res_status_after[[6]]), 1))
       expect_true(identical(as.numeric(res$is_long_term_mig[[6]]), 0))
       
       expect_true(as.numeric(as.Date(res$date_finalised_res_before[[6]])
                        - as.Date(res$date_finalised_res_before[[6]])) == 0)
       expect_true(as.numeric(as.Date(res$date_finalised_res_after[[6]])
                        - as.Date(res$date_finalised_res_before[[6]])) == 0)
       expect_true(as.numeric(as.Date(res$date_finalised_LTM[[6]])
                        - as.Date(res$date_finalised_res_before[[6]])) == 0)
       
       expect_true(identical(as.numeric(res$res_status_before[[7]]), 1))
       expect_true(identical(as.numeric(res$res_status_after[[7]]), 1))
       expect_true(identical(as.numeric(res$is_long_term_mig[[7]]), 0))
       
       expect_true(as.numeric(as.Date(res$date_finalised_res_before[[7]])
                        - as.Date(res$date_finalised_res_before[[6]])) == 380)
       expect_true(as.numeric(as.Date(res$date_finalised_res_after[[7]])
                        - as.Date(res$date_finalised_res_before[[6]])) == 802)
       expect_true(as.numeric(as.Date(res$date_finalised_LTM[[7]])
                        - as.Date(res$date_finalised_res_before[[6]])) == 802)
       
       
       expect_true(identical(as.numeric(res$res_status_before[[8]]), 1))
       expect_true(identical(as.numeric(res$res_status_after[[8]]), 1))
       expect_true(identical(as.numeric(res$is_long_term_mig[[8]]), 0))
       
       expect_true(as.numeric(as.Date(res$date_finalised_res_before[[8]])
                        - as.Date(res$date_finalised_res_before[[6]])) == 802)
       expect_true(as.numeric(as.Date(res$date_finalised_res_after[[8]])
                        - as.Date(res$date_finalised_res_before[[6]])) == 802)
       expect_true(as.numeric(as.Date(res$date_finalised_LTM[[8]])
                        - as.Date(res$date_finalised_res_before[[6]])) == 802)
     })
})
