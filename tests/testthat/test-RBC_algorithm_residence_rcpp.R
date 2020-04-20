##############################################################################
context("RBC_algorithm_residence_rcpp")
##############################################################################

describe("Run RBC with a single resident person should succeed", {
  
  it("Scenario:  1. start of backseries - 
   a resident who is returning is nonmigrating", {
     j1 <-       c(journeyId = 1, 
                   personId = 1, 
                   is_arrival = 1, 
                   date_crossing = "2017-01-01", 
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
     res <- res$journeys
     ## check rcpp_resolve jouney 1
     expect_true(identical(as.numeric(res$res_status_before[[1]]), 1))
     expect_true(identical(as.numeric(res$res_status_after[[1]]), 1))
     expect_true(identical(as.numeric(res$is_long_term_mig[[1]]), 0))
     days_RB <- as.numeric(as.Date(res$date_finalised_res_before[[1]])
                           - as.Date(res$date_finalised_res_before[[1]]))
     days_RA <- as.numeric(as.Date(res$date_finalised_res_after[[1]])
                           - as.Date(res$date_finalised_res_before[[1]]))
     days_LTM <- as.numeric(
       as.Date(res$date_finalised_LTM[[1]]
       ) - as.Date(res$date_finalised_res_before[[1]]))
     
     expect_true(days_RB == 0)
     expect_true(days_RA == 0)
     expect_true(days_LTM == 0)
   })
  
  it("Scenario: 2. a resident who migrates", {
    j1 <-       c(journeyId = 1, 
                  personId = 1, 
                  is_arrival = 0, 
                  date_crossing = "2017-01-01", 
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
    
  })
  
  it("Scenario:  3. Holiday for a non-resident of less than 8 months", {
    
    j1 <-       c(journeyId = 1, 
                  personId = 1, 
                  is_arrival = 0, 
                  date_crossing = "2017-01-01", 
                  journey_sequence = 1,
                  journeyId_prev = NA)
    
    
    j2 <-       c(journeyId = 2, 
                  personId = 1, 
                  is_arrival = 1, 
                  date_crossing = "2017-01-11", 
                  journey_sequence = 2,
                  journeyId_prev = 1)
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
    res <- res$journeys
    
    
    ## check rcpp_resolve jouney 1
    expect_true(identical(as.numeric(res$res_status_before[[1]]), 1))
    expect_true(identical(as.numeric(res$res_status_after[[1]]), 1))
    expect_true(identical(as.numeric(res$is_long_term_mig[[1]]), 0))
    
    expect_true(as.numeric(as.Date(res$date_finalised_res_before[[1]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 0)
    expect_true(as.numeric(as.Date(res$date_finalised_res_after[[1]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 132)
    expect_true(as.numeric(as.Date(res$date_finalised_LTM[[1]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 132)
    
    expect_true(identical(as.numeric(res$res_status_before[[2]]), 1))
    expect_true(identical(as.numeric(res$res_status_after[[2]]), 1))
    expect_true(identical(as.numeric(res$is_long_term_mig[[2]]), 0))
    
    expect_true(as.numeric(as.Date(res$date_finalised_res_before[[2]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 132)
    expect_true(as.numeric(as.Date(res$date_finalised_res_after[[2]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 132)
    expect_true(as.numeric(as.Date(res$date_finalised_LTM[[2]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 10)
  })
  
  it("Scenario: 4. 2 Short return trips for a non-resident of
      less than 8 months", {
        
        j1 <-       c(journeyId = 1, 
                      personId = 1, 
                      is_arrival = 0, 
                      date_crossing = "2017-01-01", 
                      journey_sequence = 1,
                      journeyId_prev = NA)
        
        j2 <-       c(journeyId =2, 
                      personId = 1, 
                      is_arrival = 1, 
                      date_crossing = "2017-01-11", 
                      journey_sequence = 2,
                      journeyId_prev = 1)
        
        j3 <-       c(journeyId =3, 
                      personId = 1, 
                      is_arrival = 0, 
                      date_crossing = "2017-03-12", 
                      journey_sequence = 3,
                      journeyId_prev = 2)
        
        j4 <-       c(journeyId = 4, 
                      personId = 1, 
                      is_arrival = 1, 
                      date_crossing = "2017-04-01", 
                      journey_sequence = 4,
                      journeyId_prev = 3)
        
        person_data <- as.data.frame(rbind(j1, j2, j3, j4),
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
        expect_true(identical(as.numeric(res$res_status_after[[1]]), 1))
        expect_true(identical(as.numeric(res$is_long_term_mig[[1]]), 0))
        
        expect_true(as.numeric(as.Date(res$date_finalised_res_before[[1]])
                            - as.Date(res$date_finalised_res_before[[1]])) == 0)
        expect_true(as.numeric(as.Date(res$date_finalised_res_after[[1]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 152)
        expect_true(as.numeric(as.Date(res$date_finalised_LTM[[1]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 152)
        
        expect_true(identical(as.numeric(res$res_status_before[[2]]), 1))
        expect_true(identical(as.numeric(res$res_status_after[[2]]), 1))
        expect_true(identical(as.numeric(res$is_long_term_mig[[2]]), 0))
        
        expect_true(as.numeric(as.Date(res$date_finalised_res_before[[2]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 152)
        expect_true(as.numeric(as.Date(res$date_finalised_res_after[[2]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 152)
        expect_true(as.numeric(as.Date(res$date_finalised_LTM[[2]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 10)
        
        expect_true(identical(as.numeric(res$res_status_before[[3]]), 1))
        expect_true(identical(as.numeric(res$res_status_after[[3]]), 1))
        expect_true(identical(as.numeric(res$is_long_term_mig[[3]]), 0))
        
        expect_true(as.numeric(as.Date(res$date_finalised_res_before[[3]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 152)
        expect_true(as.numeric(as.Date(res$date_finalised_res_after[[3]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 212)
        expect_true(as.numeric(as.Date(res$date_finalised_LTM[[3]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 212)
        
        expect_true(identical(as.numeric(res$res_status_before[[4]]), 1))
        expect_true(identical(as.numeric(res$res_status_after[[4]]), 1))
        expect_true(identical(as.numeric(res$is_long_term_mig[[4]]), 0))
        
        expect_true(as.numeric(as.Date(res$date_finalised_res_before[[4]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 212)
        expect_true(as.numeric(as.Date(res$date_finalised_res_after[[4]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 212)
        expect_true(as.numeric(as.Date(res$date_finalised_LTM[[4]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 90)
      })
  
  it("Scenario: 5. Holiday for a non-resident of more than 8 months
     and less than 12 months", {
       j1 <-       c(journeyId = 1, 
                     personId = 1, 
                     is_arrival = 0, 
                     date_crossing = "2017-01-01", 
                     journey_sequence = 1,
                     journeyId_prev = NA)
       
       
       j2 <-       c(journeyId = 2, 
                     personId = 1, 
                     is_arrival = 1, 
                     date_crossing = "2017-09-08", 
                     journey_sequence = 2,
                     journeyId_prev = 1)
       person_data <- as.data.frame(rbind(j1, j2 ),
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
       expect_true(identical(as.numeric(res$res_status_after[[1]]), 1))
       expect_true(identical(as.numeric(res$is_long_term_mig[[1]]), 0))
       
       expect_true(as.numeric(as.Date(res$date_finalised_res_before[[1]])
                      - as.Date(res$date_finalised_res_before[[1]])) == 0)
       expect_true(as.numeric(as.Date(res$date_finalised_res_after[[1]])
                      - as.Date(res$date_finalised_res_before[[1]])) == 372)
       expect_true(as.numeric(as.Date(res$date_finalised_LTM[[1]])
                      - as.Date(res$date_finalised_res_before[[1]])) == 372)
       
       expect_true(identical(as.numeric(res$res_status_before[[2]]), 1))
       expect_true(identical(as.numeric(res$res_status_after[[2]]), 1))
       expect_true(identical(as.numeric(res$is_long_term_mig[[2]]), 0))
       
       expect_true(as.numeric(as.Date(res$date_finalised_res_before[[2]])
                      - as.Date(res$date_finalised_res_before[[1]])) == 372)
       expect_true(as.numeric(as.Date(res$date_finalised_res_after[[2]])
                      - as.Date(res$date_finalised_res_before[[1]])) == 372)
       expect_true(as.numeric(as.Date(res$date_finalised_LTM[[2]])
                      - as.Date(res$date_finalised_res_before[[1]])) == 372)
     })
  
  
  it("Scenario: 6. non-resident goes there and back on same day", {
    j1 <-       c(journeyId = 1, 
                  personId = 1, 
                  is_arrival = 0, 
                  date_crossing = "2017-01-01", 
                  journey_sequence = 1,
                  journeyId_prev = NA)
    
    
    j2 <-       c(journeyId = 2, 
                  personId = 1, 
                  is_arrival = 1, 
                  date_crossing = "2017-01-01", 
                  journey_sequence = 2,
                  journeyId_prev = 1)
    person_data <- as.data.frame(rbind(j1, j2 ),
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
    expect_true(identical(as.numeric(res$res_status_after[[1]]), 1))
    expect_true(identical(as.numeric(res$is_long_term_mig[[1]]), 0))
    
    expect_true(as.numeric(as.Date(res$date_finalised_res_before[[1]])
                    - as.Date(res$date_finalised_res_before[[1]])) == 0)
    expect_true(as.numeric(as.Date(res$date_finalised_res_after[[1]])
                    - as.Date(res$date_finalised_res_before[[1]])) == 122)
    expect_true(as.numeric(as.Date(res$date_finalised_LTM[[1]])
                    - as.Date(res$date_finalised_res_before[[1]])) == 122)
    
    expect_true(identical(as.numeric(res$res_status_before[[2]]), 1))
    expect_true(identical(as.numeric(res$res_status_after[[2]]), 1))
    expect_true(identical(as.numeric(res$is_long_term_mig[[2]]), 0))
    
    expect_true(as.numeric(as.Date(res$date_finalised_res_before[[2]])
                    - as.Date(res$date_finalised_res_before[[1]])) == 122)
    expect_true(as.numeric(as.Date(res$date_finalised_res_after[[2]])
                    - as.Date(res$date_finalised_res_before[[1]])) == 122)
    expect_true(as.numeric(as.Date(res$date_finalised_LTM[[2]])
                    - as.Date(res$date_finalised_res_before[[1]])) == 0)
  })
  
  
  
  it("Scenario: 7. a non-resident who migrates after short trip", {
    j1 <-       c(journeyId = 1, 
                  personId = 1, 
                  is_arrival = 0, 
                  date_crossing = "2017-01-01", 
                  journey_sequence = 1,
                  journeyId_prev = NA)
    j2 <-       c(journeyId = 2, 
                  personId = 1, 
                  is_arrival = 1, 
                  date_crossing = "2017-01-11", 
                  journey_sequence = 2,
                  journeyId_prev = 1)
    j3 <-       c(journeyId = 3, 
                  personId = 1, 
                  is_arrival = 0, 
                  date_crossing = "2017-03-12", 
                  journey_sequence = 3,
                  journeyId_prev = 2)
    person_data <- as.data.frame(rbind(j1, j2, j3),
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
                    - as.Date(res$date_finalised_res_before[[1]])) == 425)
    expect_true(as.numeric(as.Date(res$date_finalised_LTM[[1]])
                    - as.Date(res$date_finalised_res_before[[1]])) == 425)
    
    expect_true(identical(as.numeric(res$res_status_before[[2]]), 0))
    expect_true(identical(as.numeric(res$res_status_after[[2]]), 0))
    expect_true(identical(as.numeric(res$is_long_term_mig[[2]]), 0))
    
    expect_true(as.numeric(as.Date(res$date_finalised_res_before[[2]])
                    - as.Date(res$date_finalised_res_before[[1]])) == 425)
    expect_true(as.numeric(as.Date(res$date_finalised_res_after[[2]])
                    - as.Date(res$date_finalised_res_before[[1]])) == 425)
    expect_true(as.numeric(as.Date(res$date_finalised_LTM[[2]])
                    - as.Date(res$date_finalised_res_before[[1]])) == 10)
    
    expect_true(identical(as.numeric(res$res_status_before[[3]]), 0))
    expect_true(identical(as.numeric(res$res_status_after[[3]]), 0))
    expect_true(identical(as.numeric(res$is_long_term_mig[[3]]), 0))
    
    expect_true(as.numeric(as.Date(res$date_finalised_res_before[[3]])
                    - as.Date(res$date_finalised_res_before[[1]])) == 425)
    expect_true(as.numeric(as.Date(res$date_finalised_res_after[[3]])
                    - as.Date(res$date_finalised_res_before[[1]])) == 425)
    expect_true(as.numeric(as.Date(res$date_finalised_LTM[[3]])
                    - as.Date(res$date_finalised_res_before[[1]])) == 425)
    
  })
  
  it("Scenario: 8. a non-resident who migrates after long trip
      and then returns and migrates", {
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
        person_data <- as.data.frame(rbind(j1, j2 ),
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
        expect_true(identical(as.numeric(res$res_status_after[[2]]), 1))
        expect_true(identical(as.numeric(res$is_long_term_mig[[2]]), 1))
        
        expect_true(as.numeric(as.Date(res$date_finalised_res_before[[2]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 370)
        expect_true(as.numeric(as.Date(res$date_finalised_res_after[[2]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 735)
        expect_true(as.numeric(as.Date(res$date_finalised_LTM[[2]])
                        - as.Date(res$date_finalised_res_before[[1]])) == 735)
      })
  
  
  it("Scenario: 9. a non-resident who migrates during succession of trips", {
    j1 <-       c(journeyId = 1,
                  personId = 1,
                  is_arrival = 0,
                  date_crossing = "2017-01-01",
                  journey_sequence = 1,
                  journeyId_prev = NA)
    
    j2 <-       c(journeyId = 2,
                  personId = 1,
                  is_arrival = 1,
                  date_crossing = "2017-12-17",
                  journey_sequence = 2,
                  journeyId_prev = 1)
    
    j3 <-       c(journeyId = 3,
                  personId = 1,
                  is_arrival = 0,
                  date_crossing = "2018-01-06",
                  journey_sequence = 3,
                  journeyId_prev = 2)
    
    j4 <-       c(journeyId = 4,
                  personId = 1,
                  is_arrival = 1,
                  date_crossing = "2018-01-26",
                  journey_sequence = 4,
                  journeyId_prev = 3)
    
    j5 <-       c(journeyId = 5,
                  personId = 1,
                  is_arrival = 0,
                  date_crossing = "2018-01-27",
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
                    - as.Date(res$date_finalised_res_before[[1]])) == 385)
    expect_true(as.numeric(as.Date(res$date_finalised_LTM[[1]])
                    - as.Date(res$date_finalised_res_before[[1]])) == 385)
    
    expect_true(identical(as.numeric(res$res_status_before[[2]]), 0))
    expect_true(identical(as.numeric(res$res_status_after[[2]]), 0))
    expect_true(identical(as.numeric(res$is_long_term_mig[[2]]), 0))
    
    expect_true(as.numeric(as.Date(res$date_finalised_res_before[[2]])
                    - as.Date(res$date_finalised_res_before[[1]])) == 385)
    expect_true(as.numeric(as.Date(res$date_finalised_res_after[[2]])
                    - as.Date(res$date_finalised_res_before[[1]])) == 493)
    expect_true(as.numeric(as.Date(res$date_finalised_LTM[[2]])
                    - as.Date(res$date_finalised_res_before[[1]])) == 493)
    
    expect_true(identical(as.numeric(res$res_status_before[[3]]), 0))
    expect_true(identical(as.numeric(res$res_status_after[[3]]), 0))
    expect_true(identical(as.numeric(res$is_long_term_mig[[3]]), 0))
    
    expect_true(as.numeric(as.Date(res$date_finalised_res_before[[3]])
                    - as.Date(res$date_finalised_res_before[[1]])) == 493)
    expect_true(as.numeric(as.Date(res$date_finalised_res_after[[3]])
                    - as.Date(res$date_finalised_res_before[[1]])) == 493)
    expect_true(as.numeric(as.Date(res$date_finalised_LTM[[3]])
                    - as.Date(res$date_finalised_res_before[[1]])) == 385)
    
    expect_true(identical(as.numeric(res$res_status_before[[4]]), 0))
    expect_true(identical(as.numeric(res$res_status_after[[4]]), 0))
    expect_true(identical(as.numeric(res$is_long_term_mig[[4]]), 0))
    
    expect_true(as.numeric(as.Date(res$date_finalised_res_before[[4]])
                    - as.Date(res$date_finalised_res_before[[1]])) == 493)
    expect_true(as.numeric(as.Date(res$date_finalised_res_after[[4]])
                    - as.Date(res$date_finalised_res_before[[1]])) == 513)
    expect_true(as.numeric(as.Date(res$date_finalised_LTM[[4]])
                    - as.Date(res$date_finalised_res_before[[1]])) == 513)
    
    expect_true(identical(as.numeric(res$res_status_before[[5]]), 0))
    expect_true(identical(as.numeric(res$res_status_after[[5]]), 0))
    expect_true(identical(as.numeric(res$is_long_term_mig[[5]]), 0))
    
    expect_true(as.numeric(as.Date(res$date_finalised_res_before[[5]])
                    - as.Date(res$date_finalised_res_before[[1]])) == 513)
    expect_true(as.numeric(as.Date(res$date_finalised_res_after[[5]])
                    - as.Date(res$date_finalised_res_before[[1]])) == 513)
    expect_true(as.numeric(as.Date(res$date_finalised_LTM[[5]])
                    - as.Date(res$date_finalised_res_before[[1]])) == 391)
    
  })
  
  
  
  it("Scenario: 10. a non-resident who has two trips with long delay between", {
    
    j1 <-       c(journeyId = 1, 
                  personId = 1, 
                  is_arrival = 0, 
                  date_crossing = "2017-01-01", 
                  journey_sequence = 1,
                  journeyId_prev = NA)
    
    j2 <-       c(journeyId = 2, 
                  personId = 1, 
                  is_arrival = 1, 
                  date_crossing = "2017-01-11", 
                  journey_sequence = 2,
                  journeyId_prev = 1)
    
    j3 <-       c(journeyId = 3, 
                  personId = 1, 
                  is_arrival = 0, 
                  date_crossing = "2018-01-06", 
                  journey_sequence = 3,
                  journeyId_prev = 2)
    
    j4 <-       c(journeyId = 4, 
                  personId = 1, 
                  is_arrival = 1, 
                  date_crossing = "2018-01-16", 
                  journey_sequence = 4,
                  journeyId_prev = 3)
    
    person_data <- as.data.frame(rbind(j1, j2, j3, j4),
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
    expect_true(identical(as.numeric(res$res_status_after[[1]]), 1))
    expect_true(identical(as.numeric(res$is_long_term_mig[[1]]), 0))
    
    expect_true(as.numeric(as.Date(res$date_finalised_res_before[[1]])
                    - as.Date(res$date_finalised_res_before[[1]])) == 0)
    expect_true(as.numeric(as.Date(res$date_finalised_res_after[[1]])
                    - as.Date(res$date_finalised_res_before[[1]])) == 132)
    expect_true(as.numeric(as.Date(res$date_finalised_LTM[[1]])
                    - as.Date(res$date_finalised_res_before[[1]])) == 132)
    
    expect_true(identical(as.numeric(res$res_status_before[[2]]), 1))
    expect_true(identical(as.numeric(res$res_status_after[[2]]), 1))
    expect_true(identical(as.numeric(res$is_long_term_mig[[2]]), 0))
    
    expect_true(as.numeric(as.Date(res$date_finalised_res_before[[2]])
                    - as.Date(res$date_finalised_res_before[[1]])) == 132)
    expect_true(as.numeric(as.Date(res$date_finalised_res_after[[2]])
                    - as.Date(res$date_finalised_res_before[[1]])) == 132)
    expect_true(as.numeric(as.Date(res$date_finalised_LTM[[2]])
                    - as.Date(res$date_finalised_res_before[[1]])) == 10)
    
    expect_true(identical(as.numeric(res$res_status_before[[3]]), 1))
    expect_true(identical(as.numeric(res$res_status_after[[3]]), 1))
    expect_true(identical(as.numeric(res$is_long_term_mig[[3]]), 0))
    
    expect_true(as.numeric(as.Date(res$date_finalised_res_before[[3]])
                    - as.Date(res$date_finalised_res_before[[1]])) == 370)
    expect_true(as.numeric(as.Date(res$date_finalised_res_after[[3]])
                    - as.Date(res$date_finalised_res_before[[1]])) == 502)
    expect_true(as.numeric(as.Date(res$date_finalised_LTM[[3]])
                    - as.Date(res$date_finalised_res_before[[1]])) == 502)
    
    expect_true(identical(as.numeric(res$res_status_before[[4]]), 1))
    expect_true(identical(as.numeric(res$res_status_after[[4]]), 1))
    expect_true(identical(as.numeric(res$is_long_term_mig[[4]]), 0))
    
    expect_true(as.numeric(as.Date(res$date_finalised_res_before[[4]])
                    - as.Date(res$date_finalised_res_before[[1]])) == 502)
    expect_true(as.numeric(as.Date(res$date_finalised_res_after[[4]])
                    - as.Date(res$date_finalised_res_before[[1]])) == 502)
    expect_true(as.numeric(as.Date(res$date_finalised_LTM[[4]])
                    - as.Date(res$date_finalised_res_before[[1]])) == 380)
  })
  
  it("Scenario: 11. a non-resident who migrates after long trip
     and then returns on short trips a long time apart", {
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
       ## check days_to_next_crossing
       expect_true((as.Date(res[2, "date_crossing"]) -
                      as.Date(res[1, "date_crossing"])) ==
                     res[1, "days_to_next_crossing"])
     })
  
})
