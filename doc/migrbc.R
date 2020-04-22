## ----setup, include = FALSE-------------------------------------------------------------------------------------------------------------------------
options(tinytex.verbose = TRUE)
knitr::opts_chunk$set(
  cache = FALSE,
  message = FALSE,
  warning = FALSE,
  collapse = TRUE,
  comment = "#>"
)
options(width = 150)

## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------
#  ---
#  Terms:
#    Rb:  resident status before - Resident or NonResident
#    Ra:  resident status after - Resident or NonResident
#    LTM: Long term migration - true or false
#    Journey: Travel movement of a person involving a border crossing:
#             Depart or Arrive
#  
#  Define Parameters:
#    Tw: Window size - the maximum number of days within which the resident status of
#        the [persons journey] is resolved (specified as 487 days).
#    Tm: Threshold for Accumulated days [outside the country as departed]. It is
#        required for Migration (specified as 365 days).
#  
#  Derived Parameters:
#    Tn: Threshold for Accumulated days required for Non-Migration (Tw-Tm). Journey
#        stays within resident country (derived as 122 days).
#    Ts: Threshold for instant assignment of non-migration for successive return
#        journeys (Tw-Tm)x2 (derived as 244 days).
#  
#  Define the Input Set:
#    Attributes of the person are
#      - residence status before
#      - date resolved residence status before -> date Rb
#      - list of journeys arranged in time ascending order
#  
#    Attributes of each journey are
#      - journey identifier (JID)
#      - date of crossing
#      - direction
#  
#    Working Values for each journey
#      - duration
#  
#    Final Results
#      - journey identifier (JID)
#      - prev journey identifier (prev JID)
#      - residence status before (Rb)
#      - residence status after (Ra)
#      - long term migrant -> LTM  ## or Rb or  Ra
#      - date resolved residence status before -> date Rb
#      - date resolved residence status after -> date Ra
#      - date resolved long term migrant -> date LTM
#  
#  
#  Algorithm:
#    ## We calculate the outcomes for a single person
#    ## pre-check and assignment
#    ## check the provided journey list is a valid sequence and assign duration for
#    ## each journey.
#    For each journey in list of journeys
#        ## arrive / arrive OR depart / depart
#        if the journey direction has not changed from prior journey
#            then raise error
#        ## 0 if same day
#        journey.duration is days elapsed until next journey
#        ## artificial to make later calculations easier
#        if this is the last journey then duration is set to Tw
#  
#       journey.prev journey id is the prior journey.journey id
#  
#  
#    ## Run Main Calculations
#    For each journeyUnderReview in list of journeys
#        ## the resident status before of this journey is the resident status
#        ## after of the prior journey or person if first journey
#        if first journey
#           journeyUnderReview.[resident status before] = person.
#           journeyUnderReview.[date Rb] = person.[date Rb]
#        else
#           journeyUnderReview.[resident status before] = previousJourney.
#           journeyUnderReview.[date Rb] = previousJourney.[date Ra]
#  
#        ## Step 1 of 3 - find our migration direction status
#  
#        if (journeyUnderReview.[direction] is Depart AND
#           journeyUnderReview.[resident status before] is Resident)
#           OR (journeyUnderReview.[direction] is Arrive
#           AND journeyUnderReview.[resident status before] is NonResident)
#  
#           JourneyMigrationDirection = Migration
#        else
#           JourneyMigrationDirection = Non-Migration
#  
#        ## Step 2 of 3 - look ahead to set *instant* LTM. These set LTM = false for
#        ## any qualifying journeys basically any journey which is a return (Arrive
#        ## for resident OR depart for NonResident) within Ts days of date of crossing
#        ## of JourneyUnderReview do a look ahead to see if any Ts threshold for
#        ## instant non-migrations can be set.
#  
#        if JourneyMigrationDirection = Migration
#           for each journey in list of journeys starting from the journeyUnderReview
#               if journey.[date crossing] > journeyUnderReview.[date crossing] + Ts
#                  Exit for loop
#  
#               ## The journeyUnderReview is in migration direction and journey is a
#               ## return.
#               if journey.[sequence] is Even
#                   journey.[long term migration] = false
#                   journey.[date LTM] = maximum ( journey.[date of crossing] )
#                                        and ( journeyUnderReview.[date Rb] )
#  
#  
#         ## Step 3.1 of 3 - resolve the JourneyUnderReview – Non-Migrating direction
#         ## Non Migrating direction calculation is simple – the journey is not
#         ## a migration.
#         if JourneyMigrationDirection = Non-Migration
#            AssignNoMigration ( journeyUnderReview, 0 )
#  
#         ## Step 3.2 of 3 - resolve the JourneyUnderReview – Migrating direction
#         ## For the journey under review (assume = #1), odd successive journeys
#         ## contribute to days migrating and even journeys contribute to days
#         ## not migrating. Now we loop through successive journeys to this one and
#         ## accumulate days migrating and days non migrating.
#  
#         if JourneyMigrationDirection = Migration
#            for each journey in list of journeys starting from the journeyUnderReview
#                AccumulatedDaysMigration = cumulative sum (odd journey duration)
#                AccumulatedDaysNoMigration = cumulative sum (even journey duration)
#  
#                ## eventually one of the conditions will be met
#                if AccumulatedDaysMigration >= Tm
#                   AssignMigration(journeyUnderReview, AccumulatedDaysNoMigration)
#                   and exit for loop
#                if AccumulatedDaysNoMigration >= Tn
#                   AssignNoMigration(journeyUnderReview, AccumulatedDaysMigration)
#                   and exit for loop
#  
#         ## journeyUnderReview has been resolved
#         add journeyUnderReview to results
#  
#         ## setup for next loop
#         previousJourney = journeyUnderReview
#  
#  
#  Sub Routines for Clarity
#  
#    AssignMigration ( journeyUnderReview, AccumulatedDaysNoMigration )
#        ## migration has happened so LTM is true and residence status is flipped
#        journeyUnderReview.[long term migration] = true
#        journeyUnderReview.[date LTM] = crossing date + Tm +
#          AccumulatedDaysNoMigration
#        journeyUnderReview.[resident status after] = opposite of journeyUnderReview.
#        journeyUnderReview.[date Ra] = journeyUnderReview.[date LTM]
#  
#    AssignNoMigration ( journeyUnderReview, AccumulatedDaysMigration )
#        ## No migration has happened so LTM is false and residence status stays same
#        journeyUnderReview.[resident status after]= journeyUnderReview.
#        ## date Ra cannot be earlier than date Rb
#        journeyUnderReview.[date Ra] = maximum of (crossing date + Tn +
#                         AccumulatedDaysMigration) and (journeyUnderReview.[date Rb])
#  
#        ## we only need to set LTM if it has not already been instant assigned
#        if journeyUnderReview.[long term migration] is not known
#           journeyUnderReview.[long term migration] = false
#           journeyUnderReview.[date LTM] = maximum of (crossing date + Tn +
#                 AccumulatedDaysMigration) and  (journeyUnderReview.[date Rb])
#  ---

## ---- message = FALSE-------------------------------------------------------------------------------------------------------------------------------
library(migrbc)
library(knitr)
# If using the parallel processing option (only available on UNIX like systems)
library(parallel)

## ---------------------------------------------------------------------------------------------------------------------------------------------------
log_level <- 1
log_path <- NULL
migrbc::initialize_logger(log_level = log_level,
                          log_path = log_path)

## ---------------------------------------------------------------------------------------------------------------------------------------------------
number_of_people <- 10
person_data <- migrbc::setup_random_test_data(number_of_people, 
                                              initial_date = "2001-01-01", 
                                              numJourneys = 10,
                                              min = 0, 
                                              max = 100)


## ---------------------------------------------------------------------------------------------------------------------------------------------------
## 12/16
## threshold value
dur_threshold_year <- 365
## window size
dur_ws <- 487

## ---------------------------------------------------------------------------------------------------------------------------------------------------
## genereate test data
person_temp <- migrbc::setup_random_test_data(number_of_people, 
                                              initial_date = "2001-01-01", 
                                              numJourneys = 10,
                                              min = 0, 
                                              max = 100)
## write to a temp file
temp_path <- tempdir()
utils::write.csv(person_temp, 
                 file = file.path(temp_path, "data1.csv"), 
                 row.names = FALSE)
## read in files using the fread function from the data.table package
person_data <- utils::read.csv(file.path(temp_path, "data1.csv"),
                               stringsAsFactors = F)
kable(head(person_data[, c(1:5)]))

## ---------------------------------------------------------------------------------------------------------------------------------------------------
n_groups <- 10
pre_processed_data <- migrbc::pre_process(person_data, 
                                         n_groups = n_groups)

## ---------------------------------------------------------------------------------------------------------------------------------------------------
## run in non-parallel
res <- migrbc::run_rbc(pre_processed_data, 
                       window_size = dur_ws, 
                       threshold_year = dur_threshold_year, 
                       parallel=FALSE)

## run in parallel
res <- migrbc::run_rbc(pre_processed_data, 
                       window_size = dur_ws, 
                       threshold_year = dur_threshold_year, 
                       parallel=TRUE,
                       n_core = 2)

## ---------------------------------------------------------------------------------------------------------------------------------------------------
## run in non-parallel with the raw data i.e., the person_data directly
res <- migrbc::run_rbc(person_data, 
                       window_size = dur_ws, 
                       threshold_year = dur_threshold_year, 
                       parallel=FALSE)

## run in parallel with the raw data i.e., the person_data directly
res <- migrbc::run_rbc(person_data, 
                       window_size = dur_ws, 
                       threshold_year = dur_threshold_year, 
                       parallel=TRUE,
                       n_core = 2)

## ---------------------------------------------------------------------------------------------------------------------------------------------------
## good result
kable(head(res$journeys[, c(1, 3, 8, 9, 10)]))


## ---------------------------------------------------------------------------------------------------------------------------------------------------
## set up a test data with two person
## a person with bad journeys
j1 <-       c(journeyId = 1, 
              personId = 1, 
              is_arrival = 1, 
              date_crossing = "2014-02-13", 
              journey_sequence = 1,
              journeyId_prev = NA)
j2 <-       c(journeyId = 2, 
              personId = 1, 
              is_arrival = 1, 
              date_crossing = "2014-02-26", 
              journey_sequence = 2,
              journeyId_prev = 1)
j3 <-       c(journeyId = 3, 
              personId = 2, 
              is_arrival = 1, 
              date_crossing = "2014-02-13", 
              journey_sequence = 1,
              journeyId_prev = NA)
j4 <-       c(journeyId = 4, 
              personId = 2, 
              is_arrival = 0, 
              date_crossing = "2014-02-26", 
              journey_sequence = 2,
              journeyId_prev = 1)

## initial residence status data
i1 <- c(personId = 1, 
        res_status_initial = 0, 
        date_finalised = "2017-01-01")
i2 <- c(personId = 2, 
        res_status_initial = 0, 
        date_finalised = "2017-01-01")

person_data <- as.data.frame(rbind(j1, j2, j3, j4 ),
                             stringsAsFactors = FALSE)
ini_data <- as.data.frame(rbind(i1, i2), stringsAsFactors = FALSE)  
post_data <- migrbc::run_rbc(person_data,
                             init_res_status_data = ini_data,
                             window_size = 487, 
                             threshold_year = 365,
                             include_error_columns = TRUE)

## good result
kable(head(post_data$journeys[, c(1, 3, 8, 9, 10)]))

## bad result
kable(head(post_data$error_data[, c(1, 3, 14, 15)]))
    

## ---- fig.width = 7.1, fig.height = 8---------------------------------------------------------------------------------------------------------------
plot_test <- function(mig_hist) {
  plot_mig_hist(date_crossing = as.character(mig_hist$date_crossing), 
                is_arrival = mig_hist$is_arrival,
                days_to_next_crossing = mig_hist$days_to_next_crossing,
                show_date = FALSE, 
                cex = 0.8)
}
number_of_people <- 1
person_data <- migrbc::setup_random_test_data(number_of_people, 
                                              initial_date = "2001-01-01", 
                                              numJourneys = 3,
                                              min = 0, 
                                              max = 100)
## run in non-parallel
post_data <- migrbc::run_rbc(person_data, 
                       window_size = 487, 
                       threshold_year = 365, 
                       parallel=FALSE)
## plot good result
old_par <- par(mfrow = c(1, 1))
plot_test(post_data$journeys)
par(old_par)

