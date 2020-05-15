#include <Rcpp.h>
#include <ctime>
#include <string>
#include <sstream>
#include "rcpp_exception.h"
#include "rcpp_utility.h"
using namespace Rcpp;


//' Processing RBC for a person.
//' 
//' This function is used to resolve one person's journeys, i.e., classifying 
//' a person and marking it whether or not to be a long term migrant based on 
//' the person's journeys. This function is used internally 
//' inside the package and shouldn't be exposed to the outside caller.
//' 
//' @param person_data A list object of a person's journeys.
//' @param int_res_status The initial residence status of the target person
//' @param initial_date_finalised The final resolved date of the initial residence
//'  status.
//' @param tw Windows Size, by default, it is 487 days.
//' @param tm Threshold of Year, by default, it is 365 days.
//' 
//' @return A list of classified / labelled journeys.
//' 
// [[Rcpp::export]]
Rcpp::List rcpp_resolve(Rcpp::List &person_data, 
                        int int_res_status,
                        std::string initial_date_finalised,
                        int &tw,
                          int &tm) {
    
    //// an object is a person
    std::string first_date_crossing = "";
    int tn =  tw - tm;
    int ts = (tw - tm) * 2;
    //// get data in columns
    IntegerVector journeyId = person_data["journeyId"];
    IntegerVector personId = person_data["personId"];
    StringVector date_crossing = person_data["date_crossing"];
    IntegerVector journey_sequence = person_data["journey_sequence"];
    IntegerVector is_arrival = person_data["is_arrival"];
    IntegerVector journeyId_prev = person_data["journeyId_prev"];
    int len_journeys = journeyId.length();
    if(len_journeys == 0) throw JourneyException();
    //// initialize extra variables
    IntegerVector residentBefore(len_journeys);
    IntegerVector residentAfter(len_journeys);
    IntegerVector is_long_term_mig(len_journeys);
    StringVector dateElapsedToFinalizedRB(len_journeys);
    StringVector dateElapsedToFinalizedLTM(len_journeys);
    StringVector dateElapsedToFinalizedRA(len_journeys);
    IntegerVector days_to_next_crossing(len_journeys);
    first_date_crossing = date_crossing[0];
    if(!is_date(first_date_crossing)) throw InvalidCrossDateException();
    Date  date_ini = str_to_date(first_date_crossing);
    int direction_ini = is_arrival[0];
    if(direction_ini != 0 and direction_ini !=1) throw InvalidDirectionException();
    int irs = int_res_status;
    Date irs_date;
    irs_date = str_to_date(initial_date_finalised);
    int irs_finalised = irs;
    //// the initial status is set at the midle somewhere
    if(date_ini < irs_date) {
       if(direction_ini == 1) 
         irs = 0;
       else 
         irs = 1;
    }
    residentBefore[0] = irs;
    dateElapsedToFinalizedRB[0] = date_crossing[0];
    Date date_RB_ini = str_to_date(as<std::string>(dateElapsedToFinalizedRB[0]));  
    Date date_current = get_current_date();
    //// preprocess i.e., pre-check an assignment
    //// check the provided journey list is a valid sequence and assign 
    //// duration for each journey
    for(int i = 0; i < len_journeys; i++) {
        int direction_i = is_arrival[i];
        if(direction_i != 0 and direction_i != 1) throw InvalidDirectionException();
        std::string date_crossing_c_i = "";
        date_crossing_c_i = date_crossing[i];
        if(!is_date(date_crossing_c_i)) throw InvalidCrossDateException();
        Date date_i = str_to_date(date_crossing_c_i);
        days_to_next_crossing[i]= tw;
        if(i > 0) {
            Date date_pre = str_to_date(as<std::string>(date_crossing[i- 1]));
            int direction_i_pre = is_arrival[i - 1];
            long id_i_pre = journeyId[i-1];
            //// if the previous jouney is a temporary fix, then relink
            //// the previous id to the one prior to the insert journey
            if(id_i_pre == -1) id_i_pre = journeyId[i - 2];
            journeyId_prev[i] = id_i_pre;
            if(date_pre > date_i) throw SequenceException();
            if(direction_i == direction_i_pre) throw IdenticalDirectionException();
            days_to_next_crossing[i-1] = date_i - date_pre;
        }
    }
    
    //// run main calculations
    if(len_journeys > 1) {
        for(int i=0; i < len_journeys; i++) {
            std::string date_crossing_c_i = "";
            date_crossing_c_i = date_crossing[i];
            Date date_i = str_to_date(date_crossing_c_i);
            int direction_i = is_arrival[i];
            Date date_obs_end = date_i + tw;
            if(i == 0) {
                residentBefore[i] = irs;
                dateElapsedToFinalizedRB[i] = date_crossing_c_i;
            } else {
                residentBefore[i] = residentAfter[i-1];
                std::string pre_RA = "";
                pre_RA = dateElapsedToFinalizedRA[i-1];
                if(pre_RA == "") {
                    dateElapsedToFinalizedRB[i] = date_crossing_c_i;
                } else {
                    Date pre_RA_Date = str_to_date(as<std::string>(dateElapsedToFinalizedRA[i-1]));
                    //// check with initial status finalised date
                    if(irs_date == pre_RA_Date) {
                        residentBefore[i] = irs_finalised;
                    }
                    dateElapsedToFinalizedRB[i] = date_to_str(std::max(pre_RA_Date, date_i));
                }
           }
           Date date_RB_i = str_to_date(as<std::string>(dateElapsedToFinalizedRB[i]));
           //// initial check/filter out
           //// Step 3.1 of 3 - resolve the JourneyUnderReview – Non-Migrating direction
           if((direction_i == 1 && residentBefore[i] == 1) ||
              (direction_i ==0 && !(residentBefore[i] == 1))) {
               residentAfter[i] = residentBefore[i];
               dateElapsedToFinalizedRA[i] = date_to_str(std::max(add_days(date_i, 0),
                                                               date_RB_i));
               is_long_term_mig[i] = 0;
               std::string dateLTM = "";
               dateLTM = dateElapsedToFinalizedLTM[i];
               if(dateLTM == "") {
                   dateElapsedToFinalizedLTM[i] = date_to_str(std::max(add_days(date_i, 0),
                                                        date_RB_i));
               }
               continue;
           }
           //// Step 3.2 of 3 - resolve the JourneyUnderReview – Migrating direction
           int w_seq_start = i + 1;
           Date date_i_8 = add_days(date_RB_i, ts); // the special instant LTM assignment date cutoff
           //// get days elapsed between journey’s crossing date in the current window
           int accumulatedDaysMigration = 0;
           int accumulatedDaysNoMigration = 0;
           int ind = 0;
           //// resolve the journey under review
           for(int m = w_seq_start; m <= len_journeys; m ++) {
               Date date_m_pre = str_to_date(as<std::string>(date_crossing[m-1]));
               if(date_m_pre > date_obs_end) break;
               int days_elapsed = days_to_next_crossing[m-1];
               if(ind%2 == 0) {
                   accumulatedDaysMigration += days_elapsed;
               } else {
                   accumulatedDaysNoMigration += days_elapsed;
               }
               int resolvedDaysMigration = tm + accumulatedDaysNoMigration; // opposite activity in/out NZ
               int resolvedDaysNoMigration = tn + accumulatedDaysMigration;
               if(accumulatedDaysNoMigration > tn) {
                   residentAfter[i] = residentBefore[i];
                   dateElapsedToFinalizedRA[i] = date_to_str(std::max(add_days(date_i, resolvedDaysNoMigration),
                                                                   date_RB_i));
                   is_long_term_mig[i] = 0;
                   std::string dateLTM = "";
                   dateLTM = dateElapsedToFinalizedLTM[i];
                   if(dateLTM == "") {
                       dateElapsedToFinalizedLTM[i] = date_to_str(std::max(add_days(date_i, resolvedDaysNoMigration),
                                                            date_RB_i));
                   }
                   break;
                }
                if(accumulatedDaysMigration >= tm) {
                    if(residentBefore[i] == 1) residentAfter[i] = 0;
                    else residentAfter[i] = 1;
                    dateElapsedToFinalizedRA[i] = date_to_str(std::max(add_days(date_i, resolvedDaysMigration),
                                                                   date_RB_i));
                    is_long_term_mig[i] = 1;
                    dateElapsedToFinalizedLTM[i] = date_to_str(std::max(add_days(date_i, resolvedDaysMigration),
                                                                    date_RB_i));
                    break;
                }
                //// 8 month rule - LTM=0
                //// check for instant assignment of the return journeys given journey
                //// under review is migration direction
                if(ind%2 == 0) {
                    if(i + 1 < len_journeys) {
                        Date date_s = str_to_date(as<std::string>(date_crossing[m]));
                        if(date_s < date_i_8) {
                            is_long_term_mig[m] = false;
                            std::string dateLTM = "";
                            dateLTM = dateElapsedToFinalizedLTM[m];
                            if(dateLTM == "") {
                                dateElapsedToFinalizedLTM[m] = date_to_str(std::max(date_s, date_RB_i));
                            }
                        }
                    }
                }
                ind++;
           }
        }
    } else {
        if((direction_ini == 1 && residentBefore[0] == 1) || (direction_ini == 0 && !(residentBefore[0] == 1))) {
            residentAfter[0] = residentBefore[0];
            dateElapsedToFinalizedRA[0] = date_to_str(std::max(add_days(date_ini, 0),
                                                           date_RB_ini));
            is_long_term_mig[0] = 0;
            std::string dateLTM = "";
            dateLTM = dateElapsedToFinalizedLTM[0];
            if(dateLTM == "") {
                dateElapsedToFinalizedLTM[0] = date_to_str(std::max(add_days(date_ini, 0),
                                                              date_RB_ini));
            }
        } else {
            if((date_current - date_ini) >= tm) {
                if(residentBefore[0] == 1) residentAfter[0] = 0;
                else residentAfter[0] = 1;
                dateElapsedToFinalizedRA[0] = date_to_str(std::max(add_days(date_ini, tm),
                                                             date_RB_ini));
                is_long_term_mig[0] = 1;
                dateElapsedToFinalizedLTM[0] = date_to_str(std::max(add_days(date_ini, tm),
                                                              date_RB_ini));
            } else {
                residentAfter[0] = residentBefore[0];
                dateElapsedToFinalizedRA[0] = date_to_str(std::max(add_days(date_ini, tm),
                                                             date_RB_ini));
                is_long_term_mig[0] = 0;
                std::string dateLTM = "";
                dateLTM = dateElapsedToFinalizedLTM[0];
                if(dateLTM == "") {
                    dateElapsedToFinalizedLTM[0] = date_to_str(std::max(add_days(date_ini, tm),
                                                                date_RB_ini));
                }
            }
        }
    }
    //// construct the result as an object of List  
     return(List::create( Named("journeyId") = journeyId,
                               _["personId"] = personId,
                               _["date_crossing"] = date_crossing,
                               _["journey_sequence"] = journey_sequence,
                               _["days_to_next_crossing"] = days_to_next_crossing,
                               _["is_arrival"] = is_arrival,
                               _["res_status_before"] = residentBefore,
                               _["res_status_after"] = residentAfter,
                               _["is_long_term_mig"] = is_long_term_mig,
                               _["date_finalised_res_before"] = dateElapsedToFinalizedRB,
                               _["date_finalised_res_after"] = dateElapsedToFinalizedRA,
                               _["date_finalised_LTM"] = dateElapsedToFinalizedLTM,
                               _["journeyId_prev"] = journeyId_prev,
                               _["stringsAsFactors"] = false));
}


//' Processing RBC for a list of person.
//' 
//' This function is used to resolve a list of person's journeys,
//' i.e., classifying a list of people and marking it whether or not 
//' to be a long term migrant based on the person's journeys. 
//' This function is used internally inside the package and shouldn't 
//' be exposed to the outside caller.
//' 
//' @param cross_data The personal crossing data for RBC process
//' @param ini_status_data the initial residence status data
//' @param tw Windows Size, by default, it is 487 days.
//' @param ty Threshold of Year, by default, it is 365 days.
//' 
//' @return A data frame object of classified / labelled journeys
//' 
// [[Rcpp::export]]
Rcpp::DataFrame run_rbc_process_core(Rcpp::List &cross_data,
                                     Rcpp::List &ini_status_data,
                                     int &tw,
                                     int &ty) {
    //// get data in columns
    IntegerVector journeyId = cross_data["journeyId"];
    IntegerVector personId = cross_data["personId"];
    StringVector date_crossing = cross_data["date_crossing"];
    IntegerVector journey_sequence = cross_data["journey_sequence"];
    IntegerVector is_arrival = cross_data["is_arrival"];
    IntegerVector journeyId_prev = cross_data["journeyId_prev"];
    long len_journeys = journeyId.length();
    if(len_journeys == 0) throw JourneyException();
    //// initialize extra variables
    IntegerVector residentBefore(len_journeys);
    IntegerVector residentAfter(len_journeys);
    IntegerVector is_long_term_mig(len_journeys);
    IntegerVector days_to_next_crossing(len_journeys);
    StringVector dateElapsedToFinalizedRB(len_journeys);
    StringVector dateElapsedToFinalizedLTM(len_journeys);
    StringVector dateElapsedToFinalizedRA(len_journeys); 
    IntegerVector errorCode(len_journeys);
    StringVector errorMessage(len_journeys);
    //// initial residence 
    IntegerVector v_personId = ini_status_data["personId"];
    NumericVector v_res_status_initial = ini_status_data["res_status_initial"];
    StringVector v_date_finalised = ini_status_data["date_finalised"];
  
    IntegerVector t_selectedIndex(len_journeys);
    int spt = 0; //// the current selected index position
    long cpid = personId[0]; //// get the first person id
    long get_pid = cpid; //initializing
    //// assume the data is sorted properly
    for(long i = 0; i<= len_journeys; i++) {
        if(i < len_journeys) {
            days_to_next_crossing[i] = tw;
            get_pid = personId[i];
            if(get_pid == cpid) {
                t_selectedIndex[spt] = i;
                if(spt > 0) {
                    Date date_i = str_to_date(as<std::string>(date_crossing[i]));
                    Date date_pre = str_to_date(as<std::string>(date_crossing[i- 1]));
                    days_to_next_crossing[i-1] = date_i - date_pre;
                }
                spt ++;
                continue; //// continue until all journeys belong to this person is fetched
            }
        }
        IntegerVector selectedIndex(spt);
        selectedIndex = t_selectedIndex[Range(0,spt-1)];
        long ini_index = selectedIndex[0];
        
        //// process the selected block
        Rcpp::List selected_person_data =  List::create( Named("journeyId") = journeyId[selectedIndex],
                                                                         _["personId"] = personId[selectedIndex],
                                                                         _["date_crossing"] = date_crossing[selectedIndex],
                                                                         _["journey_sequence"] = journey_sequence[selectedIndex],
                                                                         _["days_to_next_crossing"] = days_to_next_crossing[selectedIndex],
                                                                         _["is_arrival"] = is_arrival[selectedIndex],
                                                                         _["res_status_before"] = residentBefore[selectedIndex],
                                                                         _["res_status_after"] = residentAfter[selectedIndex],
                                                                         _["is_long_term_mig"] = is_long_term_mig[selectedIndex],
                                                                         _["date_finalised_res_before"] = dateElapsedToFinalizedRB[selectedIndex],
                                                                         _["date_finalised_res_after"] = dateElapsedToFinalizedRA[selectedIndex],
                                                                         _["date_finalised_LTM"] = dateElapsedToFinalizedLTM[selectedIndex],
                                                                         _["journeyId_prev"] = journeyId_prev[selectedIndex],
                                                                         _["stringsAsFactors"] = false);
        //// extract initial residence status
        int int_res_status = 0;
        std::string initial_date_finalised = "";
        //// initial default res status
        int_res_status = is_arrival[ini_index];
        if(int_res_status == 1) {
            int_res_status = 0;
        } else {
            int_res_status = 1;
        }
        initial_date_finalised = date_crossing[ini_index];
        if(v_personId.length() > 0) {
            Rcpp::Nullable<IntegerVector> v_index = get_first_index_IRS(v_personId, cpid);
            if(!Rf_isNull(v_index)) {
                Rcpp::IntegerVector this_idx = v_index.get();
                int_res_status = as<int>(v_res_status_initial[this_idx]);
                initial_date_finalised = as<std::string>(v_date_finalised[this_idx]);
            }
        }
        
        //// execute the RBC core
        Rcpp::List resolved;
        int error_code = 0;
        std::string error_message = "";
        try {
            resolved =  rcpp_resolve(selected_person_data, int_res_status, initial_date_finalised, tw, ty);
        } catch (SequenceException &ex) {
            error_code = ex.get_error_code();
            error_message = ex.what();
        } catch (IdenticalDirectionException &ex) {
            error_code = ex.get_error_code();
            error_message = ex.what();
        } catch (InvalidDirectionException &ex) {
            error_code = ex.get_error_code();
            error_message = ex.what();
        } catch (InvalidCrossDateException &ex) {
            error_code = ex.get_error_code();
            error_message = ex.what();
        } catch (JourneyException &ex) {
            error_code = ex.get_error_code();
            error_message = ex.what();
        } catch (std::exception &ex) {
            error_code = 99;
            error_message = ex.what();
        }
        
        //// update the original table
        if(error_code > 0) {
            //// The reject people from this data cleaning process and migrbc will be combined together
            //// and their journeys info will be updated to the CrossingResult table, with the states
            //// of residenceBefore, residenceAfter to its initial status, and LTM = 0, all dates set
            //// to the crossing date of each journeys of the people with error (defined in MDTU-1312).
            residentBefore[selectedIndex] = int_res_status;
            residentAfter[selectedIndex] = int_res_status;
            is_long_term_mig[selectedIndex] = 0;
            dateElapsedToFinalizedRB[selectedIndex] = date_crossing[selectedIndex];
            dateElapsedToFinalizedRA[selectedIndex] = date_crossing[selectedIndex];
            dateElapsedToFinalizedLTM[selectedIndex] = date_crossing[selectedIndex];
            errorCode[selectedIndex] = error_code;
            errorMessage[selectedIndex] = error_message.c_str();
        } else {
            IntegerVector res_residentBefore = resolved["res_status_before"];
            residentBefore[selectedIndex] = res_residentBefore;
            IntegerVector res_residentAfter = resolved["res_status_after"];
            residentAfter[selectedIndex] = res_residentAfter;
            IntegerVector res_is_long_term_mig = resolved["is_long_term_mig"];
            is_long_term_mig[selectedIndex] = res_is_long_term_mig;
            StringVector date_rb = resolved["date_finalised_res_before"];
            dateElapsedToFinalizedRB[selectedIndex] = date_rb;
            StringVector date_ra = resolved["date_finalised_res_after"];
            dateElapsedToFinalizedRA[selectedIndex] = date_ra;
            StringVector date_ltm = resolved["date_finalised_LTM"];
            dateElapsedToFinalizedLTM[selectedIndex] = date_ltm;
            IntegerVector res_days_to_next_crossing = resolved["days_to_next_crossing"];
            days_to_next_crossing[selectedIndex] = res_days_to_next_crossing;
            IntegerVector res_journeyId_prev = resolved["journeyId_prev"];
            journeyId_prev[selectedIndex] = res_journeyId_prev;
            IntegerVector res_journey_sequence = resolved["journey_sequence"];
            journey_sequence[selectedIndex] = res_journey_sequence;
            errorCode[selectedIndex] = error_code;
            errorMessage[selectedIndex] = "";
        }
        //reset
        t_selectedIndex.erase(0, spt-1);
        t_selectedIndex[0] = i;
        spt = 1;
        cpid = get_pid;
    }

    return(DataFrame::create(Named("journeyId") = journeyId,
                              _["journeyId_prev"] = journeyId_prev,
                              _["personId"] = personId,
                              _["date_crossing"] = date_crossing,
                              _["journey_sequence"] = journey_sequence,
                              _["days_to_next_crossing"] = days_to_next_crossing,
                              _["is_arrival"] = is_arrival,
                              _["res_status_before"] = residentBefore,
                              _["res_status_after"] = residentAfter,
                              _["is_long_term_mig"] = is_long_term_mig,
                              _["date_finalised_res_before"] = dateElapsedToFinalizedRB,
                              _["date_finalised_res_after"] = dateElapsedToFinalizedRA,
                              _["date_finalised_LTM"] = dateElapsedToFinalizedLTM,
                              _["error_code"] = errorCode,
                              _["error_message"] = errorMessage,
                              _["stringsAsFactors"] = false));
}



//' Processing RBC for a list of person.
//' 
//' This function is used to resolve a list of person's journeys with error,
//' This function is used internally inside the package and shouldn't 
//' be exposed to the outside caller.
//' 
//' @param cross_data The personal crossing data for RBC process
//' @param ini_status_data the initial residence status data
//' @param error_message The error message.
//' @param tw Windows Size, by default, it is 487 days.
//' 
//' @return A data frame object of classified / labelled journeys
//' 
// [[Rcpp::export]]
Rcpp::DataFrame run_rbc_process_with_error(Rcpp::List &cross_data,
                                      Rcpp::List &ini_status_data,
                                      StringVector &error_message,
                                      int &tw) {
    int error_code = 99;
    //// get data in columns
    IntegerVector journeyId = cross_data["journeyId"];
    IntegerVector personId = cross_data["personId"];
    StringVector date_crossing = cross_data["date_crossing"];
    IntegerVector journey_sequence = cross_data["journey_sequence"];
    IntegerVector is_arrival = cross_data["is_arrival"];
    IntegerVector journeyId_prev = cross_data["journeyId_prev"];
    long len_journeys = journeyId.length();
    if(len_journeys == 0) throw JourneyException();
    //// initialize extra variables
    IntegerVector residentBefore(len_journeys);
    IntegerVector residentAfter(len_journeys);
    IntegerVector is_long_term_mig(len_journeys);
    IntegerVector days_to_next_crossing(len_journeys);
    StringVector dateElapsedToFinalizedRB(len_journeys);
    StringVector dateElapsedToFinalizedLTM(len_journeys);
    StringVector dateElapsedToFinalizedRA(len_journeys); 
    IntegerVector errorCode(len_journeys);
    StringVector errorMessage(len_journeys);
    //// initial residence 
    IntegerVector v_personId = ini_status_data["personId"];
    NumericVector v_res_status_initial = ini_status_data["res_status_initial"];
    StringVector v_date_finalised = ini_status_data["date_finalised"];
    
    IntegerVector t_selectedIndex(len_journeys);
    int spt = 0; //// the current selected index position
    long cpid = personId[0]; //// get the first person id
    long get_pid = cpid;
    //// assume the data is sorted properly
    for(long i = 0; i<= len_journeys; i++) {
        if(i < len_journeys) {
            days_to_next_crossing[i] = tw;
            get_pid = personId[i];
            if(get_pid == cpid) {
                t_selectedIndex[spt] = i;
                if(spt > 0) {
                    Date date_i = str_to_date(as<std::string>(date_crossing[i]));
                    Date date_pre = str_to_date(as<std::string>(date_crossing[i- 1]));
                    days_to_next_crossing[i-1] = date_i - date_pre;
                }
                spt ++;
                continue; //// continue until all journeys belong to this person is fetched
            }
        }
      
        IntegerVector selectedIndex(spt);
        selectedIndex = t_selectedIndex[Range(0,spt-1)];
        long ini_index = selectedIndex[0];
      
        //// extract initial residence status
        int int_res_status = 0;
        std::string initial_date_finalised = "";
        //// initial default res status
        int_res_status = is_arrival[ini_index];
        if(int_res_status == 1) {
            int_res_status = 0;
        } else {
            int_res_status = 1;
        }
        initial_date_finalised = date_crossing[ini_index];
        if(v_personId.length() > 0) {
            Rcpp::Nullable<IntegerVector> v_index = get_first_index_IRS(v_personId, cpid);
            if(!Rf_isNull(v_index)) {
                Rcpp::IntegerVector this_idx = v_index.get();
                int_res_status = as<int>(v_res_status_initial[this_idx]);
                initial_date_finalised = as<std::string>(v_date_finalised[this_idx]);
            }
        }
        //// execute the RBC core
        residentBefore[selectedIndex] = int_res_status;
        residentAfter[selectedIndex] = int_res_status;
        is_long_term_mig[selectedIndex] = 0;
        dateElapsedToFinalizedRB[selectedIndex] = date_crossing[selectedIndex];
        dateElapsedToFinalizedRA[selectedIndex] = date_crossing[selectedIndex];
        dateElapsedToFinalizedLTM[selectedIndex] = date_crossing[selectedIndex];
        errorCode[selectedIndex] = error_code;
        errorMessage[selectedIndex] = error_message;
        //// reset
        t_selectedIndex.erase(0, spt-1);
        t_selectedIndex[0] = i;
        spt = 1;
        cpid = get_pid;
    }
    
    return(DataFrame::create(Named("journeyId") = journeyId,
                              _["journeyId_prev"] = journeyId_prev,
                              _["personId"] = personId,
                              _["date_crossing"] = date_crossing,
                              _["journey_sequence"] = journey_sequence,
                              _["days_to_next_crossing"] = days_to_next_crossing,
                              _["is_arrival"] = is_arrival,
                              _["res_status_before"] = residentBefore,
                              _["res_status_after"] = residentAfter,
                              _["is_long_term_mig"] = is_long_term_mig,
                              _["date_finalised_res_before"] = dateElapsedToFinalizedRB,
                              _["date_finalised_res_after"] = dateElapsedToFinalizedRA,
                              _["date_finalised_LTM"] = dateElapsedToFinalizedLTM,
                              _["error_code"] = errorCode,
                              _["error_message"] = errorMessage,
                              _["stringsAsFactors"] = false));
}
