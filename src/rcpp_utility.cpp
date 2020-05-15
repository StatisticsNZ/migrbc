#include <Rcpp.h>
using namespace Rcpp;

Date str_to_date(std::string date_in_string) {
    return Date(date_in_string, "%Y-%m-%d");
}

bool is_date(std::string date_in_string){
    try{
         Date test  = Date(date_in_string, "%Y-%m-%d");
         if(test.getYear() < 1900) return (false);
         if(test.getMonth() < 0) return (false);
         if(test.getMonth() > 12) return (false);
         if(test.getDay() < 0) return (false);
         if(test.getDay() > 31) return (false);
         return(true);
    }catch(...) {
         return(false);
    }
}

std::string date_to_str(Date date) {
    return date.format("%Y-%m-%d");
}

Date get_current_date() {
    time_t rawtime;
    struct tm * timeinfo;
    char buffer[80];
    time (&rawtime);
    timeinfo = localtime(&rawtime);
    strftime(buffer,80,"%Y-%m-%d %I:%M:%S",timeinfo);
    std::string str(buffer);
    return Date(str, "%Y-%m-%d");
}

Rcpp::Date add_days(Rcpp::Date date, int days) {
    return(date + days);
}

Rcpp::Nullable<Rcpp::IntegerVector> get_first_index_IRS(Rcpp::IntegerVector person_ids, long value) {
    long len = person_ids.length();
    IntegerVector index = 0;
    bool found = false;
    for(long i = 0; i < len; i++){
      if (person_ids[i] == value) {
        found = true;
        index = i;
        break;
      }
    }
    
    if(found)return (index);
    else return(R_NilValue);
}
