#include <Rcpp.h>
using namespace Rcpp;

Date str_to_date(std::string date_in_string);
bool is_date(std::string date_in_string);
std::string date_to_str(Date date);
Date get_current_date();
Rcpp::Date add_days(Rcpp::Date date, int days);
Rcpp::Nullable<Rcpp::IntegerVector> get_first_index_IRS(Rcpp::IntegerVector person_ids, long value);
