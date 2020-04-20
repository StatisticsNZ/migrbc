#include <Rcpp.h>
#include <exception>
using namespace Rcpp;

class SequenceException : public std::exception {
    static const int error_code = 1;
    public: int get_error_code() {
      return error_code;
    }
    
    std::string what() /* override */ {
      return "The sequence is not in the right order.";
    }
};

class IdenticalDirectionException : public std::exception {
    static const int error_code = 2;
    public: int get_error_code() {
      return error_code;
    }
    
    std::string what() /* override */ {
      return "'is_arrival' cannot be identical to previous one.";
    }
};

class InvalidDirectionException : public std::exception {
    static const int error_code = 3;
    public: int get_error_code() {
      return error_code;
    }
      
    std::string what() /* override */ {
      return "Invalid direction code";
    }
};

class InvalidCrossDateException : public std::exception {
    static const int error_code = 4;
    public: int get_error_code() {
      return error_code;
    }
    
    std::string what() /* override */ {
      return "The cross date contains invalid value.";
    }
};

class JourneyException : public std::exception {
    static const int error_code = 5;
    public: int get_error_code() {
      return error_code;
    }
    
    std::string what() /* override */ {
      return "The person does not have any journey records.";
    }
};
