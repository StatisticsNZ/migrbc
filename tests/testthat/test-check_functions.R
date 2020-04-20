#############################################################################
context("check_functions")
#############################################################################
describe("Run check_integer", {
    it("happy case", {
        expect_true(is.null(check_integer("test", 1)))
        expect_true(is.null(check_integer(NULL, 1)))
        ## NULL should return NULL
        expect_true(is.null(check_integer("test", NULL)))
    })
    it("unhappy case", {
        expect_error(check_integer("test", "x"), "*is non-numeric*")
        expect_error(check_integer("test", c(1, 2)), "*does not have length 1*")
        expect_error(check_integer("test", 1.23), "*is not an integer*")
        expect_error(check_integer("test", 0), "*is less than*")
        expect_error(check_integer("test", -1), "*is less than*")
    })
})

describe("Run check_and_tidy_date", {
    it("happy case", {
        expect_true(methods::is(check_and_tidy_date("2012-12-01", "test"), 
                                "Date"))
        expect_true(methods::is(check_and_tidy_date("2012-12-01", ""), 
                                "Date"))
    })
    it("unhappy case", {
        expect_error(check_and_tidy_date(NULL, "x"), 
                     "*cannot be NULL*")
        expect_error(check_and_tidy_date(c("1", "2"), "x"), 
                     "*does not have length*")
        expect_error(check_and_tidy_date(c("1", "2"), "x"), 
                     "*does not have length*")
        expect_error(check_and_tidy_date(NA, "x"), 
                     "*is missing*")
        expect_error(check_and_tidy_date(123, "x"),
                     "*but not Character*")
        expect_error(check_and_tidy_date("xxxx", "x"),
                     "*All formats failed to parse. No formats found*")
    })
})

describe("Run check_object_size", {
    it("happy case", {
        expect_true(is.null(check_object_size("test", 1)))
        expect_true(is.null(check_object_size(rep("A",  time = 2), 
                             max_ram = 200, target_unit = "bytes")))
        expect_true(is.null(check_object_size(rep("A",  time = 1000), 
                                              max_ram = 200,
                                              target_unit = "Kb")))
        expect_true(is.null(check_object_size(rep("A",  time = 10000000), 
                                              max_ram = 200,
                                              target_unit = "Mb")))
    })
    
    it("unhappy case", {
        expect_error(check_object_size("xxxx", "x"),
                     "*must be a number*")
        expect_error(check_object_size(rep("A", 1), 1, 
                                       target_unit = c("bytes")),
                     "*The maximum object size is*")
        expect_error(check_object_size(rep("A", 10000000), 200, 
                                       target_unit = c("Kb")),
                     "*The maximum object size is*")
    })
})

describe("test other functions", {
    it("happy cases", {
        expect_true(is.null(check_is_logic(1)))
        expect_true(is.null(check_is_logic(0)))
        expect_true(is.null(check_is_logic(TRUE)))
        expect_true(is.null(check_is_logic(FALSE)))
        expect_true(is.null(check_is_logic(c(1,0,1,0))))
        expect_error(
            check_and_tidy_date_first_last(
                date="2017-01-01",
                date_crossing = "2017-01-01",
                name = "date_first"), NA)
        expect_error(
            check_and_tidy_date_first_last(
                date="2016-01-01",
                date_crossing = "2017-01-01",
                name = "date_last"), 
            "*is earlier than last element of*")
        expect_error(
            check_and_tidy_date_first_last(
                date="2018-01-01",
                date_crossing = "2017-01-01",
                name = "date_first"), 
            "*is later than first element of*")
        expect_error(check_and_tidy_date_crossing("2017-01-01"), 
                     NA)
        expect_error(
            check_and_tidy_date_first_last(
                date=NA,
                date_crossing = "2017-01-01",
                name = "date_first"), "*is missing*")
    })
    it("unhappy cases", {
        expect_error(check_is_logic("xyz"), "*has class*")
        expect_error(check_is_logic(NA),  "*has missing values*")
        expect_error(check_is_logic(NULL), "*has class*")
        expect_error(check_is_logic(c(1,0,5,0)), 
                "*is numeric, but has values not equal to 0, 1*")
        expect_error(check_and_tidy_date_first_last(), 
                     "*is missing, with no default*")
        expect_error(
            check_and_tidy_date_first_last(date="2017-01-01"), 
            "*is missing, with no default*")
        expect_error(
            check_and_tidy_date_first_last(
                date="2017-01-01",
                date_crossing = "2017-01-01",
                name = "test"), 
            "*'arg' should be one of*")
        expect_error(
            check_and_tidy_date_first_last(
                date="2017-21-01",
                date_crossing = "2017-01-01",
                name = "date_first"), 
            "*has invalid year-month-date format*")
        expect_error(
            check_and_tidy_date_first_last(
                date=c("2017-01-01", "2018-01-01"),
                date_crossing = "2017-01-01",
                name = "date_first"), 
            "*does not have length 1*")
        
       expect_error(check_positive_number("x", "test"),
                     "*is non-numeric*")  
       expect_error(check_positive_number(-1, "test"),
                    "*is non-positive*")
       expect_error(check_positive_number(NA, "test"),
                    "*is missing*")
       expect_error(check_positive_number(c(1,2), "test"),
                    "*does not have length*")
       expect_error(check_and_tidy_date_crossing(NA), 
                    "*has missing values*")
       expect_error(check_and_tidy_date_crossing(NULL), 
                    "*has class*")
       expect_error(check_and_tidy_date_crossing(1), 
                    "*has class*")
       expect_error(check_and_tidy_date_crossing("xyz"), 
                    "*a standard unambiguous format*")
       expect_error(check_and_tidy_date_crossing(c("2012-01-01",
                                                   "2012-01-01",
                                                   "0000-30-65")),
                    "*contains invalid date*")
       init_res_status_data <- data.frame(res_status_initial = logical(), 
                                          personId = numeric(), 
                                          stringsAsFactors = FALSE)

       expect_error(check_ini_res_data_columns(init_res_status_data),
                    "*contain the required columns*")
       expect_error(check_ini_res_data_columns(c(1,2)),
                    "* must be a type of data frame*")
       
       expect_error(check_data_columns(init_res_status_data),
                    "*contain the required columns*")
       expect_error(check_data_columns(c(1,2)),
                    "* must be a type of data frame*")
    })
})
