############################################################################## 
context("pre_process")
##############################################################################

setup_data <- function(rows_data) {
    setup_random_test_data(rows_data, 
                          initial_date = "2001-01-01", 
                          numJourneys = 5, 
                          min = 0, 
                          max = 100)
}

describe("Run preprocess with 1000", {
    it("Run pre_process without initial res status", {
        person_data <- setup_data(1000)
        expect_error(res <- pre_process(person_data, NULL, 10), NA)
        expect_true(length(res) == 10)
        expect_error(res <- pre_process(person_data, NULL, 13), NA)
        expect_true(length(res) == 13)
        expect_error(res <- pre_process(person_data, NULL, 18), NA)
        expect_true(length(res) == 18)
        expect_error(res <- pre_process(person_data, NULL, 1000), NA)
        expect_true(length(res) == 1000)
        expect_error(res <- pre_process(person_data, NULL, 1200), NA)
        expect_true(length(res) == 1000)
    })
    
    it("Run pre_process without initial res status", {
        person_data <- setup_data(1234)
        expect_error(res <- pre_process(person_data, NULL, 10), NA)
        expect_true(length(res) == 10)
        expect_error(res <- pre_process(person_data, NULL, 13), NA)
        expect_true(length(res) == 13)
        expect_error(res <- pre_process(person_data, NULL, 18), NA)
        expect_true(length(res) == 18)
        expect_error(res <- pre_process(person_data, NULL, 1000), NA)
        expect_true(length(res) == 617)
        expect_error(res <- pre_process(person_data, NULL, 1500), NA)
        expect_true(length(res) == 1234)
    })
})


describe("Run preprocess with unhappy cases", {
    it("With NULL data provided", {
        expect_error(res <- pre_process(NULL, NULL, 10), 
                     "The input data must be a type of data frame")
    })
    
    it("With NA data provided", {
        expect_error(res <- pre_process(NULL, NULL, 10), 
                     "The input data must be a type of data frame")
    })
    
    it("With None Data frame data provided", {
        expect_error(res <- pre_process(c(1, 2, 3), NULL, 10), 
                     "The input data must be a type of data frame")
    })
    
    it("With empty dataset", {
        person_data <- setup_data(1)
        person_data <- person_data[- (1:5), ]
        expect_error(res <- pre_process(person_data, NULL, 10), 
                     "No journey data provided")
    })
    
    it("With invalid column name", {
        ## The 'same' with automatic column names:
        t_3 <- LETTERS[1:3]
        data_db <- data.frame(1, 1:10, sample(t_3, 10, replace = TRUE))
        expect_error(res <- pre_process(data_db, NULL, 10), 
                     "The journey data does not contain the required columns")
    })
    
    it("With invalid initial residence data", {
        person_data <- setup_data(10)
        expect_error(res <- pre_process(person_data, NA, 10), 
                     "*argument of type*")
        expect_error(res <- pre_process(person_data, "xyz", 10), 
                     "*argument of type*")
    })
})
