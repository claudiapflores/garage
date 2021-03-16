# Load recurrence_interval_prob function to test
recurrence_interval_prob <- function(Y,R){
  # Body
  T <- Y + 1 # T is recurrence interval, Y is years on record
  Prob <- (1)/(T/R) # R is ranking
  # Error checking
  R = ifelse((R < 0),
             return("Ranking must be a positive integer"),
             R)
  # Output is the probability
  risk <- case_when( # Print the following "" if certain conditions (e.g. <,>,=...) for Prob results are met
    Prob > 0.5 ~ "High",
    Prob == 0.5 ~ "Medium",
    Prob < 0.5 ~ "Low"
  )
  return(risk)
}

# Test that contains a test data frame for the function recurrence_interval_prob

test_that("test_recurrence_interval_prob" ,
          {rip_test_data = as.data.frame(cbind(R = c(1:5), max_annual_discharge_m3_s = sample(1:999, 5, replace = FALSE), year= c(2001,2002,2003,2004,2005),
                                           Y = rep(5, times = 5)))
          expect_length(rip_test_data$R,5) # Expected column length should be five
          expect_length(rip_test_data$Y,5) # Expected column length should be five
          expect_true(min(rip_test_data$Y) > 0) # The number of years on record should be greater than 0 and not negative
          expect_true(length(rip_test_data$R) == max(rip_test_data$Y)) # The column length of R should equal to the values or max value in column Y
          expect_equal(recurrence_interval_prob(Y = 5, R = 3),"Medium") # Using the values Y = 5, and R = 3 in the recurrence_interval_prob function, the answer should be "Medium"
          })



