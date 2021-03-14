rip_test_data = as.data.frame(cbind(R = c(1:5), max_annual_discharge_m3_s = sample(1:999, 5, replace = FALSE), year= c(2001,2002,2003,2004,2005),
                                    Y = rep(5, times=5)))


#save this to a file called test_spring_summary
test_that("recurrence_interval_prob_works" ,
          {rip_test_data = as.data.frame(cbind(R = c(1:5), max_annual_discharge_m3_s = sample(0:999, 5, replace = FALSE), year= c(2001,2002,2003,2004,2005),
                                           Y = rep(5, times=5)))
          expect_length(Y, 5)
          expect_length(R,5)
          expect_true(Y = 5)
          })


