context("pulse")

test_that("dtq_pulse", {
  data <- data.frame(DateTime = dttr::dtt_disaggregate(as.POSIXct("2002-02-02")))
  data$Discharge <- as.double(1:nrow(data))
  
  expect_identical(check_dtq(data), data)
  expect_error(check_dtq(data, rate_up = 0.5), 
               "the values in the differenced column 'Discharge' of data must lie between -Inf and 0.5")
  
  pulse <- dtq_pulse(data)
  expect_identical(pulse$Discharge[1:3], c(1,0,3))
  
  pulse <- dtq_pulse(data, start = data$DateTime[5], target = 100, rate_up = 10,
                     rate_down = 100, duration = 5L)
  
  expect_identical(pulse$Discharge[5:11], c(5,15,25,35,45,55,11))
  
  pulse <- dtq_pulse(data, start = data$DateTime[5], target = 100, rate_up = 10,
                     rate_down = 7.5, duration = 5L)

  expect_identical(pulse$Discharge[5:11], c(5,15,25,33.5,26,18.5,11))
})

test_that("dtq_pulse with no change", {
  data <- data.frame(DateTime = dttr::dtt_disaggregate(as.POSIXct("2002-02-02")))
  data$Discharge <- 0

  pulse <- dtq_pulse(data, rate_up = 1)
  expect_identical(pulse$Discharge[1:3], rep(0, 3))
})