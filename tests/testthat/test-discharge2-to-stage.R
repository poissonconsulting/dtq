test_that("discharge2_to_stage", {
  data <- data.frame(DateTime = dttr::dtt_disaggregate(as.POSIXct("2002-02-02")))
  data$Discharge <- as.double(1:nrow(data))
  data$Discharge2 <- as.double(1:nrow(data)) * 10
  
  r1d <- expand.grid(Discharge = as.double(c(2,4:7)), Discharge2 = c(10,20,30,40,50))
  r1d$Stage = r1d$Discharge^0.5 + r1d$Discharge2
  
  r1d <- r1d[order(r1d$Discharge, r1d$Discharge2),]
  
  expect_error(dtq_discharge2_to_stage(data, r1d, rate_down = 1), 
               "argument 'rate_down' is not used [(]yet[)]")
  
  data <- dtq_discharge2_to_stage(data, r1d)
  
  expect_equal(data$Stage[1:6], 
               c(NA, 21.4142135623731, 31.7071067811865, 
                 42, 52.2360679774998, NA))
  
  data1 <- dtq_discharge2_to_stage(data, r1d, n = 1)
  
  expect_equal(data1$Stage[1:6], 
               c(NA, NA, 31.4142135623731, 41.7071067811865, 52, NA))
  
  expect_identical(data1[c("DateTime", "Discharge")], 
                   data[c("DateTime", "Discharge")])
})

