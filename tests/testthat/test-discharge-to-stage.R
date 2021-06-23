test_that("discharge_to_stage", {
  data <- data.frame(DateTime = dtt_disaggregate(as.POSIXct("2002-02-02")))
  data$Discharge <- as.double(1:nrow(data))
  
  r1d <- data.frame(Discharge = as.double(c(2,4:7)))
  r1d$Stage = r1d$Discharge^0.5
  
  expect_error(dtq_discharge_to_stage(data, r1d, rate_down = 1), 
               "argument 'rate_down' is not used [(]yet[)]")
  
  data <- dtq_discharge_to_stage(data, r1d)
  
  expect_identical(data$Stage[1:8], c(NA,2^0.5,mean(c(2^0.5,4^0.5)),(4:7)^0.5,NA))
  
  data1 <- dtq_discharge_to_stage(data, r1d, n = 1)
  
  expect_identical(data1$Stage[1:9], c(NA, NA,2^0.5,mean(c(2^0.5,4^0.5)),(4:7)^0.5,NA))
  expect_identical(data1[c("DateTime", "Discharge")], 
                   data[c("DateTime", "Discharge")])
})

