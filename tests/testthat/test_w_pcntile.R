library(SocEpi)
context("Numeric outputs")

test_that("w_pcntile is numeric", {

  data <- data.frame(population = round(rnorm(100, 200, 50), 0),
                     var = rnorm(100, 0, 1),
                     pcnt = rnorm(100, 10, 2)
                      )

  data$pcnt <- ifelse(data$pcnt > 100, 100, data$pcnt)

  expect_is(w_pcntile(data, population, var), "numeric")
  expect_is(zscore(data$population, data$pcnt)$z.score, "numeric")

})
