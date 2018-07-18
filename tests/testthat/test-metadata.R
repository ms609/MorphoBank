context("Metadata functions")
test_that("Time and date can be extracted", {
  filename <- 'inst/mbank_X24932_6-19-2018_744.nex'
  expect_equal('2018-06-19 07:44:41', NexusTime(filename, 'string'))
})
