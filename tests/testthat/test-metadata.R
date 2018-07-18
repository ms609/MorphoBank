context("Metadata functions")
test_that("Time and date can be extracted", {
  filename <- MostRecentNexus(MorphoBankExports(path='../../inst'))
  expect_equal('2018-06-19 07:44:41', NexusTime(filename, 'string'))
})
