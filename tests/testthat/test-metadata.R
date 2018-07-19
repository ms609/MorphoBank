context("Metadata functions")
test_that("Time and date can be extracted", {
  filename <- MostRecentNexus()
  expect_equal('2018-06-19 07:44:41', NexusTime(filename, 'string'))
  filename <- MostRecentTNT()
  expect_equal('2018-07-18 08:08:21', TNTTime(filename, 'string'))
})
