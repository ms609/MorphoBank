context("Character functions")
filename <- MostRecentNexus(MorphoBankExports(path='../../inst'))

test_that("Character data is read from file", {
  my_data <- TreeSearch::ReadAsPhyDat(filename)
  expect_equal(54, length(names(my_data)))
  expect_equal(225, sum(attr(my_data, 'weight')))
})

test_that("Character descriptions are extracted", {
  my_chars <- TreeSearch::ReadCharacters(filename)
  states <- attr(my_chars, 'state.labels')
  expect_equal(129, sum(vapply(states, IsNeomorphic, logical(1))))
  expect_equal(225, ncol(my_chars))
  expect_equal(4975, sum(!my_chars %in% c('-', '?')))
})

test_that("Character notes are extracted", {
  char_notes <- ReadNotes(filename)
  expect_equal(225, length(char_notes))
})
