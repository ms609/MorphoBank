context("Text functions")
test_that("MorphoLink generates correct links", {
  expect_equal('[project 977](https://morphobank.org/permalink/?P977)',
               MorphoLink(977))
  expect_equal('<mark>', substr(MorphoLink(97777777), 1, 6))
})

test_that("EndSentence works correctly", {
  expect_equal('Hi.', EndSentence('Hi'))
  expect_equal('Hi.', EndSentence('Hi.'))
  expect_equal('Hi?', EndSentence('Hi?'))
  expect_equal('Hi!', EndSentence('Hi!'))
})

test_that("MorphoBankDecode decodes", {
  expect_equal("' -- x  \n 1--2", MorphoBankDecode("'' - x^n 1-2"))
})

