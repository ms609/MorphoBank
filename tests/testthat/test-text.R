context("Text functions")
test_that("MorphoLink generates correct links", {
  expect_equal('[project 977](https://morphobank.org/permalink/?P977)',
               MorphoLink(977))
  expect_equal('<mark>', substr(MorphoLink(97777777), 1, 6))
})

