context("GitHub link functions")
test_that("GitHub links work", {
  LinkWorks <- function(url) attr(curlGetHeaders(url), 'status') == 200
  options('GitHubUser'='ms609')
  expect_true(LinkWorks(substr(GitHubPages(repo='hyoliths'), 3, 27)))
  expect_true(LinkWorks(substr(GitLink(repo='hyoliths'), 3, 27)))
  expect_true(LinkWorks(
    substr(GitLink('R/github.R', repo='MorphoBank'), 65, 132)
    ))


})
