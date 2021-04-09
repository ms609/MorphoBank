context("GitHub link functions")
test_that("GitHub links work", {
  LinkWorks <- function(url) attr(curlGetHeaders(url, verify = FALSE), 'status') == 200
  options('GitHubUser' = 'ms609',GitHubRepo = 'MorphoBank')
  expect_true(LinkWorks(substr(GitHubPages(repo='hyoliths'), 3, 27)))
  expect_true(LinkWorks(substr(GitLink('R/github.R'), 65, 132)))
  expect_true(LinkWorks(substr(GitLink('R/github.R', raw=FALSE), 55, 112)))
})
