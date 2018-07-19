#' GitHub Link
#'
#' Generates a link to a file on a specified GitHub repository.
#'
#' @param path Character string specifying path to file in specified GitHub repository
#' @param alt Character string specifying link text to display if
#'            knitr output is in HTML format.
#' @param raw Logical specifying whether to link to the raw file at
#'            raw.githubusercontent.com.
#'            If `FALSE`, links to the file at github.com.
#' @template gitHubParams
#'
#' @return A link to a file hosted on GitHub, formatted appropriately for HTML
#' or Latex output
#' @export
#'
#' @author Martin R. Smith
#' @importFrom knitr is_html_output
#' @examples {
#'   GitLink('ms609', 'MorphoBank')
#'   options(GitHubUser="ms609", GitHubRepo="MorphoBank")
#'   GitLink()
#' }
GitLink <- function (path='', alt=NULL, raw=TRUE, user=getOption('GitHubUser'),
                     repo=getOption('GitHubRepo')) {
  if (is.null(user)) warning("Specify GitHub Username with options(GitHubUser='USERNAME')")
  if (is.null(repo)) warning("Specify GitHub repository with options(GitHubRepo='REPONAME')")
  rawGit    <- paste0("https://raw.githubusercontent.com/", user, '/', repo, "/master/")
  gitHubUrl <- paste0("https://github.com/", user, '/', repo, "/tree/master/")
  paste0(" [",
         if (!is_html_output() || is.null(alt)) {
           paste0(gsub("https://", "", ifelse(raw, rawGit, gitHubUrl), fixed=TRUE), path)
         } else alt,
         "](", ifelse(raw, rawGit, gitHubUrl), path, ")")
}

#' @describeIn GitLink  A link to `user`.github.io/`repo`/
#' @importFrom knitr is_html_output
GitHubPages <- function (path='', alt=NULL, user=getOption('GitHubUser'), repo=getOption('GitHubRepo')) {
  if (is.null(user)) warning("Specify GitHub Username with options(GitHubUser='USERNAME')")
  if (is.null(repo)) warning("Specify GitHub repository with options(GitHubRepo='REPONAME')")
  gitPageUrl <- paste0("https://", user, '.github.io/', repo, '/', path)
  paste0(" [", ifelse (!is_html_output() || is.null(alt),
                       gsub("https://", "", gitPageUrl, fixed=TRUE), alt),
         "](", gitPageUrl, ")")
}
