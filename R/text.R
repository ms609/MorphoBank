#' End Sentence
#'
#' @param string Input string
#'
#' @return `string`, punctuated with a final full stop (period).`
#'
#' @examples {
#' EndSentence("Hello World")
#' # "Hello World."
#' }
#'
#' @author Martin R. Smith
#' @export
EndSentence <- function (string) {
  ret <- gsub("\\s*\\.?\\s*\\.$", ".", paste0(string, '.'), perl=TRUE)
  ret <- gsub("(\\.[\"'])\\.$", "\\1", ret, perl=TRUE)
  ret <- gsub("([!\\?])\\.$", "\\1", ret, perl=TRUE)
  ret
}

PrintStates <- function (states) {
  states <- gsub("^'(.*)'$", "\\1", states)
  tokens <- seq_along(states) - 1L
  if (states[1] == "") {
    states <- states[-1]
    tokens <- tokens[-1]
    transformational <- TRUE
  } else {
    transformational <- FALSE
  }
  cat(paste0(" > ", tokens, ": ", states, "  \n"))
  cat("> ", if (transformational) "Transformational" else "Neomorphic", "character.  \n>\n")
}

PrintNaughtyInapplicables <- function (states) {
  if (any(states == '-'))
    cat("  \n Oh dear! <mark>**You included the inapplicable token in a neomorphic character!**</mark>",
        "  \n That's really very naughty, as @Brazeau2018 will tell you.",
        "  \n Unless you are very sure that you understand the consequences, ",
        "you should mark the character as Transformational by setting State 0 to",
        "`[Transformational character]`, or re-code: \n\n - ",
        paste(names(states[states == '-']), collapse="  \n - "))
}

#' Link to MorphoBank project
#'
#' @param id Integer corresponding to the project's MorphoBank identifier
#'
#' @return Text providing a link to the project, or if the project is not yet
#' publically available, a note instructing password holders how to log in.
#' @export
MorphoLink <- function (id, linkText=paste('project', id)) {
  # https://morphobank.org/permalink/?P not currently working, so:
  taxaRoot <- "https://morphobank.org/index.php/Projects/Taxa/project_id/"
  linkStatus <- attr(curlGetHeaders(paste0(taxaRoot, id)), 'status')
  if (linkStatus == 200) {
    paste0('[', linkText, '](https://morphobank.org/permalink/?P', id, ')')
  } else {
    paste0("<mark>[This dataset has not yet been released to the public.",
    "If you have been given a password, access the dataset by",
    "[logging in to MorphoBank](https://morphobank.org/index.php/LoginReg/form)",
    "using the project ID '", id,
    "' as your e-mail address, and the password you have been given.]</mark>")
  }
}
