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

#' Decode MorphoBank text
#'
#' Converts strings from MorphoBank notes into a format compatible with Latex / bookdown
#'
#' @param string String to process
#'
#' @return A string with new lines and punctuation reformatted
#' @export
#' @author Martin R. Smith
#'
MorphoBankDecode <- function (string) {
  string <- gsub("^n", "  \n", string, fixed=TRUE)
  string <- gsub("''", "'", string, fixed=TRUE)
  string <- gsub(" - ", " -- ", string, fixed=TRUE)
  string <- gsub("(\\d)\\-(\\d)", "\\1--\\2", string, perl=TRUE)
  string <- gsub("(\\d) ?um\\b", "\\1 \u{03BC}m", string, perl=TRUE)
  string <- gsub(" [recoded as neomorphic]", " Inapplicable tokens in this neomorphic character have been replaced with the absent token, following @Brazeau2018", string, fixed=TRUE)

  # Return:
  string
}


#' Print States
#'
#' Prints a summary of the character states for a given character
#'
#' @param states A character vector stating the labels for each character state,
#' as given by `attr(ReadCharacters(filename), 'state.labels')`
#'
#' @return Text listing the states, ready for inclusion in a markdown source document
#'
#' @author Martin R. Smith
#' @export
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
#' @param id Integer corresponding to the project's MorphoBank identifier.
#' A global default can be set using `options(MorphoBankProject=1234)`.
#' @param linkText Text to appear in link, once project is live
#'
#' @return Text providing a link to the project, or if the project is not yet
#' publically available, a note instructing password holders how to log in.
#' @export
MorphoLink <- function (id=getOption('MorphoBankProject'), linkText=paste('project', id)) {
  # https://morphobank.org/permalink/?P not currently working, so:
  taxaRoot <- "http://morphobank.org/index.php/Projects/Taxa/project_id/"
  linkStatus <- attr(curlGetHeaders(paste0(taxaRoot, id), verify = FALSE), 'status')
  if (linkStatus == 200) {
    paste0('[', linkText, '](https://morphobank.org/permalink/?P', id, ')')
  } else {
    paste("<mark>[This dataset has not yet been released to the public.",
    "If you have been given a password, access the dataset by",
    "[logging in to MorphoBank](https://morphobank.org/index.php/LoginReg/form)",
    "using the project ID `", id,
    "` as your e-mail address, and the password you have been given.]</mark>")
  }
}
