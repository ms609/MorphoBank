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
#' @template statesParam
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

#' Print State Notes
#'
#' @param notes Named character vector, with names corresponding to terminal taxa,
#' and values corresponding to character coding notes for each taxon in turn.
#' This matches the format given by `ReadNotes(filename)[[character_number]][[2]]`.`
#' @param taxaNames Character vector specifying names of taxa.
#' @param taxaItalic Character vector corresponding to taxaNames, but using
#' `_`underscores`_` to denote italic text where required.
#' @param charId Character or integer identifying the character, for use in the
#' resultant div's ID attribute.
#' @param Format Function that adds italics or other formatting to printed notes.
#'
#' @return Text listing the character coding notes for each taxon, ready for inclusion in a markdown source document
#'
#' @author Martin R. Smith
#' @export
#'
#' @examples {
#' \dontrun{
#'   notes <- ReadNotes(filename)
#'   character_1_notes <- notes[[1]]
#'   cat("Character 1")
#'
#'   PrintStateNotes(character_1_notes[[2]])
#'   }
#' }
PrintStateNotes <- function (notes, taxaNames=NULL, taxaItalic=taxaNames,
                             charId=character(0), Format=I) {
  PrintThisNote <- function (note) {
    afflictedTaxa <- names(note)[note]
    if (is.null(taxaNames)) {
      taxaNames <- taxaItalic <- afflictedTaxa
    }
    master <- afflictedTaxa[1]
    cat(paste0("<div class='state-note' id='", master, "-coding-", charId, "'>",
               paste(taxaItalic[taxaNames %in% afflictedTaxa], collapse=', '),
               ": ", Format(notes[master]), "</div>  \n  \n"))
  }

  DuplicateOf <- function (x) {
    duplicates <- duplicated(x)
    masters <- x[!duplicates]
    vapply(masters, function (d) x == d, logical(length(x)))
  }

  if (length(notes) > 0) {
    cat('  \n<div class="state-notes">')
  }
  if (length(notes) == 1) {
    onlyOne <- TRUE
    names(onlyOne) <- names(notes)
    PrintThisNote(onlyOne)
  } else {
    notes <- notes[order(names(notes))]
    duplicates <- DuplicateOf(toupper(notes))
    apply(duplicates, 2, PrintThisNote)
  }
  if (length(notes) > 0) {
    cat('</div>  \n  \n')
  }
}

#' Print Naughty Inapplicables
#'
#' Prints a warning message when an inapplicable token is found in a neomorphic character.
#'
#' @template statesParam
#'
#' @return A warning message for inclusion in a markdown documnet.
#'
#' @author Martin R. Smith
#' @export
PrintNaughtyInapplicables <- function (states) {
  if (any(states == '-'))
    cat("  \n Oh dear! <mark>**You included the inapplicable token in a neomorphic character!**</mark>",
        "  \n That's unlikely to be a good idea:",
        "  see Brazeau, Smith & Guillerme (2018, Systematic Biology).",
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
