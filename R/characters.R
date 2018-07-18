#' Read Notes
#'
#' @template filenameParam
#'
#' @return TBC
#'
#' @author Martin R. Smith
#' @export
ReadNotes <- function (filename) {
  taxon.pattern <- "^\\s+[\"']?([^;]*?)[\"']?\\s*$"
  charNote.pattern <- "^\\s+TEXT\\s+CHARACTER=(\\d+)\\s+TEXT='(.*)';\\s*$"
  stateNote.pattern <- "^\\s+TEXT\\s+TAXON=(\\d+)\\s+CHARACTER=(\\d+)\\s+TEXT='(.*)';\\s*$"

  lines <- enc2utf8(readLines(filename))
  upperLines <- toupper(lines)

  notesStart <- which(upperLines == "BEGIN NOTES;")
  endBlocks <- which(upperLines == "ENDBLOCK;")
  taxlabels <- which(trimws(upperLines) == "TAXLABELS")
  semicolons <- which(trimws(upperLines) == ";")

  if (length(notesStart) == 0) {
    return(list("NOTES block not found in Nexus file."))
  } else if (length(taxlabels) == 0) {
    return(list("TAXLABELS not found in Nexus file."))
  } else if (length(notesStart) > 1) {
    return(list("Multiple NOTES blocks found in Nexus file."))
  } else if (length(taxlabels) > 1) {
    return(list("Multiple TAXLABELS found in Nexus file."))
  } else {
    taxaEnd <- semicolons[semicolons > taxlabels][1] - 1L
    taxaLines <- lines[(taxlabels + 1):taxaEnd]
    taxon.matches <- grepl(taxon.pattern, taxaLines, perl=TRUE)
    taxa <- gsub(taxon.pattern, "\\1", taxaLines[taxon.matches], perl=TRUE)
    taxa <- gsub(' ', '_', taxa, fixed=TRUE)

    notesEnd <- endBlocks[endBlocks > notesStart][1] - 1L
    notesLines <- lines[(notesStart + 1):notesEnd]
    charNote.matches <- grepl(charNote.pattern, notesLines, perl=TRUE)
    charNotes <- gsub(charNote.pattern, "\\2",
                      notesLines[charNote.matches], perl=TRUE)
    charNotes <- EndSentence(MorphoBankDecode(charNotes))
    charNumbers <- gsub(charNote.pattern, "\\1",
                        notesLines[charNote.matches], perl=TRUE)

    stateNote.matches <- grepl(stateNote.pattern, notesLines, perl=TRUE)
    stateNotes <- gsub(stateNote.pattern, "\\3",
                       notesLines[stateNote.matches], perl=TRUE)
    stateNotes <- EndSentence(MorphoBankDecode(stateNotes))
    stateTaxon <- gsub(stateNote.pattern, "\\1",
                       notesLines[stateNote.matches], perl=TRUE)
    stateChar  <- gsub(stateNote.pattern, "\\2",
                       notesLines[stateNote.matches], perl=TRUE)

    seqAlongNotes <- seq_len(max(as.integer(c(stateChar, charNumbers))))
    charNotes <- lapply(seqAlongNotes, function (i) {
      ret <- list(
        charNotes[charNumbers == i],
        stateNotes[stateChar == i])
      names(ret[[2]]) <- taxa[as.integer(stateTaxon[stateChar == i])]

      # Return:
      ret
    })
    names(charNotes) <- seqAlongNotes

    # Return:
    charNotes
  }
}

#' Is Character Transformational
#'
#' @template statesParam
#'
#' @return A logical specifying whether each character is transformational
#' @export
#'
IsTransformational <- function (states) {
  gsub("^'(.*)'$", "\\1", states)[1] == ""
}

#' @describeIn IsTransformational The inverse of IsTransformational
IsNeomorphic <- function (states) !IsTransformational(states)

