#' Nexus Time
#'
#' Report time that a Nexus file was exported from MorphoBank
#'
#' @template filenameParam
#' @param format Format of output; see below
#'
#' @return The time that `filename` was exported from MorphoBank, according
#' to its internal comment, either by specifying the year, month, day and time as a double
#' (`format = 'double'`) or as a string in the format `YYYY-MM-DD hh:mm:ss` (otherwise)
#'
#' @author Martin R. Smith
#' @export
NexusTime <- function (filename, format='double') {
  FILE <- file(filename)
  open(FILE)
  comment <- readLines(FILE, n=3)[3]
  close(FILE)
  comment <- sub("\\-(\\d) ", "-0\\1 ", comment) # MorphoBank does odd things with times!
  if (format == 'double') {
    as.double(sub('.*(\\d{4})\\-(\\d{2})\\-(\\d{2})\\s(\\d{2})\\.(\\d{2}\\.\\d{2}).*', "\\1\\2\\3\\4\\5", comment, perl=TRUE))
  } else {
    sub('.*(\\d{4}\\-\\d{2}\\-\\d{2}\\s\\d{2})\\.(\\d{2})\\.(\\d{2}).*', "\\1:\\2:\\3", comment, perl=TRUE)
  }
}

#' @describeIn NexusTime TNT files
#' @export
TNTTime <- NexusTime

#' MorphoBank exports
#'
#' @param path Path to search
#' @param pattern Character string specifying regexp pattern, if files have
#' been renamed after downloading from MorphoBank
#' @param \dots Additional parameters to [list.files]
#'
#' @return A character vector listing the full filenames of all Nexus files
#' in the specified directory beginning "mbank_", and thus presumed exported
#' from MorphoBank.
#' @export
#' @author Martin R. Smith
MorphoBankExports <- function (path='.', pattern='mbank_.*\\.nex', ...) {
  list.files(path, pattern=pattern, full.names=TRUE, ...)
}

#' @describeIn MorphoBankExports Lists files exported in TNT format
#' @export
MorphoBankTNTs <- function (path='.', pattern='mbank_.*\\.tnt', ...) {
  MorphoBankExports(path, pattern, ...)
}

#' Most recent Nexus file
#'
#' Reports which of a list of nexus files was exported from MorphoBank most recently
#'
#' @param filenames Character vector listing locations of nexus files exported from MorphoBank
#'
#' @return character of length one, specifying the most recent of the files provided
#' @author Martin R. Smith
#' @export
MostRecentNexus <- function (filenames = MorphoBankExports()) {
  filenames[which.max(vapply(filenames, NexusTime, double(1)))]
}

#' @describeIn MostRecentNexus Reports which of a list of TNT matrices was
#' exported from MorphoBank most recently
#' @export
MostRecentTNT <- function(filenames=MorphoBankTNTs()) {
  filenames[which.max(vapply(filenames, TNTTime, double(1)))]
}
