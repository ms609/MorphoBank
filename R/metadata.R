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
  comment <- sub("\\-(\\d) ", "-0\\1 ", comment) # Morphobank do odd things with times!
  if (format == 'double') {
    as.double(sub('.*(\\d{4})\\-(\\d{2})\\-(\\d{2})\\s(\\d{2})\\.(\\d{2}\\.\\d{2}).*', "\\1\\2\\3\\4\\5", comment, perl=TRUE))
  } else {
    sub('.*(\\d{4}\\-\\d{2}\\-\\d{2}\\s\\d{2})\\.(\\d{2})\\.(\\d{2}).*', "\\1:\\2:\\3", comment, perl=TRUE)
  }
}

#' Most recent Nexus file
#'
#' Reports which of a list of nexus files was exported from MorphoBank most recently
#'
#' @param filenams Character vector listing locations of nexus files exported from MorphoBank
#'
#' @return character of length one, specifying the most recent of the files provided
#' @author Martin R. Smith
#' @export
MostRecentNexus <- function (filenames) {
  filenames[which.max(vapply(filenames, NexusTime, double(1)))]
}
