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
