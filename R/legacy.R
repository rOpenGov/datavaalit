# Functions to remove - not needed any more?

.pad.zeros <- function(x, target.length=4) {
  x <- as.character(x)
  char.diff <- target.length - nchar(x)
  if (char.diff < 0) {
    warning("The length of a padded string cannot be smaller than the original")
    return(x)
  }
  return(paste(paste(rep("0", char.diff), collapse=""), x, sep=""))
}