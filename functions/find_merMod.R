find_merMod <- function(obj) {
  if (inherits(obj, "merMod")) return(obj)
  
  if (is.list(obj)) {
    for (el in obj) {
      res <- find_merMod(el)
      if (!is.null(res)) return(res)
    }
  }
  
  NULL
}
