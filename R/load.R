#' Combine a list of files into RDF datastore
#'
#' Processes a list of files which are either rdfxml (stored in .rdf) files
#' or turtle files (.ttl).    Each file is loaded and combined.
#' The rdf is serialized to the generated filename and also returned.
#'
#' @param files A list of rdf files (.ttl or .rdf
#' @return RDF datastore 
#' @export
#'
#' @import tidyverse
#' @import stringr
#' @import rdflib
#' @seealso \code{\link(rdflib)}
rdf_from_files <- function(files, generated_filename = "combined.ttl") {

  rdf_list <- map(files, function(fname) {
     format <- "turtle"
     if(str_detect(fname, ".rdf$")) {
        format <- "rdfxml"
     }
     ff <- read_lines(fname)
     rdf_parse(ff, format = format, 
            base = "https://profiles.healthsciencessc.org")
  })

  all_rdf <- do.call(c, rdf_list)
  rdf_serialize(all_rdf, doc = generated_filename, format = "turtle")

  all_rdf
}
