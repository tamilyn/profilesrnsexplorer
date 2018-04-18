#' Run SPARL query the query is explicitly specified or defined in a file
#'
#' Runs a SPARQL query on the datastore.  Either the SPARQL query
#' must be specified, or a name represeting a query in the 
#' query library.
#'
#' @param ds RDF data store
#' @param sparql A SPARQL query (optional)
#' @param qname SPARQL query name (optional)
#' @return A data frame with query results 
#'
#' @import tidyverse
#'
#' @export
#' @seealso \code{\link(rdflib)}
query <- function(ds, sparql = NULL, qname = NULL) {
 
}

#' Load SPARL query definition from file
#'
#' @param filename The name of a file containing a SPARQL query
#' @return A string containing the SPARQL query
#'
#' @import tidyverse
#' @import readr`
#' @import fs
#'
#' @export
load_sparql <- function(filename) {

   sparql_dir <- "inst/sparql"
   fname <- fs::path(sparql_dir, filename)
   read_lines(fname) %>% str_c(collapse = " \n ")
}

#' Load SPARL query definition from file and run it
#'
#' @param r The RDF data store 
#' @param s The SPARLQ statement or the name of a file containing a SPARQL query
#' @return Data frame containing the results of running the SPARQL query
#'
#' @import rdflib
#' @import stringr
#'
#' @export
fsparql <- function(r, s) {
   if(str_detect(s, ".sparql$")) {
      sparql <- load_sparql(s)
   } else if (str_detect(s, "SELECT")) {
     sparql <- s
   } else {
      filename <- str_c(s, ".sparql")
      sparql <- load_sparql(filename)
   }
   r %>% rdf_query(sparql)
}
