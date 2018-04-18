#' Convert a display url to a profile RDF node 
#'
#' \code{profilesrnsexplorer} Uses rdflib and httr to fetch data
#' from ProfilesRNS site.  This converts the display url (which 
#' displays HTML) into the corresponding URL containing the RDF
#' for that page.
#'
#' @param u A display url which will be converted to an RDF url
#' @return a url
#'
#' @import stringr 
#' @importFrom stringr str_detect str_c str_split
#' @importFrom dplyr last 
#' @import tidyverse
#'
#' @export
#' @seealso \code{\link(rdflib)}
display_url_to_rdf_url <- function(u) {

  if(str_detect(u, ".rdf$") && str_detect(u, "profile")) {
     message(str_c("   already rdf url: ", u))
     return(u)
  }
   
  parts <- str_split(u, "/") %>% unlist
  nodeid <- last(parts)
  
  #if there's already a rdf on it
  url <- str_c(u, "/", nodeid, ".rdf")
  url <- str_replace(url, "display", "profile")
}


#' Fetches RDF and returns the content 
#'
#' @param url A url to fetch
#' @return text content 
#'
#' @import httr
#' @import stringr
#'
#' @export
#' @seealso \code{\link(httr)} 
fetch_rdf_url <- function(url) {
  message(str_c("FETCHING: ", url))
  response <- GET(url)
  if(response$status_code != 200) {
    warning(str_c(response$status_code, " Failed to fetch: ", url))
    return(NULL)
  }
  content(response, "text")
}


#' Fetches RDF and returns the content (optionally saving)
#'
#' @param url A url to fetch
#' @param tmpfile If specified, the content will be stored in this file 
#' @return text content 
#'
#' @import tidyverse
#' @import dplyr
#' @import stringr
#' @import httr 
#' @import rdflib
#'
#' @export
#' @seealso \code{\link(httr)} 
fetch_and_parse <- function(url, tmpfile = NULL) {

   content <- fetch_rdf_url(url)
   if(is.null(content)) {
     return(NULL)
   }
   if(!is.null(tmpfile)) {
     message(str_c("    saving results to: ", tmpfile))
     saveRDS(content, tmpfile)
   }

   content %>%
       rdf_parse(format = "rdfxml",
                 base = "https://profiles.healthsciencessc.org")
}

#' Fetches articles details for each article URL 
#'
#' \code{profilesrnsexplorer} Uses rdflib and httr to fetch data
#' from ProfilesRNS site.  This converts the display url (which 
#' displays HTML) into the corresponding URL containing the RDF
#' for that page.
#'
#' @param u A display url which will be converted to an RDF url
#' @return a url
#' 
#' @import stringr
#' @import fs
#' @import purrr
#'
#' @export
#' @seealso \code{\link(rdflib)}
#' could use type of article instead of pmid
fetch_articles <- function(alist, identifier = "person",
                           outdir = "fetched_rdf_articles",
                           save_per_article = FALSE) {

  if(!is.null(outdir)) {
    # make sure the directory exists
    fs::dir_create(outdir)
  }

  rdf_list <- map2(1:length(alist), alist,  function(i, display_url) {
    url <- display_url_to_rdf_url(display_url)

    save_fname <- NULL
    if(save_per_article) {
      save_fname <- fs::path(outdir, str_c(identifier,
                                           "_article_", i, ".rdf"))
    }
    fetch_and_parse(url, save_fname)
  })

  all_rdf <- do.call(c, rdf_list)

  ttl_fname <- str_c(identifier, "_all_articles.ttl")
  rdf_serialize(all_rdf, doc = fs::path(outdir, ttl_fname),
                format = "turtle")
  all_rdf
}


#' Fetch main page RDF and linked article pages
#'
#' \code{profilesrnsexplorer} Uses rdflib and httr to fetch data
#' from ProfilesRNS site.  Fetches a main profile page and
#' follows links to extract article details.
#'
#' @param outdir A directory where temporary and processed files will be saved
#' @param identified An identifier used to identified the person, such as John_Doe
#' @param initial_rdf_url  A initial display url which will be converted to an RDF url
#' @param rdf_file Name of downloaded RDF file
#'
#' @import stringr 
#' @import tidyverse
#' @import rdflib 
#' @import readr
#'
#' @export
#' @seealso \code{\link(rdflib)}
fetch_person_and_articles <- function(outdir, identifier, initial_rdf_url, rdf_file) {

  rdf_url <- display_url_to_rdf_url(initial_rdf_url)

  message(str_c("fetch_person_and_articles: ", identifier))

  pdata <- fetch_rdf_url(rdf_url)
  generated_file <- fs::path(outdir, rdf_file)
  write_file(pdata, generated_file)

  ff <- read_lines(generated_file)
  z <- rdf_parse(ff, format="rdfxml", 
                base = "https://profiles.healthsciencessc.org")

  sparql <-
'PREFIX bibo: <http://purl.org/ontology/bibo/>
PREFIX prns: <http://profiles.catalyst.harvard.edu/ontology/prns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

    SELECT ?a ?pmid ?authorList ?pubName ?pubDate ?title
    WHERE { ?a bibo:pmid ?pmid .
            ?a prns:hasAuthorList ?authorList .
            ?a prns:hasPublicationVenue ?pubName .
            ?a prns:publicationDate ?pubDate.
            ?a rdfs:label ?title
    }'

 results <- z %>% rdf_query(sparql)
 if(nrow(results) > 0) {
    message(str_c("Saving results: ", nrow(results), " for ", identifier ))
    write_csv(results, fs::path(outdir, str_c("articles_query_results_", identifier, ".csv")))

 articles_rdf <- fetch_articles(results$a, identifier = identifier, outdir)
  } else {
    message(str_c("NO articles for ", identifier))
  }
}
