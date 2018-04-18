#' Generate script to fetch RDF for persons without data 
#'
#' @return a data frame containing data to be fetched 
#'
#' @import tidyverse
#' @import dplyr
#' @importFrom dplyr filter
#'
#' @export
identify_missing <- function() {
  data(pilot_grant_status)
  data(author_pubs)

  people_with_publications <- author_pubs %>% 
    distinct(personid) %>% 
    pull(personid)

  df <- pilot_grant_status %>% 
    filter(!personid %in% people_with_publications) %>%
    generate_fetch_urls() %>%
    generate_fetch_script("fetch_missing")
}


#' Generate script to fetch RDF for all non applicants 
#'
#' \code{profilesrnsexplorer} Uses rdflib and httr to fetch data
#' from ProfilesRNS site.  This converts the display url (which 
#' displays HTML) into the corresponding URL containing the RDF
#' for that page.
#'
#' @return a data frame containing data to be fetched 
#'
#' @import tidyverse
#' @import dplyr
#' @importFrom dplyr filter
#'
#' @export
identify_nonapplicant <- function() {
  df <- pilot_grant_status %>% 
    filter(!grant_status %in% c("funded", "unfunded"))
    generate_fetch_urls() %>%
    generate_fetch_script("fetch_nonapplicant")
}

#' Generate script to fetch RDF for specified persons 
#'
#' \code{profilesrnsexplorer} Uses rdflib and httr to fetch data
#' from ProfilesRNS site.  This converts the display url (which 
#' displays HTML) into the corresponding URL containing the RDF
#' for that page.
#'
#' @param wanted_df  A data frame containing persons 
#' @return a data frame containing data to be fetched 
#'
#' @import tidyverse
#' @import dplyr
#' @importFrom dplyr filter
#'
#' @export
#' @seealso \code{\link(rdflib)}



#' Generate script to fetch RDF for applicants (funded and unfunded)
#'
#' \code{profilesrnsexplorer} Uses rdflib and httr to fetch data
#' from ProfilesRNS site.  This converts the display url (which 
#' displays HTML) into the corresponding URL containing the RDF
#' for that page.
#'
#' @return a data frame containing data to be fetched 
#'
#' @import tidyverse
#' @import dplyr
#' @importFrom dplyr filter
#'
#' @export
#' @seealso \code{\link(rdflib)}
identify_applicant <- function() {
  data(pilot_grant_status)

  df <- pilot_grant_status %>% 
    filter(grant_status %in% c("funded", "unfunded"))
    generate_fetch_urls() %>%
    generate_fetch_script("fetch_applicant")
}

#' Generate script to fetch RDF for specified persons 
#'
#' \code{profilesrnsexplorer} Uses rdflib and httr to fetch data
#' from ProfilesRNS site.  This converts the display url (which 
#' displays HTML) into the corresponding URL containing the RDF
#' for that page.
#'
#' @param wanted_df  A data frame containing persons 
#' @return a data frame containing data to be fetched 
#'
#' @import tidyverse
#' @import dplyr
#' @importFrom dplyr filter
#'
#' @export
#' @seealso \code{\link(rdflib)}
generate_fetch_urls <- function(df) {
  p <- "https://profiles.healthsciencessc.org/profile/"
  df %>%
    mutate(rdf_url = str_c(p, nodeid,"/",nodeid,".rdf"),
          rdf_file = str_c(nodeid, ".rdf"),
          identifier = str_replace_all(displayname, " |\\.", "_")) %>%
    select(identifier, rdf_url, rdf_file)
}



#' Generate script to fetch RDF
#'
#' @param status A data frame 
#' @return status_prefix A prefix used in the generated filename
#'
#' @import stringr 
#' @importFrom stringr str_detect str_c str_split
#' @importFrom dplyr last 
#' @import tidyverse
#' @importFrom lubridate ymd_hms
#'
#' @export
#' @seealso \code{\link(rdflib)}
generate_fetch_script <- function(status, script_prefix) {
  next_one <- status %>%
    mutate(cmd = str_c("fetch_person_and_articles(outdir, ",
                     '"' , identifier, '"', " , ",
                     '"' , rdf_url, '"', " , ",
                     '"' , rdf_file, '"',
                     ")"))

  s <- lubridate::ymd_hms(lubridate::now())
  tm <- str_replace_all(str_c(s),":|-","") %>%
    str_replace_all(" ","_")

  script_fname <- str_c(script_prefix, tm, ".R")
  startcmds <- c("outdir <- 'inst/generated'",
               " ",
               "library(tidyverse)",
               "library(fs)",
               "library(profilesrnsexplorer)",
               "fs::dir_create(outdir)",
               " ")

  allcmds <- append(startcmds, next_one$cmd)
  write_lines(allcmds, script_fname)

  message(str_c("Generated script: ", script_fname))
}
