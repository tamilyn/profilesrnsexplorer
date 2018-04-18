#
extract_rdf_to_dfs <- function() {
  require(profilesrnsexplorer)
  require(tidyverse)
  require(rdflib)

  rdf <- load_combined_rdf()
  make_data(rdf)
}

#
make_data <- function(rdf) {
  require(profilesrnsexplorer)
  require(tidyverse)
  require(rdflib)

  author_pubs <- fsparql(rdf, "author_pubs")
  #it's the informationresource we want, not the authorship
  info_resource_author <- fsparql(rdf, "linked_author1")

  coauthor_links <- make_coauthor_links(author_pubs)

  coauthor_data <- coauthor_links %>%
    group_by(personid1, personid) %>%
    summarise(weight=n())

  usethis::use_data(coauthor_data, overwrite = TRUE)

  pubs <- fsparql(rdf, "pubs")

  linked <- fsparql(rdf, "linked_author")

  #author, lastName, firstName, etc
  #fullname, author, authorship, authorRank
  author_authorship <- fsparql(rdf, "author_authorship")
  author <- fsparql(rdf, "author")
  mesh <- fsparql(rdf, "mesh")

  usethis::use_data(coauthor_links, overwrite = TRUE)
  usethis::use_data(author_pubs, overwrite = TRUE)
  usethis::use_data(pubs, overwrite = TRUE)
  usethis::use_data(linked, overwrite = TRUE)
  usethis::use_data(info_resource_author, overwrite = TRUE)
  usethis::use_data(mesh, overwrite = TRUE)
  usethis::use_data(author, overwrite = TRUE)

  #bb <- linked %>% left_join(pubs, by=c("authorship"="authorship"))
}

#
load_combined_rdf <- function() {
  require(profilesrnsexplorer)
  require(tidyverse)
  require(rdflib)

  fname = "inst/combined.ttl"
  rdf <- read_lines(fname) %>% rdf_parse(format = "turtle")
  rdf
}

make_person_coauthor_links <- function(author_pubs, p) {
  article_ids <- author_pubs %>%
    filter(personid == p) %>%
    distinct(infoResource) %>%
    pull(infoResource)

  print(str_c("Person: ", p, " num articles: ", length(article_ids)))
  p_pubs <- author_pubs %>%
    filter(infoResource %in% article_ids) %>%
    mutate(personid1 = p)
}


make_coauthor_links <- function(author_pubs) {
   articles <- author_pubs %>% distinct(infoResource)

   persons <- author_pubs %>% distinct(personid) %>% pull(personid)
   zz <- map(persons,
             ~ make_person_coauthor_links(author_pubs, .))

  coauthor_links <- zz %>% bind_rows() %>%
    filter(personid1 != personid) %>%
    mutate(from = ifelse(personid1 < personid, personid1, personid),
           to = ifelse(personid1 < personid, personid, personid1),
           pmid = str_c(pmid)) %>%
    select(from, to, pmid, pubdate, infoResource) %>%
    distinct()
}
