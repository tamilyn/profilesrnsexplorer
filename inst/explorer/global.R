library(tidyverse)
library(shiny)
library(shinyjs)
library(plotly)
library(DT)
library(lubridate)
library(profilesrnsexplorer)

data(pilot_grant_status)
data(author)
data(author_pubs)
data(coauthor_data)
data(info_resource_author)

print("loading authors")
print(str_c("loading authors: ", nrow(author)))
print(colnames(author))

authors <- author %>%
  left_join(pilot_grant_status, by=c("personid"="personid"))

author_names <- authors %>% pull(fullname) %>% unique

#would be good to have this as the value is the pmid
#eg a names list whwere the name is the personid and the value is the name
author_names_choices <- append("All", author_names)

grant_status_choices <- c("All", "unfunded", "funded", "applicant", "non applicant")

max_num <- 500

