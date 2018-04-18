
#
# Combine funded and unfunded information with ProfilesRNS data to
# generate profilesrns_with_grant
#
# profilerns data
identify_netid_or_email <- function(profilesrns_people, df) {
  require(tidyverse)
  require(fs)

  xf <- df %>%
    select(-lastname, -firstname) %>%
    left_join(profilesrns_people, by=c("netid"="internalusername")) %>%
    filter(!is.na(personid)) %>%
    mutate(internalusername = netid)

  yf <- df %>%
    select(-lastname, -firstname) %>%
    left_join(profilesrns_people, by=c("netid"="emailaddr")) %>%
    filter(!is.na(personid)) %>%
    mutate(emailaddr = netid)

  zf <- bind_rows(xf,yf) %>% distinct()
}

build_profilesrns_with_grant <- function() {
  require(tidyverse)
  require(fs)
  require(readxl)

  profilesrns_people <- read_csv("inst/extdata/person_nodes_query.csv") %>%
    mutate(personid = as.integer(personid),
           nodeid = as.integer(nodeid))

# unfunded
  dir <- "inst/extdata"
  dfile2 <- fs::path(dir, "pilot_pis_not_funded-dr4.16.18.xlsx")
  unfunded <- readxl::read_excel(dfile2) %>%
  select(email, personid = PersonID, firstname = PI_first_name, lastname = PI_last_name,
         bs_or_c = `Basic Scientst (BS) or Clinician`) %>%
  mutate(personid = as.integer(personid))

# funded
  #"SCTR Pilot Project PIs Master List - 2.20.18drPH.xlsx")
  dfile <- fs::path(dir, "SCTRPilotProject_4.16.18drPH.xlsx")

  excel_colnames <-  c("record_id",  "lastname", "firstname",
    "degree", "rank_at_funding", "institution", "college",
    "dept", "division","specialty", "netid", "grant_start",
    "remarks", "bs_or_c")

  excel_coltypes <- c("numeric", "text", "text", "text", "text",
    "text", "text", "text", "text", "text", "text", "date", "text",
    "text" )

  hdrlines <- 2
  funded_applicants <- readxl::read_excel(dfile,
        col_names = excel_colnames,
        col_types = excel_coltypes,
        skip  = hdrlines)


  new_one <- identify_netid_or_email(profilesrns_people,
                                   funded_applicants) %>%
    mutate(grant_status = "funded")

  new_two <- unfunded %>%
    select(-firstname, -lastname) %>%
    filter(!is.na(personid)) %>%
    left_join(profilesrns_people,  by=c("personid"="personid"))

  inactive_unfunded <- new_two %>%
    filter(is.na(nodeid))

  new_three <- new_two %>%
    filter(!is.na(nodeid), !is.na(personid)) %>%
    mutate(grant_status = "unfunded")

  both <- bind_rows(new_one, new_three) %>%
    select(personid, netid, internalusername, emailaddr, displayname, nodeid, grant_status,
         grant_start, everything()) %>%
  select(-netid, -email, -record_id) %>%
  arrange(lastname)

  person_fields <- read_csv(fs::path(dir, "person_fields.csv")) %>%
    select(internalusername,personid,isactive)

  active <- person_fields %>%
    filter(isactive == "true") %>% pull(personid)

  active_persons <- person_fields %>%
    filter(isactive == "true")

  both_active <- both %>%
    filter(personid %in% active)

  others <- profilesrns_people %>%
    filter(personid %in% active) %>%
    filter(!personid %in% both_active$personid) %>%
    mutate(grant_status = "non_applicant")

  profilesrns_with_grant <- bind_rows(both_active, others)
  write_csv(profilesrns_with_grant, fs::path(dir, "profilesrns_with_grant.csv"))
  profilesrns_with_grant
}

pilot_grant_status <- build_profilesrns_with_grant()
usethis::use_data(pilot_grant_status)
