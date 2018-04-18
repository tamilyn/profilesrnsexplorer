library(tidyverse)
library(profilesrnsexplorer)
library(fs)

files <- fs::dir_ls("inst/generated", glob="*.rdf|*.ttl")
print(length(files))
rdf <- rdf_from_files(files, "generated_whenever")
