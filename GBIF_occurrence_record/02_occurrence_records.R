#############################
# get GBIF records

###############################
# get GBIF occurrence records
# June 2026

# Note that I am creating a new data_occ folder in this project marked by the year 

###########################################
# downloading data from GBIF


#####################################################
library(tidyverse)
library(rgbif)

##################################################
rm(list = ls())

####################################################
# custom functions
source("r_functions/f_occ_download_v3.r")
source("r_functions/functions_taxa.r")
#####################################################

# set up folder for downloads

gb_dir <- './data/data_occ2026/' 

# create this directory if it does not exist:
if (!dir.exists(gb_dir)) {
  dir.create(gb_dir, recursive = TRUE)
}

####################################################

# load dataset with GBIF taxa

exampledataset <- readr::read_csv('./data/exampledataset_GBIFtaxaJune2026.csv') # this was generated in 01_precleaning.R
exampledataset<-exampledataset[,!names(exampledataset)%in%c("species","specieskey")]# removes any columns which might have the same name
exampledataset$specieskey<-exampledataset$acceptedusagekey 
exampledataset$species<-exampledataset$genus_species
# the species key is the GBIF code for the species and is what we use to download the occurrence records

# this will be entered in the .Renviron file, so you won't need to enter them here
username <- "***" # **username of GBIF account
password <- "***" # **GBIF account's password
emailadd <- "***" # **GBIF account's email address


sp_key <- exampledataset %>%
  filter(!is.na(specieskey)&rank=="species") %>%
  select(species, specieskey) %>%
  distinct %>%
  arrange(species) 
# this removes any genus-level-only GBIF matches,any species-not-found, and any reps
# extracts species key and binomial name

###################
# downloading records for a bunch of species takes a while so we will just do one species

sp_key<-sp_key[sp_key$species=="Ips grandicollis",]

#  remove this shortcut if doing a real run
#######################

spp_per_chunk <- 150 # 250 seems a practical maximum for my laptop, if species list too long

# Split into chunks of n = spp_per_chunk species with their keys
sp_key <- 
  split(sp_key, ceiling(seq_along(sp_key$species)/ spp_per_chunk)) # 290 total

# GBIF accepts max of 3 concurrent requests so divide into sets of 3
sp_key <- split(sp_key, ceiling(seq_along(sp_key)/3))

# Separately send sets of 3 requests. 2nd request won't be sent until 1st has been downloaded.
# This function can take a while to run, e.g. a few minutes for each set of 100 species
walk2(sp_key, names(sp_key),  
      function(x, y) {
        # x is 1 set of 1-3 chunks of species
        # y is the set's name & is used to name the saved RDS versions of each set's wait_metadata
        # Send 1-3 requests to GBIF
        # *******************
        download_metadata <- map(x, ~f_occ_download_v3(.$specieskey))
        # *******************
        # Wait until downloads ready then save details of the requests
        wait_metadata <- map(download_metadata, ~occ_download_wait(.))
        # Saved RDS of wait_metdata is named with the set's name    
        saveRDS(wait_metadata, paste0(gb_dir, 'wait_metadata_', y, '.rds'))
        # Download the occurrence data once they're ready
        walk(wait_metadata, ~occ_download_get(.x$key, path = gb_dir))
        # Unzip downloaded data
        files_to_unzip <- map(wait_metadata, ~paste0(gb_dir, .$key, '.zip'))
        walk(files_to_unzip, ~unzip(., exdir = substring(paste(getwd(),gsub("\\.","",gb_dir),sep=""),1,nchar(paste(getwd(),gsub("\\.","",gb_dir),sep=""))-1)))
        # Read each CSV, add citation, save as RDS, delete ZIP & CSV
        csvs_to_read <- map(files_to_unzip, ~str_replace(., '.zip', '.csv'))
        rds_to_save <- map(files_to_unzip, ~str_replace(., '.zip', '.rds'))
        for (i in seq_along(csvs_to_read)) {
          d <- data.table::fread(csvs_to_read[[i]]) 
          comment(d) <- 
            c('download_link' = attributes(download_metadata[[i]])$downloadLink,
              'citation' = attributes(download_metadata[[i]])$citation)
          saveRDS(d, rds_to_save[[i]])
          file.remove(files_to_unzip[[i]])
          file.remove(csvs_to_read[[i]])
        } # end for
      } # end function(x, y)
) # end walk2

# this function takes a long time to run. 
# This part takes hours, not minutes

fns <- dir(gb_dir, pattern = '\\.rds', full = TRUE)
fns
# this only loads the first set of data if it came in multiple chunks
wait_metadata <- readRDS(fns[str_which(fns, 'metadata_1.rds')])
gb_occ <- readRDS(fns[str_which(fns, wait_metadata[[1]]$key)])# loads occurrence data
comment(gb_occ)['download_link'] # record this download link for later data reference
# "https://api.gbif.org/v1/occurrence/download/request/0035753-260519110011954.zip" - 
# this is the api for downloading the full data from the example dataset rather than just the single species
comment(gb_occ)['citation']
rps <- gb_occ %>% group_by(species) %>% summarise(n = n())
