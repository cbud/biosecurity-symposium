###############################################
# Before you can download a set of occurrence record data from GBIF for a bunch of species 
# you have to do some taxonomic cleaning

###############################################
# load libraries
library(tidyverse)

##################################################
# loading of specialty functions
source("r_functions/functions_taxa.r")

###############################################
exampledataset <- read_csv("data/exampledataset20260526.csv")

###################################################
# precleaning

exampledataset$GS<-trimws(exampledataset$Identity)
exampledataset$GS<-preclean(exampledataset$GS) # custom function
exampledataset$GS<-str_to_sentence(exampledataset$GS) # makes sure only first letter is uppercase
exampledataset$GS<-gsub("\u00A0", "",exampledataset$GS) 

# check for reps
counts<-table(exampledataset$GS)
counts[counts>1] # no reps

###############
# species list
tax_sp<-unique(exampledataset$GS) # 73 

#####################################
### Get taxonomy info from GBIF   ###
#####################################
xtra_cols <- c("kingdomkey", "phylumkey", "classkey", "orderkey", "specieskey",
               "note", "familykey", "genuskey", "scientificname", "canonicalname", "confidence")

######################
# apply the get_accepted_taxonomy function over the vector of species names
tax_acc_l <- lapply(tax_sp,get_accepted_taxonomy_insect_sp) # takes a couple of minutes to run
# custom function

# make dataframe of all results
suppressMessages(
  tax_acc_sp <- tax_acc_l %>% 
    purrr::reduce(full_join) %>% 
    mutate(genus_species = str_squish(genus_species)) %>% 
    select(-one_of(xtra_cols))
)

# merge
exampledataset<-rename(exampledataset,"user_supplied_name"="GS")
exampledataset<-left_join(exampledataset,tax_acc_sp,by=c("user_supplied_name"))

##################################
# check the gbif outputs

# (1) some species GBIF may not find at all
sp_not_found<-tax_acc_sp[tax_acc_sp$genus_species=="species not found",]$user_supplied_name
sp_not_found
# can do manual check for any synonyms

# replace the genus species for these with the original as default option
exampledataset[exampledataset$user_supplied_name%in%sp_not_found,"genus_species"]<-exampledataset[exampledataset$user_supplied_name%in%sp_not_found,"user_supplied_name"]

# (2) some species GBIF will only find a genus level match
genus_only<-tax_acc_sp[tax_acc_sp$rank=="genus"&!is.na(tax_acc_sp$rank),]$user_supplied_name
genus_only

# replace the genus species for these with the original as default option
exampledataset[exampledataset$user_supplied_name%in%genus_only,"genus_species"]<-exampledataset[exampledataset$user_supplied_name%in%genus_only,"user_supplied_name"]

# check if list had unique species - this is a good point to see if you had synonyms in the list
length(unique(exampledataset$genus_species))# 73

# bring across the accepted usage key for the names which were synonyms
exampledataset[is.na(exampledataset$acceptedusagekey),"acceptedusagekey"]<-exampledataset[is.na(exampledataset$acceptedusagekey),"usagekey"]


write.csv(exampledataset,"data/exampledataset_GBIFtaxaJune2026.csv",row.names = FALSE)
