f_occ_download_v3 <- function(species_keys) {
  # v3 includes PRESERVED_SPECIMEN & MATERIAL_SAMPLE & omits coordinateUncertaintyInMeters condition
  # gbif_keys is a vector of GBIF's specieskey
  # Function asks GBIF to create a file of occurrences for subsequent download from the user's GBIF account. It initiates data preparation by GBIF & returns the request's metadata. It's useful to save the metadata to an R object (list) because they can be submitted back to GBIF to check if the data are ready to download, plus they contain the citation details etc.
  # This function may return fewer records than indicated by rgbif::occ_count due to the predicates defined in rgbif::occ_download.
  rgbif::occ_download(
    pred_in("taxonKey", species_keys),
    pred_in("basisOfRecord", c('HUMAN_OBSERVATION', 'OBSERVATION', 'OCCURRENCE', 'MATERIAL_CITATION', 'MATERIAL_SAMPLE', 'LIVING_SPECIMEN', 'PRESERVED_SPECIMEN')),
    # pred_lte("coordinateUncertaintyInMeters", 10000),
    pred("hasCoordinate", TRUE),
    pred("hasGeospatialIssue", FALSE),
    format = "SIMPLE_CSV", 
    user = username,
    pwd = password,
    email = emailadd)
}

