# AI biosecurity workshop generalized

# ===========================================================
# LLM ANALYSIS PIPELINE — GENERALISED SCRIPT
# ===========================================================

# -------------------------------
# USER SETTINGS (EDIT THIS BLOCK)
# -------------------------------

settings <- list(
  
  # INPUT DATA
  input_file = "corpus/scopus_sample_data_set.xlsx",
  
  # OUTPUT
  out_dir = "./outputs/example_ai_run",
  
  # MODEL + PROCESSING
  model = "gpt-4o-mini",
  chunk_size = 100,
  
  # TOPIC FILTER COLUMN (from topic_settings$output_column)
  on_topic_column = "on_topic_parsed",   # <-- changeable
  
  # STRING THAT DEFINES "YES"
  on_topic_pattern = "^yes",
  
  # ENTITY COLUMN (parsed names from AI)
  entity_column = "weed_name_parsed",            # <-- changeable must match prompts
  entity_delim = ";",
  
  # GBIF SETTINGS
  gbif_kingdom = "Plantae",                      # change if needed
  
  # API KEY FILE
  api_key_file = "Biosecurity workshop api/Biosecurity_symposium_api.txt"
)

# -------------------------------
# PACKAGE SETUP
# -------------------------------

required_packages <- c(
  "tidyverse","readxl","janitor","httr","jsonlite",
  "progressr","tictoc","openai","gt","knitr"
)

installed <- rownames(installed.packages())

for (pkg in required_packages) {
  if (!pkg %in% installed) install.packages(pkg)
}

invisible(lapply(required_packages, library, character.only = TRUE))

# -------------------------------
# LOAD DATA
# -------------------------------

# select function to read in your data type

corpus <- read_xlsx(settings$input_file) %>% distinct() %>% clean_names() 

 #corpus <- readRDS(settings$input_file)

# corpus <- read_csv(settings$input_file) 
# -------------------------------
# API KEY
# -------------------------------
if (file.exists(settings$api_key_file)) {
  openai_key <- readLines(settings$api_key_file, warn = FALSE)
  Sys.setenv(OPENAI_API_KEY = openai_key)
} else {
  stop("API key file not found.")
}

# -------------------------------
# LOAD PROMPTS + FUNCTIONS
# -------------------------------
source("prompts/prompt_1.R")
source("functions/f_ai_text_analysis.R")
source("functions/f_get_gbif_backbone_ids_from_df.R")

# -------------------------------
# BUILD title_abstract FIELD
# -------------------------------
corpus <- corpus %>%
  rowwise() %>%
  mutate(
    title_abstract = stringr::str_squish(
      paste(na.omit(c(title, abstract, author_keywords, index_keywords)),
            collapse = " ")
    )
  ) %>%
  ungroup()

data_pubs_all <- corpus

data_pubs <- data_pubs_all %>%
  distinct(eid, .keep_all = TRUE) %>%
  mutate(row_id = row_number())

# -------------------------------
# CREATE OUTPUT DIR
# -------------------------------
if (!dir.exists(settings$out_dir)) dir.create(settings$out_dir, recursive = TRUE)

# ===========================================================
# 1. RUN TOPIC FILTER (ROW 1 OF topic_settings)
# ===========================================================
tic()

topic_category <- topic_settings$topic_category[1]
output_column  <- topic_settings$output_column[1]
system_text    <- topic_settings$system_text[1]
user_text      <- topic_settings$user_text[1]

topic_dir <- file.path(settings$out_dir, topic_category)

# ✅ CLEAN DIRECTORY (prevents duplicate chunk stacking)
if (dir.exists(topic_dir)) unlink(topic_dir, recursive = TRUE)
dir.create(topic_dir, recursive = TRUE)

chunks <- split(data_pubs,
                ceiling(seq_len(nrow(data_pubs)) / settings$chunk_size))

for (i in seq_along(chunks)) {
  
  message("Processing ", topic_category, " | Chunk ", i)
  
  result_vector <- f_ai_text_analysis(
    chunks[[i]]$title_abstract,
    settings$model,
    system_text,
    user_text
  )
  
  chunks[[i]][[output_column]] <- result_vector
  
  saveRDS(
    chunks[[i]],
    file = file.path(topic_dir, paste0(topic_category, "_chunk_", i, ".rds"))
  )
  
  # ✅ MEMORY CLEANING
  rm(result_vector)
  gc()
}

# ✅ FREE CHUNKS FROM MEMORY
rm(chunks)
gc()

# ✅ LOAD ONLY chunk files
files <- list.files(topic_dir, full.names = TRUE, pattern = "\\.rds$")

on_topic_final <- purrr::map_dfr(files, readRDS) %>%
  distinct(eid, .keep_all = TRUE)   # ✅ safeguard against duplication

# ✅ CLEAN TEMP OBJECTS
rm(files)
gc()

saveRDS(
  on_topic_final,
  file = file.path(topic_dir, paste0(topic_category, "_final_results.rds"))
)

toc()

# -------------------------------
# FILTER ON-TOPIC (GENERALIZED)
# -------------------------------
on_topic_pubs <- on_topic_final %>%
  filter(stringr::str_detect(
    tolower(.data[[settings$on_topic_column]]),
    settings$on_topic_pattern
  ))

# ✅ CLEAN UNUSED OBJECT
rm(on_topic_final)
gc()

# ===========================================================
# 2. RUN REMAINING AI TASKS
# ===========================================================
tic()

for (j in 2:nrow(topic_settings)) {
  
  topic_category <- topic_settings$topic_category[j]
  output_column  <- topic_settings$output_column[j]
  system_text    <- topic_settings$system_text[j]
  user_text      <- topic_settings$user_text[j]
  
  topic_dir <- file.path(settings$out_dir, topic_category)
  
  # ✅ CLEAN DIRECTORY BEFORE RUN
  if (dir.exists(topic_dir)) unlink(topic_dir, recursive = TRUE)
  dir.create(topic_dir, recursive = TRUE)
  
  chunks <- split(on_topic_pubs,
                  ceiling(seq_len(nrow(on_topic_pubs)) / settings$chunk_size))
  
  for (i in seq_along(chunks)) {
    
    message("Processing ", topic_category, " | Chunk ", i)
    
    result_vector <- f_ai_text_analysis(
      chunks[[i]]$title_abstract,
      settings$model,
      system_text,
      user_text
    )
    
    chunks[[i]][[output_column]] <- result_vector
    
    saveRDS(
      chunks[[i]],
      file = file.path(topic_dir, paste0(topic_category, "_chunk_", i, ".rds"))
    )
    
    # ✅ MEMORY CLEANING (critical)
    rm(result_vector)
    gc()
  }
  
  # ✅ REMOVE CHUNKS FROM MEMORY
  rm(chunks)
  gc()
  
  files <- list.files(topic_dir, full.names = TRUE, pattern = "\\.rds$")
  
  final_df <- purrr::map_dfr(files, readRDS) %>%
    distinct(eid, .keep_all = TRUE)  # ✅ safeguard
  
  # ✅ CLEAN FILE LIST
  rm(files)
  gc()
  
  saveRDS(
    final_df,
    file = file.path(topic_dir, paste0(topic_category, "_final_results.rds"))
  )
  
  # ✅ FREE MEMORY BEFORE NEXT LOOP
  rm(final_df)
  gc()
}

toc()

# ===========================================================
# MERGE AI RESULTS 
# ===========================================================

merge_ai_results <- function(topic_settings, out_dir, join_col = "eid") {
  
  results_list <- purrr::pmap(
    topic_settings,
    function(topic_category, output_column, system_text, user_text) {
      
      results_file <- file.path(
        out_dir,
        topic_category,
        paste0(topic_category, "_final_results.rds")
      )
      
      if (!file.exists(results_file)) return(NULL)
      
      readRDS(results_file) %>%
        distinct(.data[[join_col]], .keep_all = TRUE) %>%
        select(all_of(c(join_col, output_column)))
    }
  )
  
  purrr::compact(results_list)
}

ai_results <- merge_ai_results(topic_settings, settings$out_dir)

data_pubs_final <- purrr::reduce(
  ai_results,
  dplyr::left_join,
  by = "eid",
  .init = data_pubs_all
) %>%
  distinct()

saveRDS(data_pubs_final,
        file = file.path(settings$out_dir, "data_pubs_final.rds"))

# ===========================================================
# LONG FORMAT + GBIF (FULLY GENERALISED)
# ===========================================================

data_pubs_long <- data_pubs_final %>%
  tidyr::separate_longer_delim(
    .data[[settings$entity_column]],
    delim = settings$entity_delim
  ) %>%
  mutate(
    entity_clean = stringr::str_squish(.data[[settings$entity_column]])
  ) %>%
  filter(!is.na(entity_clean),
         entity_clean != "",
         entity_clean != "NA") %>%
  distinct(entity_clean, eid, .keep_all = TRUE)

gbif_results <- f_get_gbif_backbone_ids_from_df(
  df = data_pubs_long,
  species_col = "entity_clean",
  kingdom = settings$gbif_kingdom,
  use_progress = FALSE,
  save = FALSE
)

# OPTIONAL FILTER
gbif_results <- gbif_results %>%
  filter(kingdom_gbif == settings$gbif_kingdom)

# JOIN BACK
gbif_results_ai <- gbif_results %>%
  left_join(data_pubs_long,
            by = c("user_supplied_name" = "entity_clean"))

# SAVE
saveRDS(
  gbif_results_ai,
  file = file.path(
    settings$out_dir,
    paste0("gbif_results_", Sys.Date(), ".rds")
  )
)

# ===========================================================
# SUMMARY TABLE
# ===========================================================

p_table_ai <- gbif_results_ai %>%
  filter(!is.na(species_gbif)) %>%
  group_by(species_gbif) %>%
  summarise(
    publication_count = n_distinct(eid),
    names_used = paste(sort(unique(user_supplied_name)), collapse = "; "),
    .groups = "drop"
  ) %>%
  arrange(desc(publication_count))

p_table_ai %>%
  gt() %>%
  tab_header(title = "Publication counts by accepted species (GBIF)")

saveRDS(
  p_table_ai,
  file = file.path(
    settings$out_dir,
    paste0("article_counts_by_species_", Sys.Date(), ".rds")
  )
)

# ===========================================================
# DONE
# ===========================================================
message("Pipeline completed successfully.")
