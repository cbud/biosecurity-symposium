library(tidyverse)
library(rgbif)
library(janitor)
library(progressr)

f_get_gbif_backbone_ids_from_df <- function(
    df,
    species_col,
    batch_size = 50,
    sleep_time = 0.5,
    save = TRUE,
    out_dir = "Outputs/species_detected",
    file_prefix = "gbif_ids",
    use_progress = interactive(),
    kingdom = NULL,
    phylum = NULL
) {
  
  species_col <- enquo(species_col)
  
  species_vec <- df |>
    dplyr::pull(!!species_col) |>
    unique() |>
    sort() |>
    as.character()
  
  species_chunks <- split(
    species_vec,
    ceiling(seq_along(species_vec) / batch_size)
  )
  
  # ---------------------------
  # ✅ Smart resolver
  # ---------------------------
  resolve_name <- function(sp, kingdom = NULL, phylum = NULL) {
    
    res <- tryCatch(
      rgbif::name_backbone(
        name = sp,
        kingdom = kingdom,
        phylum = phylum,
        rank = "SPECIES"
      ),
      error = function(e) return(NULL)
    )
    
    # Retry if poor match
    if (!is.null(res) &&
        !is.null(res$matchType) &&
        res$matchType == "HIGHERRANK") {
      
      res2 <- tryCatch(
        rgbif::name_backbone(
          name = sp,
          kingdom = kingdom,
          phylum = phylum,
          strict = FALSE,
          rank = "SPECIES"
        ),
        error = function(e) return(NULL)
      )
      
      if (!is.null(res2) &&
          !is.null(res2$rank) &&
          res2$rank == "SPECIES") {
        res <- res2
      }
    }
    
    return(res)
  }
  
  if (use_progress) {
    progressr::handlers(global = TRUE)
  }
  
  run_batches <- function(p = NULL) {
    purrr::map_dfr(species_chunks, function(chunk) {
      
      res <- purrr::map_dfr(chunk, function(sp) {
        
        if (use_progress && !is.null(p)) p()
        
        out <- resolve_name(sp, kingdom, phylum)
        
        if (!is.null(out)) {
          out$user_supplied_name <- sp
        }
        
        out
      })
      
      Sys.sleep(sleep_time)
      res
    })
  }
  
  if (use_progress) {
    gbif_df <- progressr::with_progress({
      p <- progressr::progressor(along = species_vec)
      run_batches(p)
    })
  } else {
    gbif_df <- run_batches()
  }
  
  # ---------------------------
  # ✅ Clean + keys
  # ---------------------------
  gbif_df <- gbif_df |>
    janitor::clean_names()
  
  gbif_df <- gbif_df |>
    mutate(
      taxon_key = usage_key,
      accepted_taxon_key = dplyr::coalesce(
        species_key,
        usage_key
      ),
      match_issue = rank != "SPECIES" | match_type %in% c("HIGHERRANK", "NONE")
    )
  
  # ---------------------------
  # ✅ SAVE MATCH ISSUES AS CHECKLIST TIBBLES
  # ---------------------------
  match_issues_df <- gbif_df |>
    dplyr::filter(match_issue)
  
  if (nrow(match_issues_df) > 0) {
    
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    
    purrr::walk(
      match_issues_df$user_supplied_name,
      function(sp) {
        
        safe_name <- gsub("[^A-Za-z0-9]+", "_", sp)
        
        file <- file.path(
          out_dir,
          paste0("gbif_checklist_", safe_name, ".rds")
        )
        
        # ✅ This is the key change: save full checklist tibble
        checklist_tbl <- tryCatch(
          rgbif::name_backbone_checklist(
            sp,
            kingdom = kingdom,
            verbose = TRUE
          ),
          error = function(e) {
            message("Checklist failed for: ", sp)
            return(NULL)
          }
        )
        
        if (!is.null(checklist_tbl)) {
          saveRDS(checklist_tbl, file)
          message("Saved checklist tibble: ", sp)
        }
      }
    )
    
    # Optional: save list of all problem names
    saveRDS(
      match_issues_df$user_supplied_name,
      file.path(out_dir, paste0("gbif_match_issue_names_", Sys.Date(), ".rds"))
    )
  }
  # ---------------------------
  # ✅ Rename taxonomy columns
  # ---------------------------
  tax_cols <- c(
    "kingdom", "phylum", "class", "order",
    "family", "genus", "species", "rank", "status"
  )
  
  gbif_df <- gbif_df |>
    dplyr::rename_with(~ paste0(.x, "_gbif"), .cols = any_of(tax_cols))
  
  # ---------------------------
  # ✅ Optional filters
  # ---------------------------
  if (!is.null(kingdom)) {
    gbif_df <- gbif_df |> dplyr::filter(kingdom_gbif == kingdom)
  }
  
  if (!is.null(phylum)) {
    gbif_df <- gbif_df |> dplyr::filter(phylum_gbif == phylum)
  }
  
  # ---------------------------
  # ✅ Save main output
  # ---------------------------
  if (save) {
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    
    outfile <- file.path(
      out_dir,
      paste0(file_prefix, "_", Sys.Date(), ".rds")
    )
    
    saveRDS(gbif_df, outfile)
  }
  
  return(gbif_df)
}

# ------------------------------
# ✅ Example dataset (unchanged)
# ------------------------------
# example_df <- tibble::tibble(
#   species_name = c(
#     "Festuca arundinacea",
#     "Conyza bonariensis",
#     "Lolium arundinaceum",
#     "Aster novi-belgii",
#     "Calystegia sepium",
#     "Senecio jacobaea",
#     "Agropyron repens",
#     "Anopheles gambiae"
#   
#   )
# )
# 
# # ------------------------------
# # ✅ Run function
# # ------------------------------
# gbif_results <- f_get_gbif_backbone_ids_from_df(
#   df = example_df,
#   species_col = species_name,
#   use_progress = FALSE,
#   save = FALSE,
#  # phylum = "Arthropoda",
#   kingdom = "Plantae"
# )
# 
# # Inspect columns
