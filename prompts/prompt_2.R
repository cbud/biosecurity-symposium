#---------------------------------------------------------------
# PROMPT 2 – Improved version for higher weed–nonweed precision
#---------------------------------------------------------------
# Key improvements implemented:
# - Weed identification is now *strictly conservative*.
# - Any ambiguous species (crop? native? impacted but not invasive?) → return NA.
# - No common names allowed in any output.
# - Weed vs. impacted species are cleanly separated.
# - Research themes separated from on-topic classification.
# - Abbreviated Latin binomials are expanded using closest earlier genus.
# -Chris Buddenhagen 2026-03-26
#---------------------------------------------------------------

topic_settings <- tibble::tibble(
  topic_category = c(
    "On_topic_or_not",
    "weed_names",
    "impacted_species",
    "weed_habitat",
    "research_themes"
  ),
  
  output_column = c(
    "on_topic_parsed",
    "weed_name_parsed",
    "impacted_species_parsed",
    "habitat_parsed",
    "research_themes_parsed"
  ),
  
  #---------------------------
  # SYSTEM PROMPTS
  #---------------------------
  system_text = c(
    
    # 1. On-topic detection
    "You are a scientific assistant determining whether a research abstract is about invasive weeds or harmful invasive or alien plant species. Focus only on invasive or harmful plants, not general ecology.",
    
    # 2. Weed name extraction (strict)
      "You are a scientific assistant extracting names of invasive weeds and harmful invasive or alien plant species with high precision. 
      Extract only plant species explicitly described as invasive, harmful, noxious, or weedy. 
      If the species’ weed status is unclear or ambiguous, exclude it. 
      Expand abbreviated Latin binomials only when genus is certain from context. 
      Never infer, guess or add information that is not present in the text. 
      If uncertain, return NA.",
    
    # 3. Impacted species extraction (strict)
    "You are a scientific assistant identifying species of plants or crops that are negatively impacted by invasive weeds or harmful invasive or alien plant species. 
     Do not include the invasive species themselves. 
     Expand abbreviated Latin binomials only when genus is certain. 
     Respond concisely and return only valid Latin binomials.",
    
    # 4. Habitat detection
    "You are a scientific assistant identifying habitats and sectors affected by invasive weeds and harmful invasive or alien plant species. 
     Return only clear habitat or sector descriptors.",
    
    # 5. Research themes
    "You are a scientific assistant identifying scientific themes discussed in abstracts related to weeds and invasive plants."
  ),
  
  #---------------------------
  # USER PROMPTS
  #---------------------------
  user_text = c(
    
    # 1. On-topic detection
    "Identify if the article is about weed or invasive plant research. 
     Respond with 'Yes' or 'No' without any additional text, labels, or formatting.",
    
    # 2. Weed name extraction
    "Identify plant species described as invasive, harmful, noxious, or weeds. 
     Return only semicolon-separated list of Latin binomials only (Genus species). 
     Expand abbreviated binomials using the closest earlier mention of the genus. 
     Rules:
      - Do NOT include disease-causing organisms (fungi, bacteria, viruses),
      - Do NOT include insect pests,
      - Do NOT include crops, natives, useful or endemic species,
      - Do NOT include species whose weed status is ambiguous.
      - Do NOT include common names. 
     Provide no explanatory text and deduplicate exact matches. 
     Return only the list or NA, with no extra text.",
    
    # 3. Impacted species extraction
    "Identify plant species or crops that are negatively impacted by an invasive, harmful, noxious, or weedy species. 
     Return a concise semicolon-separated list of Latin binomials only (Genus species) but not the naming authority. 
     Expand abbreviated binomials using the closest earlier mention of the genus. 
     Rules:
       - Do NOT includedisease-causing organisms (fungi, bacteria, viruses),
       - Do NOT includeinsect pests,
       - Do NOT includeinvasive plants and weeds.
     Do not include common names. 
     Provide no explanatory text. 
     Return 'NA' if no valid impacted species are found.",
    
    # 4. Habitat detection
    "Identify the habitat(s) and sector(s) for the invasive weeds and harmful invasive or alien plant species. 
     Use semicolons to separate habitats and sectors. (e.g., 'arable; pasture; forest plantations; agriculture; forestry'). 
     Do not include common names or explanatory text. 
     Return 'NA' if none are found.",
    
    # 5. Research themes
    "Identify the scientific themes covered. 
     Provide semi-colon-separated responses that identify research areas: 
       biology and ecology; biosecurity, pathways, spread and risk; impacts; 
       naturalization; biocontrol; management; herbicide effectiveness; 
       herbicide resistance; integrated weed management; biodiversity impacts; 
       modelling; decision support; policy, governance and social dimensions. 
     Do not include extra explanatory text or formatting."
  )
)