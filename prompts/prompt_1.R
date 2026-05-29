#---------------------------------------------------------------
# PROMPT 2 – Minimal version (On-topic + Weed name recovery only)
#---------------------------------------------------------------
# - Retains strict weed identification rules
# - Designed for high precision weed-name recovery
# - No common names allowed
# - Ambiguous species → NA
# - Abbreviated Latin names expanded only if genus is certain
# -Chris Buddenhagen 2026-03-26
#---------------------------------------------------------------

topic_settings <- tibble::tibble(
  topic_category = c(
    "On_topic_or_not",
    "weed_names"
  ),
  
  output_column = c(
    "on_topic_parsed",
    "weed_name_parsed"
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
     If uncertain, return NA."
  ),
  
  #---------------------------
  # USER PROMPTS
  #---------------------------
  user_text = c(
    
    # 1. On-topic detection
    "Identify if the article is about weed or invasive plant research. 
     Respond with 'yes' or 'no' without any additional text, labels, or formatting.",
    
    # 2. Weed name extraction
    "Identify plant species described as invasive, harmful, noxious, or weeds. 
     Return only semicolon-separated list of Latin binomials only (Genus species). 
     Expand abbreviated binomials using the closest earlier mention of the genus. 
     Rules:
      - Do NOT include disease-causing organisms (fungi, bacteria, viruses),
      - Do NOT include insect pests,
      - Do NOT include crops, natives, useful or endemic species,
      - Do NOT include species whose weed status is ambiguous,
      - Do NOT include common names. 
     Provide no explanatory text and deduplicate exact matches. 
     Return only the list or NA, with no extra text."
  )
)