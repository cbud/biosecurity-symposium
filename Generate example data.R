#recreate scopus data
library(readxl)
library(janitor)
library(tidyverse)

sample<-read_xlsx("data/combined_random_weeds_20260323.xlsx", sheet= "Sheet1") %>% distinct() %>% select(eid, manual_calls_weeds)

corpus<-readRDS("~/GitHub/AI biosecurity/corpuses/data_pubs_final_weeds.rds")

sample<-sample %>% left_join(corpus)

dput(names(sample)) 

sample<-sample %>% select("authors", "author_full_names", 
                "author_s_id", "title", "year", "source_title", "volume", "issue", 
                "art_no", "page_start", "page_end", "cited_by","eid", "doi", "link", 
                "affiliations", "authors_with_affiliations", "abstract", "author_keywords", 
                "index_keywords",  "document_type", "manual_calls_weeds" ) %>% distinct()


library(openxlsx)

write.xlsx(sample, "corpus/scopus_sample_data_set.xlsx", overwrite = TRUE)
           

