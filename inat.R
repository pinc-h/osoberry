library(tidyverse)
library(ggpattern)

base_url <- "https://api.inaturalist.org/v1/observations"

fetch_inat_data <- function(term_id) {
  
  query <- list(
    taxon_id = 53419,
    term_id = term_id,
    per_page = 200,
    page = 1,
    quality_grade = "research",
    verifiable = TRUE
  )
  
  all_obs <- list()
  page <- 1
  
  while (TRUE) {
    query$page <- page
    response <- GET(base_url, query = query)
    data <- fromJSON(content(response, "text"), flatten = TRUE)
    if (length(data$results) == 0) break
    all_obs <- c(all_obs, list(data$results))
    page <- page + 1
    Sys.sleep(0.5)
  }
  
  bind_rows(all_obs)
  
}

sex_obs <- fetch_inat_data(9)

# Extract sex from the dataframe within the annotations column and add as a new column
sex_obs <- sex_obs %>%
  mutate(
    sex = sapply(annotations, function(anno) {
      
      # Check if annotations exist
      if (is.null(anno) || nrow(anno) == 0) return(NA_character_)
      
      sex_anno <- anno %>% 
        filter(controlled_attribute_id == 9)
      
      if (11 %in% sex_anno$controlled_value_id) {
        "Male"
      } else if (10 %in% sex_anno$controlled_value_id) {
        "Female"
      } else {
        NA_character_
      }
    })
  )

male_obs <- sex_obs %>% filter(sex == "Male")
female_obs <- sex_obs %>% filter(sex == "Female")
