library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggpattern)
library(sf)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)

library(ggplot2)

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

sex_counts <- sex_obs %>%
  filter(!is.na(sex)) %>%
  count(sex, name = "Count")

plot1 <- sex_counts %>%
  ggplot(aes(x = sex, y = Count)) + 
  geom_bar(stat = "identity", width = 0.7, fill = "black", color = "black") +
  labs(x = "Sex", y = "Observations") +
  theme_classic(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    )
plot1
ggsave(filename = "plot1.jpg", plot = plot1, height = 5, width = 7, units = "in")

# Extract lat/long
sex_obs_geo <- sex_obs %>%
  separate(
    location,
    into = c("latitude", "longitude"),
    sep = ",", 
    convert = TRUE
  ) %>%
  filter(
    !is.na(sex),
    !is.na(latitude),
    !is.na(longitude),
    between(longitude, -140, -100),
    between(latitude, 25, 60)
  )

west_na <- ne_countries(
  scale = "medium",
  continent = "North America",
  returnclass = "sf"
) %>% 
  st_crop(xmin = -140, ymin = 25, xmax = -100, ymax = 60)  # Crops to west NA

plot2 <-ggplot() +
  geom_sf(data = west_na, fill = "gray90", color = "gray70") + 
  geom_point(
    data = sex_obs_geo,
    aes(x = longitude, y = latitude, color = sex),
    size = 3, alpha = 0.8
  ) +
  coord_sf(
    xlim = c(-128.5, -120),
    ylim = c(35, 51.5),
    expand = FALSE
  ) +
  scale_color_manual(
    values = c("Female" = "deeppink", "Male" = "dodgerblue"),
    guide = guide_legend(title = NULL)
  ) +
  theme_void()

plot2
ggsave(filename = "plot2.jpg", plot = plot2, height = 7, width = 6, units = "in")
