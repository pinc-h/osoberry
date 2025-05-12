library(ggplot2)
library(dplyr)
library(ggpattern)

data <- read.csv("osoberry/data.csv")

visits_summary <- data %>%
  group_by(sex) %>%
  summarise(total_visits = sum(visits, na.rm = TRUE)) %>%
  mutate(sex = factor(sex, levels = c("f", "m"), labels = c("Female", "Male")))

plot1 <- visits_summary %>%
  ggplot(aes(x = sex, y = total_visits)) + 
  geom_bar(stat = "identity", width = 0.7, fill = "black", color = "black") +
  labs(title="Total Osoberry pollinator visits by sex", X = NULL, y = "Total Visits") +
  theme_minimal(base_size = 14)
plot1
ggsave(filename = "plot1.jpg", plot = plot1, height = 5, width = 7, units = "in")

#---------------------

indiv_summary <- data %>%
  group_by(indiv, sex) %>%
  summarise(total_visits = sum(visits, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(total_visits > 0) %>%
  mutate(indiv = factor(indiv, levels = sort(unique(indiv))))

plot2 <- indiv_summary %>%
  ggplot(aes(x = indiv, y = total_visits)) + 
  geom_col_pattern(aes(pattern = sex), fill = "white", colour = "black", 
                   pattern_density = 0.35, pattern_spacing = 0.01) +
  scale_pattern_manual(values = c("f" = "stripe", "m" = "crosshatch"),
                       labels = c("f" = "Female", "m" = "Male")) + 
  labs(title="Osoberry pollinator visits by individual (n = 26)", x = "Individual ID", y = "Total Visits", fill = "Sex") +
  theme_classic(base_size = 14) + 
  theme(legend.position = "top")
plot2
ggsave(filename = "plot2.jpg", plot = plot2, height = 5, width = 7, units = "in")

ggplot(indiv_summary, aes(x = indiv, y = total_visits)) +
  geom_col_pattern(
    aes(pattern = sex),
    fill = "white",
    colour = "black",
    pattern_density = 0.35,
    pattern_spacing = 0.01
  ) +
  scale_pattern_manual(
    values = c("f" = "stripe", "m" = "crosshatch"),
    labels = c("f" = "Female", "m" = "Male")
  ) +
  labs(
    title = "Osoberry pollinator visits by individual (n = 26)",
    x = "Individual ID", 
    y = "Total Visits"
  ) +
  theme_classic() +
  theme (
    legend.position = "top"
  )
