# 04_soil_chemistry_analysis.R
# Purpose: Summarize and visualize vineyard soil pH data.

library(tidyverse)
library(readr)
library(forcats)

# ----------------------------
# Project paths
# ----------------------------
processed_dir <- "data/processed"
figures_dir <- "figures"
output_dir <- "output"

if (!dir.exists(figures_dir)) dir.create(figures_dir, recursive = TRUE)
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ----------------------------
# Load cleaned data
# ----------------------------
ph_data <- read_csv(
  file.path(processed_dir, "ph_clean.csv"),
  show_col_types = FALSE
)

# ----------------------------
# Standardize factors
# ----------------------------
treatment_levels <- c("HW", "LW", "NCC", "OF", "SF", "NF", "H", "NH")
collection_levels <- c("Bud Break", "Bloom", "Veraison", "Harvest")

treatment_labels <- c(
  HW  = "High Water Cover Crop",
  LW  = "Low Water Cover Crop",
  NCC = "No Cover Crop",
  OF  = "Organic Fertilizer",
  SF  = "Synthetic Fertilizer",
  NF  = "No Fertilizer",
  H   = "Herbicide",
  NH  = "No Herbicide"
)

ph_data <- ph_data %>%
  mutate(
    treatment_code = factor(as.character(treatment_code), levels = treatment_levels),
    collection_stage = factor(as.character(collection_stage), levels = collection_levels),
    year = factor(year)
  )

# ----------------------------
# Summary table: treatment x year
# ----------------------------
ph_summary_treatment_year <- ph_data %>%
  group_by(year, treatment_code, treatment_label) %>%
  summarise(
    mean_ph = mean(ph, na.rm = TRUE),
    sd_ph = ifelse(dplyr::n() > 1, sd(ph, na.rm = TRUE), NA_real_),
    n = dplyr::n(),
    se_ph = sd_ph / sqrt(n),
    .groups = "drop"
  ) %>%
  arrange(year, treatment_code)

write_csv(
  ph_summary_treatment_year,
  file.path(output_dir, "ph_summary_by_treatment_year.csv")
)

# ----------------------------
# Summary table: treatment x year x collection stage
# ----------------------------
ph_summary_stage <- ph_data %>%
  group_by(year, collection_stage, treatment_code, treatment_label) %>%
  summarise(
    mean_ph = mean(ph, na.rm = TRUE),
    sd_ph = ifelse(dplyr::n() > 1, sd(ph, na.rm = TRUE), NA_real_),
    n = dplyr::n(),
    se_ph = sd_ph / sqrt(n),
    .groups = "drop"
  ) %>%
  arrange(year, treatment_code, collection_stage)

write_csv(
  ph_summary_stage,
  file.path(output_dir, "ph_summary_by_stage.csv")
)

# ----------------------------
# Plot 1: mean pH by treatment and year
# ----------------------------
plot_treatment_year <- ggplot(
  ph_summary_treatment_year,
  aes(x = treatment_code, y = mean_ph, fill = treatment_code)
) +
  geom_col(width = 0.8) +
  geom_errorbar(
    aes(ymin = mean_ph - sd_ph, ymax = mean_ph + sd_ph),
    width = 0.2
  ) +
  facet_grid(~ year) +
  scale_x_discrete(labels = treatment_labels) +
  labs(
    title = "Soil pH by Treatment and Year",
    x = "Treatment",
    y = "Mean pH",
    fill = "Treatment"
  ) +
  coord_cartesian(ylim = c(6, 8)) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

ggsave(
  filename = file.path(figures_dir, "soil_ph_by_treatment.png"),
  plot = plot_treatment_year,
  width = 11,
  height = 6,
  dpi = 300
)

# ----------------------------
# Plot 2: fertilizer seasonal pattern
# ----------------------------
ph_fertilizer <- ph_summary_stage %>%
  filter(treatment_code %in% c("OF", "SF", "NF"))

plot_fertilizer_seasonal <- ggplot(
  ph_fertilizer,
  aes(
    x = collection_stage,
    y = mean_ph,
    group = treatment_code,
    color = treatment_code
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_errorbar(
    aes(ymin = mean_ph - sd_ph, ymax = mean_ph + sd_ph),
    width = 0.15
  ) +
  facet_grid(~ year) +
  scale_color_discrete(labels = treatment_labels[c("OF", "SF", "NF")]) +
  labs(
    title = "Seasonal Soil pH: Fertilizer Treatments",
    x = "Sampling stage",
    y = "Mean pH",
    color = "Treatment"
  ) +
  coord_cartesian(ylim = c(6, 8)) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = file.path(figures_dir, "soil_ph_fertilizer_seasonal.png"),
  plot = plot_fertilizer_seasonal,
  width = 10,
  height = 5.5,
  dpi = 300
)

# ----------------------------
# Plot 3: herbicide seasonal pattern
# ----------------------------
ph_herbicide <- ph_summary_stage %>%
  filter(treatment_code %in% c("H", "NH"))

plot_herbicide_seasonal <- ggplot(
  ph_herbicide,
  aes(
    x = collection_stage,
    y = mean_ph,
    group = treatment_code,
    color = treatment_code
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_errorbar(
    aes(ymin = mean_ph - sd_ph, ymax = mean_ph + sd_ph),
    width = 0.15
  ) +
  facet_grid(~ year) +
  scale_color_discrete(labels = treatment_labels[c("H", "NH")]) +
  labs(
    title = "Seasonal Soil pH: Herbicide Treatments",
    x = "Sampling stage",
    y = "Mean pH",
    color = "Treatment"
  ) +
  coord_cartesian(ylim = c(6, 8)) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = file.path(figures_dir, "soil_ph_herbicide_seasonal.png"),
  plot = plot_herbicide_seasonal,
  width = 10,
  height = 5.5,
  dpi = 300
)

message("Soil chemistry (pH) analysis complete.")