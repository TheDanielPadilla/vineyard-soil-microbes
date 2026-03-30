# 02_microbial_biomass_analysis.R
# Purpose: Summarize and visualize vineyard soil microbial biomass data.

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
mb <- read_csv(
  file.path(processed_dir, "microbial_biomass_clean.csv"),
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

mb <- mb %>%
  mutate(
    treatment_code = factor(as.character(treatment_code), levels = treatment_levels),
    collection_stage = factor(as.character(collection_stage), levels = collection_levels),
    year = factor(year)
  )

# ----------------------------
# Choose response variable
# ----------------------------
# This matches the main outcome used in the original workbook:
# "ng dna/gram dry soil"
response_var <- "dna_ng_per_gram_dry_soil"

# ----------------------------
# Summary table: treatment x year
# ----------------------------
mb_summary_treatment_year <- mb %>%
  group_by(year, treatment_code, treatment_label) %>%
  summarise(
    mean_biomass = mean(.data[[response_var]], na.rm = TRUE),
    sd_biomass = sd(.data[[response_var]], na.rm = TRUE),
    n = dplyr::n(),
    se_biomass = sd_biomass / sqrt(n),
    .groups = "drop"
  ) %>%
  arrange(year, treatment_code)

write_csv(
  mb_summary_treatment_year,
  file.path(output_dir, "microbial_biomass_summary_by_treatment_year.csv")
)

# ----------------------------
# Summary table: treatment x year x collection stage
# ----------------------------
mb_summary_stage <- mb %>%
  group_by(year, collection_stage, treatment_code, treatment_label) %>%
  summarise(
    mean_biomass = mean(.data[[response_var]], na.rm = TRUE),
    sd_biomass = sd(.data[[response_var]], na.rm = TRUE),
    n = dplyr::n(),
    se_biomass = sd_biomass / sqrt(n),
    .groups = "drop"
  ) %>%
  arrange(year, treatment_code, collection_stage)

write_csv(
  mb_summary_stage,
  file.path(output_dir, "microbial_biomass_summary_by_stage.csv")
)

# ----------------------------
# Plot 1: mean microbial biomass by treatment and year
# ----------------------------
plot_treatment_year <- ggplot(
  mb_summary_treatment_year,
  aes(x = treatment_code, y = mean_biomass, fill = treatment_code)
) +
  geom_col(width = 0.8) +
  geom_errorbar(
    aes(ymin = mean_biomass - sd_biomass, ymax = mean_biomass + sd_biomass),
    width = 0.2
  ) +
  facet_grid(~ year) +
  scale_x_discrete(labels = treatment_labels) +
  labs(
    title = "Microbial Biomass by Treatment and Year",
    x = "Treatment",
    y = "Mean microbial biomass (ng DNA / gram dry soil)",
    fill = "Treatment"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

ggsave(
  filename = file.path(figures_dir, "microbial_biomass_by_treatment.png"),
  plot = plot_treatment_year,
  width = 11,
  height = 6,
  dpi = 300
)

# ----------------------------
# Plot 2: fertilizer seasonal pattern
# ----------------------------
mb_fertilizer <- mb_summary_stage %>%
  filter(treatment_code %in% c("OF", "SF", "NF"))

plot_fertilizer_seasonal <- ggplot(
  mb_fertilizer,
  aes(
    x = collection_stage,
    y = mean_biomass,
    group = treatment_code,
    color = treatment_code
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_errorbar(
    aes(ymin = mean_biomass - sd_biomass, ymax = mean_biomass + sd_biomass),
    width = 0.15
  ) +
  facet_grid(~ year) +
  scale_color_discrete(labels = treatment_labels[c("OF", "SF", "NF")]) +
  labs(
    title = "Seasonal Microbial Biomass: Fertilizer Treatments",
    x = "Sampling stage",
    y = "Mean microbial biomass (ng DNA / gram dry soil)",
    color = "Treatment"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = file.path(figures_dir, "microbial_biomass_fertilizer_seasonal.png"),
  plot = plot_fertilizer_seasonal,
  width = 10,
  height = 5.5,
  dpi = 300
)

# ----------------------------
# Plot 3: cover crop seasonal pattern
# ----------------------------
mb_cover_crop <- mb_summary_stage %>%
  filter(treatment_code %in% c("HW", "LW", "NCC"))

plot_cover_crop_seasonal <- ggplot(
  mb_cover_crop,
  aes(
    x = collection_stage,
    y = mean_biomass,
    group = treatment_code,
    color = treatment_code
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_errorbar(
    aes(ymin = mean_biomass - sd_biomass, ymax = mean_biomass + sd_biomass),
    width = 0.15
  ) +
  facet_grid(~ year) +
  scale_color_discrete(labels = treatment_labels[c("HW", "LW", "NCC")]) +
  labs(
    title = "Seasonal Microbial Biomass: Cover Crop Treatments",
    x = "Sampling stage",
    y = "Mean microbial biomass (ng DNA / gram dry soil)",
    color = "Treatment"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = file.path(figures_dir, "microbial_biomass_cover_crop_seasonal.png"),
  plot = plot_cover_crop_seasonal,
  width = 10,
  height = 5.5,
  dpi = 300
)

message("Microbial biomass analysis complete.")