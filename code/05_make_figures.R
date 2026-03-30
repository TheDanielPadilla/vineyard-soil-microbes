# 05_make_figures.R
# Purpose: Create final polished figures for the GitHub repository README.

library(tidyverse)
library(readr)

# ----------------------------
# Project paths
# ----------------------------
processed_dir <- "data/processed"
figures_dir <- "figures"

if (!dir.exists(figures_dir)) dir.create(figures_dir, recursive = TRUE)

# ----------------------------
# Shared factor levels and labels
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

# ----------------------------
# Shared plotting theme
# ----------------------------
theme_portfolio <- function() {
  theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold"),
      legend.title = element_text(face = "bold")
    )
}

# ----------------------------
# Load cleaned data
# ----------------------------
mb <- read_csv(
  file.path(processed_dir, "microbial_biomass_clean.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    treatment_code = factor(as.character(treatment_code), levels = treatment_levels),
    collection_stage = factor(as.character(collection_stage), levels = collection_levels),
    year = factor(year)
  )

cn <- read_csv(
  file.path(processed_dir, "carbon_nitrogen_clean.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    treatment_code = factor(as.character(treatment_code), levels = treatment_levels),
    collection_stage = factor(as.character(collection_stage), levels = collection_levels),
    year = factor(year)
  )

ph_data <- read_csv(
  file.path(processed_dir, "ph_clean.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    treatment_code = factor(as.character(treatment_code), levels = treatment_levels),
    collection_stage = factor(as.character(collection_stage), levels = collection_levels),
    year = factor(year)
  )

# ----------------------------
# Create summary tables inside this script
# ----------------------------
mb_summary_treatment_year <- mb %>%
  group_by(year, treatment_code, treatment_label) %>%
  summarise(
    mean_biomass = mean(dna_ng_per_gram_dry_soil, na.rm = TRUE),
    sd_biomass = ifelse(dplyr::n() > 1, sd(dna_ng_per_gram_dry_soil, na.rm = TRUE), NA_real_),
    .groups = "drop"
  )

mb_summary_stage <- mb %>%
  group_by(year, collection_stage, treatment_code, treatment_label) %>%
  summarise(
    mean_biomass = mean(dna_ng_per_gram_dry_soil, na.rm = TRUE),
    sd_biomass = ifelse(dplyr::n() > 1, sd(dna_ng_per_gram_dry_soil, na.rm = TRUE), NA_real_),
    .groups = "drop"
  )

cn_summary_treatment_year <- cn %>%
  group_by(year, treatment_code, treatment_label) %>%
  summarise(
    mean_cn_ratio = mean(cn_average, na.rm = TRUE),
    sd_cn_ratio = ifelse(dplyr::n() > 1, sd(cn_average, na.rm = TRUE), NA_real_),
    .groups = "drop"
  )

cn_summary_stage <- cn %>%
  group_by(year, collection_stage, treatment_code, treatment_label) %>%
  summarise(
    mean_cn_ratio = mean(cn_average, na.rm = TRUE),
    sd_cn_ratio = ifelse(dplyr::n() > 1, sd(cn_average, na.rm = TRUE), NA_real_),
    .groups = "drop"
  )

ph_summary_treatment_year <- ph_data %>%
  group_by(year, treatment_code, treatment_label) %>%
  summarise(
    mean_ph = mean(ph, na.rm = TRUE),
    sd_ph = ifelse(dplyr::n() > 1, sd(ph, na.rm = TRUE), NA_real_),
    .groups = "drop"
  )

# ----------------------------
# Figure 1: microbial biomass by treatment and year
# ----------------------------
fig_mb_treatment <- ggplot(
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
  theme_portfolio() +
  theme(legend.position = "none")

ggsave(
  file.path(figures_dir, "microbial_biomass_by_treatment.png"),
  fig_mb_treatment,
  width = 11,
  height = 6,
  dpi = 300
)

# ----------------------------
# Figure 2: microbial biomass fertilizer seasonal
# ----------------------------
fig_mb_fertilizer <- mb_summary_stage %>%
  filter(treatment_code %in% c("OF", "SF", "NF")) %>%
  ggplot(
    aes(
      x = collection_stage,
      y = mean_biomass,
      color = treatment_code,
      group = treatment_code
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
  theme_portfolio()

ggsave(
  file.path(figures_dir, "microbial_biomass_fertilizer_seasonal.png"),
  fig_mb_fertilizer,
  width = 10,
  height = 5.5,
  dpi = 300
)

# ----------------------------
# Figure 3: carbon:nitrogen ratio by treatment and year
# ----------------------------
fig_cn_treatment <- ggplot(
  cn_summary_treatment_year,
  aes(x = treatment_code, y = mean_cn_ratio, fill = treatment_code)
) +
  geom_col(width = 0.8) +
  geom_errorbar(
    aes(ymin = mean_cn_ratio - sd_cn_ratio, ymax = mean_cn_ratio + sd_cn_ratio),
    width = 0.2
  ) +
  facet_grid(~ year) +
  scale_x_discrete(labels = treatment_labels) +
  labs(
    title = "Carbon:Nitrogen Ratio by Treatment and Year",
    x = "Treatment",
    y = "Mean carbon:nitrogen ratio",
    fill = "Treatment"
  ) +
  theme_portfolio() +
  theme(legend.position = "none")

ggsave(
  file.path(figures_dir, "cn_ratio_by_treatment.png"),
  fig_cn_treatment,
  width = 11,
  height = 6,
  dpi = 300
)

# ----------------------------
# Figure 4: carbon:nitrogen fertilizer seasonal
# ----------------------------
fig_cn_fertilizer <- cn_summary_stage %>%
  filter(treatment_code %in% c("OF", "SF", "NF")) %>%
  ggplot(
    aes(
      x = collection_stage,
      y = mean_cn_ratio,
      color = treatment_code,
      group = treatment_code
    )
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_errorbar(
    aes(ymin = mean_cn_ratio - sd_cn_ratio, ymax = mean_cn_ratio + sd_cn_ratio),
    width = 0.15
  ) +
  facet_grid(~ year) +
  scale_color_discrete(labels = treatment_labels[c("OF", "SF", "NF")]) +
  labs(
    title = "Seasonal Carbon:Nitrogen Ratio: Fertilizer Treatments",
    x = "Sampling stage",
    y = "Mean carbon:nitrogen ratio",
    color = "Treatment"
  ) +
  theme_portfolio()

ggsave(
  file.path(figures_dir, "cn_ratio_fertilizer_seasonal.png"),
  fig_cn_fertilizer,
  width = 10,
  height = 5.5,
  dpi = 300
)

# ----------------------------
# Figure 5: soil pH by treatment and year
# ----------------------------
fig_ph_treatment <- ggplot(
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
  theme_portfolio() +
  theme(legend.position = "none")

ggsave(
  file.path(figures_dir, "soil_ph_by_treatment.png"),
  fig_ph_treatment,
  width = 11,
  height = 6,
  dpi = 300
)

message("Final portfolio figures created successfully.")