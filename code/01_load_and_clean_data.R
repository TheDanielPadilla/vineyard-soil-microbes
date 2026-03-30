# 01_load_and_clean_data.R
# Purpose: Load raw vineyard soil datasets, standardize names and labels,
# and save cleaned files for downstream analysis.

library(tidyverse)
library(janitor)
library(readr)

# ----------------------------
# Project paths
# ----------------------------
raw_dir <- "data/raw"
processed_dir <- "data/processed"

if (!dir.exists(processed_dir)) {
  dir.create(processed_dir, recursive = TRUE)
}

# ----------------------------
# Shared factor levels / labels
# ----------------------------
treatment_levels <- c("HW", "LW", "NCC", "OF", "SF", "NF", "H", "NH")

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

collection_levels <- c("Bud Break", "Bloom", "Veraison", "Harvest")

# ----------------------------
# Helper functions
# ----------------------------
clean_treatment_factor <- function(x) {
  factor(x, levels = treatment_levels)
}

clean_collection_factor <- function(x) {
  factor(x, levels = collection_levels)
}

add_treatment_label <- function(df, treatment_col = "treatment_code") {
  df %>%
    mutate(
      treatment_label = recode(.data[[treatment_col]], !!!treatment_labels)
    )
}

# ----------------------------
# 1. Microbial biomass data
# ----------------------------
# The original workbook loads three yearly files separately and combines them.
# For a cleaned version, stack them with bind_rows().
# Source files referenced in workbook:
# - 2016 microbial biomass.csv
# - 2017 microbial biomass.csv
# - 2018 microbial biomass.csv

mb_2016 <- read_csv(
  file.path(raw_dir, "2016 microbial biomass.csv"),
  show_col_types = FALSE
) %>%
  mutate(year = 2016)

mb_2017 <- read_csv(
  file.path(raw_dir, "2017 microbial biomass.csv"),
  show_col_types = FALSE
) %>%
  mutate(year = 2017)

mb_2018 <- read_csv(
  file.path(raw_dir, "2018 microbial biomass.csv"),
  show_col_types = FALSE
) %>%
  mutate(year = 2018)

microbial_biomass_clean <- bind_rows(mb_2016, mb_2017, mb_2018) %>%
  clean_names() %>%
  rename(
    collection_stage = collection,
    treatment_code = treatment,
    average_ng_ul = average_ng_ul,
    st_dev_ng_ul = st_dev,
    water_content = water_content,
    wet_soil_mass = mass_wet_soil,
    dna_mass_ng = dna_mass_ng,
    dry_soil_grams = grams_dry_soil,
    dna_ng_per_gram_dry_soil = ng_dna_gram_dry_soil,
    dilution_factor = dilution_factor,
    sample_volume_ul = sample_volume_ul
  ) %>%
  mutate(
    year = as.integer(year),
    treatment_code = clean_treatment_factor(treatment_code),
    collection_stage = clean_collection_factor(collection_stage)
  ) %>%
  add_treatment_label("treatment_code") %>%
  arrange(year, treatment_code, collection_stage)

write_csv(
  microbial_biomass_clean,
  file.path(processed_dir, "microbial_biomass_clean.csv")
)

# ----------------------------
# 2. Carbon / nitrogen data
# ----------------------------
# Workbook indicates one file:
# - carbon.nitrogen.csv
# Columns include Collection, Sample, C/N 1, C/N 2, C/N 3, average, std, Year
# plus two empty columns that can be removed.

carbon_nitrogen_clean <- read_csv(
  file.path(raw_dir, "carbon.nitrogen.csv"),
  show_col_types = FALSE
) %>%
  clean_names() %>%
  select(-starts_with("x"), -starts_with("unnamed"), -matches("^\\.\\.\\.")) %>%
  rename(
    collection_stage = collection,
    treatment_code = sample,
    cn_rep_1 = c_n_1,
    cn_rep_2 = c_n_2,
    cn_rep_3 = c_n_3,
    cn_average = average,
    cn_sd = std,
    year = year
  ) %>%
  mutate(
    year = as.integer(year),
    treatment_code = clean_treatment_factor(treatment_code),
    collection_stage = clean_collection_factor(collection_stage)
  ) %>%
  add_treatment_label("treatment_code") %>%
  arrange(year, treatment_code, collection_stage)

write_csv(
  carbon_nitrogen_clean,
  file.path(processed_dir, "carbon_nitrogen_clean.csv")
)

# ----------------------------
# 3. pH data
# ----------------------------
# Workbook indicates:
# - pH.all.csv
# with columns Treatment, Collection, Year, pH

ph_clean <- read_csv(
  file.path(raw_dir, "pH.all.csv"),
  show_col_types = FALSE
) %>%
  clean_names() %>%
  rename(
    treatment_code = treatment,
    collection_stage = collection,
    year = year,
    ph = p_h
  ) %>%
  mutate(
    year = as.integer(year),
    treatment_code = clean_treatment_factor(treatment_code),
    collection_stage = clean_collection_factor(collection_stage)
  ) %>%
  add_treatment_label("treatment_code") %>%
  arrange(year, treatment_code, collection_stage)

write_csv(
  ph_clean,
  file.path(processed_dir, "ph_clean.csv")
)

# ----------------------------
# 4. Optional summary tables
# ----------------------------

microbial_biomass_summary <- microbial_biomass_clean %>%
  group_by(year, treatment_code, treatment_label) %>%
  summarise(
    mean_microbial_biomass = mean(dna_ng_per_gram_dry_soil, na.rm = TRUE),
    sd_microbial_biomass = sd(dna_ng_per_gram_dry_soil, na.rm = TRUE),
    n = dplyr::n(),
    .groups = "drop"
  )

write_csv(
  microbial_biomass_summary,
  file.path(processed_dir, "microbial_biomass_summary.csv")
)

carbon_nitrogen_summary <- carbon_nitrogen_clean %>%
  group_by(year, treatment_code, treatment_label) %>%
  summarise(
    mean_cn_ratio = mean(cn_average, na.rm = TRUE),
    sd_cn_ratio = sd(cn_average, na.rm = TRUE),
    n = dplyr::n(),
    .groups = "drop"
  )

write_csv(
  carbon_nitrogen_summary,
  file.path(processed_dir, "carbon_nitrogen_summary.csv")
)

ph_summary <- ph_clean %>%
  group_by(year, treatment_code, treatment_label) %>%
  summarise(
    mean_ph = mean(ph, na.rm = TRUE),
    sd_ph = sd(ph, na.rm = TRUE),
    n = dplyr::n(),
    .groups = "drop"
  )

write_csv(
  ph_summary,
  file.path(processed_dir, "ph_summary.csv")
)

message("Data cleaning complete.")

