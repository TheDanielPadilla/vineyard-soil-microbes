# Vineyard Soil Microbial Diversity

A senior research project examining how vineyard management practices relate to soil microbial biomass, soil chemistry, and seasonal variation in vineyard soils.

## Overview

This project explores how sustainable viticulture practices influence soil microbial and chemical properties in vineyard systems. The broader study was a multi-year interdisciplinary collaboration involving biochemistry, soil science, statistics, and wine and viticulture researchers.

The analysis compares treatment groups across three management categories:

- cover crops
- fertilizer
- herbicide

Sampling occurred at four stages of the growing season:

- Bud Break
- Bloom
- Veraison
- Harvest

This repository is a portfolio version of my analytical and visualization work from the project. It includes cleaned R scripts, selected figures, and a presentation summarizing the study.

## Research Questions

- How do vineyard management practices affect soil microbial biomass?
- How do soil chemistry measures such as carbon:nitrogen ratio and pH vary across treatments?
- Do treatment effects change over the growing season?
- What patterns are visible across years and management categories?

## Data and Measures

The repository includes analysis and visualizations for:

- microbial biomass concentration
- carbon:nitrogen ratio
- soil pH

The broader project also examined microbial diversity and additional soil chemistry measures in the context of sustainable vineyard management.

## Methods

The project uses R for data cleaning, summary statistics, and visualization across treatment groups, years, and seasonal sampling stages.

Main steps include:

1. loading and cleaning raw vineyard soil datasets
2. standardizing treatment and sampling-stage labels
3. summarizing outcomes by treatment, year, and collection stage
4. generating publication-style figures for microbial biomass, carbon:nitrogen ratio, and soil pH

## Repository Structure

- `presentation/` – final presentation slides summarizing the project
- `code/` – cleaned R scripts for loading, cleaning, analysis, and figure generation
- `figures/` – final exported figures used for GitHub display
- `data/raw/` – raw input data files, if shareable
- `data/processed/` – cleaned datasets produced by the scripts
- `output/` – summary tables exported by the analysis scripts
- `archive/` – older exploratory workbook materials
- `notes/` – project notes and context

## Main Scripts

- `01_load_and_clean_data.R` – imports and standardizes the raw datasets
- `02_microbial_biomass_analysis.R` – summarizes and visualizes microbial biomass results
- `03_carbon_nitrogen_analysis.R` – summarizes and visualizes carbon:nitrogen ratio results
- `04_soil_chemistry_analysis.R` – summarizes and visualizes soil pH results
- `05_make_figures.R` – creates the final polished figures used in this repository

## Selected Figures

<h3>Microbial Biomass by Treatment and Year</h3>
<img src="figures/microbial_biomass_by_treatment.png" alt="Microbial biomass by treatment and year" width="700">

<h3>Carbon:Nitrogen Ratio by Treatment and Year</h3>
<img src="figures/cn_ratio_by_treatment.png" alt="Carbon nitrogen ratio by treatment and year" width="700">

<h3>Soil pH by Treatment and Year</h3>
<img src="figures/soil_ph_by_treatment.png" alt="Soil pH by treatment and year" width="700">

<h3>Seasonal Microbial Biomass: Fertilizer Treatments</h3>
<img src="figures/microbial_biomass_fertilizer_seasonal.png" alt="Seasonal microbial biomass for fertilizer treatments" width="700">

## Key Findings

Based on the presentation and analysis materials:

- seasonal changes influenced microbial patterns in the vineyard soil data
- microbial biomass, carbon:nitrogen ratio, and pH can be compared across treatment groups and growing-season stages
- treatment effects were examined across multiple years and management categories, including cover crop, fertilizer, and herbicide conditions

## Project Context

This project was completed as part of a collaborative research effort on sustainable vineyard practices. The broader study examined how vineyard management practices may affect soil microbial communities, nutrient cycling, and soil-related indicators in viticulture systems.

## Limitations

This repository presents a cleaned portfolio version of the project rather than the full research archive. It focuses on selected outcome measures and visual summaries, and it does not include every experimental component from the broader study. Because this was collaborative research, some raw data or related materials may be omitted from the public version.

## Acknowledgment

This project was part of a collaborative research effort involving multiple faculty and student contributors across several departments. I am presenting this repository as a portfolio version of my analytical and visualization work related to the project.

## Author

Daniel Padilla
