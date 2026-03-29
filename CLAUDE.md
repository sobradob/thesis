# Thesis: Missing Data in GPS Measurements (PPMI)

Research thesis by Boaz Sobrado (Utrecht University, 2017) proposing **Personal Map Matched Imputation (PPMI)** — a method for handling missing and noisy smartphone GPS data by building personalised spatial maps that leverage the regularity of human mobility.

## Project Structure

- **preprocessing/** — Converts raw Google Location Services JSON to R data frames
- **PPMI/** — Core PPMI method implementation (main script + 15 support functions)
- **PalmiusImplementation/** — Palmius comparison method (11 functions)
- **Barnett&Onnella_Implementation/** — Barnett & Onnella baseline comparison
- **Objective Results/** — Quantitative accuracy evaluation across methods
- **Aggregate Results/** — Practical application evaluation (e.g. time spent at home)
- **scripts/** — Exploratory analysis scripts (data wrangling, feature extraction, visualisation)
- **Thesis Manuscript/** — Final APA-formatted manuscript (Rmd → LaTeX → PDF)
- **Thesis Report/** — Earlier 2017 report version
- **Correspondence/** — Research emails

## Tech Stack

- **Language:** R (primary), with R Markdown and LaTeX for document generation
- **Key R packages:** sp, raster, rgdal, geosphere, dplyr, tidyr, ggplot2, leaflet, keras, papaja, knitr
- **Build pipeline:** R Markdown → knitr → LaTeX → PDF

## Data

Raw GPS data is **not included** in the repository (sensitive personal location data). Prospective researchers can obtain their own data via Google Takeout. Preprocessed intermediate files (RDS format) are expected by the analysis scripts.

## Code Conventions

- Each method directory contains a main script, a `functions/` subfolder, and a `scripts/` subfolder
- Functions are sourced dynamically via `source()` calls in main scripts
- Results are generated as R objects (RDS) and visualised with ggplot2/leaflet
- The manuscript uses the `papaja` package for APA formatting
