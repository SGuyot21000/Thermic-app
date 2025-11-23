# THERMIC  
**Thermal Resistance Modeling and Inactivation Calculator**

THERMIC is an R/Shiny application designed to model and analyze thermal inactivation of microorganisms using D-values and z-values. It integrates individual and mixed-effects models, supports uncertainty quantification, and provides interactive visualizations to explore the impact of temperature and other factors on microbial survival.

This repository contains:
- the full Shiny application code,
- the curated database of D-values,
- example data files for demonstration and testing.

---

## 1. Features

- Import of D-value datasets (literature-based or user-provided)  
- Calculation and visualization of:
  - log-linear models,
  - individual-based models per microorganism,
  - (optionally) mixed-effects models,
- Estimation and comparison of z-values,
- Graphical outputs for:
  - survival curves,
  - D(T) relationships,
  - confidence intervals and uncertainty,
- Export of summary tables and model parameters.

---

## 2. Repository structure

The main elements of this repository are:

- `app.R`  
  Main entry point of the Shiny application.

- `server_logic_v4.R`  
  Server logic and statistical modeling functions used by the app.

- `www/`  
  Static resources (logo, CSS, images, etc.).

- `data/` *(optional, to be created locally if not present)*  
  Contains:
  - full D-value database (if shared),
  - example datasets for demonstration.

- `Supp data_updated.xlsm`  
  Excel file with the curated D-value database and/or supporting data (see article for details).

- `LICENSE`  
  GNU General Public License v3.0 (GPL-3.0).

---

## 3. Installation

### 3.1. Prerequisites

- R (version ≥ 4.1 recommended)  
- RStudio (optional but convenient)  
- Suggested OS: Windows, Linux, or macOS

### 3.2. Required R packages

Install the required packages in R:

```r
install.packages(c(
  "shiny",
  "shinydashboard",
  "data.table",
  "dplyr",
  "ggplot2",
  "DT",
  "readxl",
  "readr"
  # add here any additional package used in the app
))
