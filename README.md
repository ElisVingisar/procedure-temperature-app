# Temperature-Procedure Analysis App
This repository contains the source code for an interactive R Shiny application developed as part of a bachelor's thesis at the University of Tartu. The application explores the associations between daily air temperature and the frequency of medical procedures in Estonian hospitals, using standardized health data in OMOP CDM format and the DLNM statistical model.
## Overview of the Features
Extreme temperatures can influence hospital workloads. This tool helps identify how daily mean temperatures (and their delayed effects up to 10 days) affect the frequency of selected medical procedures.
* Select hospitals and procedures
* Choose temperature and lag values
* Visualize results as:
  * Relative risk line plots
  * Interactive heatmaps
* Highlight statistically significant results
* Compare multiple heatmaps side-by-side
## Data & Privacy 
Health data (RITA MAITT, 2012–2019) is not included due to privacy restrictions. Temperature files are provided for selected Estonian cities. The app requires OMOP CDM-compatible data.
## Usage
1. Install the required R packages:
```r
install.packages(c(
  "shiny", "shinyjs", "plotly", "dlnm", "RColorBrewer",
  "DT", "dplyr", "tidyr", "tibble", "DatabaseConnector", "later"
))
```
2. Set up a `.Renviron` file or adjust `db_connect.R` <br> Make sure the `connect_db()` and `disconnect_db()` functions can access your database credentials — either via a `.Renviron` file or by editing `db_connect.R` directly.

3. Run the app <br> The application can be launched by running the `app/app.R` file.
## Author
Elis Vingisar
