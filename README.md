# Cyclistic Bike-Share Case Study

## Overview
This project analyzes how casual riders and annual members use Cyclistic bike-share services differently.
The objective is to generate insights that support strategies for converting casual riders into annual members.

The analysis was conducted using R as part of the Google Data Analytics Capstone.

---

## Report
The full analysis, visualizations, and findings are available in the HTML report:

-[Click to Open Report](https://udohchioma78.github.io/Cyclistic-Bike-Share-Case-Study/)

---

## Data Source
The data used in this project comes from Divvy Bikes’ publicly available trip data.

The raw CSV files used for this analysis are included in this repository.
These files are used directly in the code to ensure reproducibility of the analysis.

---

## Data License
This project uses data provided under the Divvy Bikes Data License Agreement:
- [https://www.divvybikes.com/data-license-agreement](https://divvybikes.com/data-license-agreement)

---

## Tools Used
- R
- RStudio
- tidyverse
- lubridate
- ggplot2
- DT

---

## Files in This Repository
- `index.html` - Knit report with analysis, tables, and visualizations
- `Cyclistic-Bike-Share-Case-Study.Rmd` - Full R Markdown analysis and data cleaning steps
- `Divvy_Trips_2019_Q1.zip` – Contains the `Divvy_Trips_2019_Q1.csv` file
- `Divvy_Trips_2020_Q1.zip` – Contains the `Divvy_Trips_2020_Q1.csv` file
- `README.md` - Project overview and data access information
- `LICENSE-DATA.txt` – Contains the licensing information for the Divvy trip datasets.
- `My_cleaning_and_analysis_process_for_Divvy_Trips_Q1_2019_and_2020_data.R` – R script containing the data cleaning and analysis process for the 2019 and 2020 Q1 Divvy trip datasets.
---

## How to Reproduce This Analysis

1. Download the zip files in this repository (`Divvy_Trips_2019_Q1.zip` and `Divvy_Trips_2020_Q1.zip`).

2. Extract both zip files to obtain the CSV datasets.

3. Place all project files (CSV files, `.R` script, and `.Rmd` file) in the same folder.

4. Open this folder as your **working directory** in RStudio.

5. Install the required R packages listed at the top of the scripts if they are not already installed.

6. Run the script `My_cleaning_and_analysis_process_for_Divvy_Trips_Q1_2019_and_2020_data.R` to perform the data cleaning and preparation.

7. Open `Cyclistic-Bike-Share-Case-Study.Rmd` and **Knit** the file to reproduce the analysis, tables, and visualizations.
