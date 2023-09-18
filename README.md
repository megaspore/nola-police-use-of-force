# nola-police-use-of-force
NOLA Police Use of Force Data Science Project
# NOPD Use of Force Analysis

Welcome to the NOPD Use of Force Analysis project. This repository contains data and R code used to analyze the use of force incidents by the New Orleans Police Department (NOPD). 

The NOPD_Use_of_Force_Incidents dataset provides a detailed snapshot of New Orleans Police officers' use of physical force during their duties, sourced from the New Orleans city record archive API as of May 2023. With 33 feature columns and 3314 rows, each entry represents a specific incident, encompassing a wide range of information from incident details to officers' demographics, environmental conditions, and nuanced elements of the force employed. However, the dataset's original format, intended for record-keeping rather than analysis, presented challenges due to concatenation, particularly in relating officers to specific subjects. Extensive data preprocessing was necessary to make it suitable for analysis, involving organization, restructuring, and cleaning. A weighted "Grade" feature was introduced to classify incidents, with key findings revealing the impact of subjects' mental stability, alcohol consumption, and force type on incident outcomes. Disparities in gender and racial representation within the police force compared to incident data underscored the need for adjustments.

To predict incident grades, various models were explored, including linear regression, Random Forest, XGBoost, and TensorFlow. A "precognitive" model aimed to predict grades using factors leading up to an incident, while a comprehensive model incorporated all successful predictive features, both proving effective. XGBoost and TensorFlow emerged as top performers, showcasing advanced predictive capabilities. Overall, this project highlights crucial insights into NOPD use-of-force incidents, signaling areas for improvement and addressing disparities.




Below is an overview of the project files:

## Files

- **NOPD_Use_of_Force_Incidents.csv:** This file contains the dataset of NOPD use of force incidents. To use it, ensure it's in the same working directory as the R script.

- **nola_use_of_force_rcode.R:** This R script is the core of the analysis. It includes data preprocessing, analysis, and visualization code.

- **testknit.pdf:** This is a PDF document generated from the R script using a tool like R Markdown. It provides a compiled version of the R script's output and results.

## Usage

1. Ensure that the `NOPD_Use_of_Force_Incidents.csv` file is in the same directory as the `nola_use_of_force_rcode.R` script to execute the analysis.

2. Run the `nola_use_of_force_rcode.R` script to perform the analysis.

3. Review the results and insights in the `testknit.pdf` document.
