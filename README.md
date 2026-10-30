# CardioPredict: Cardiovascular Disease Occurrence Prediction and Visualization Dashboard

An enterprise-grade analytical dashboard engineered in **R and Shiny**, adhering to strict epidemiological methodology, machine learning best practices, and survey-weighted estimation.

CardioPredict focuses exclusively on **five core research questions and visualizations**, comparing records from the **Kaggle Cardiovascular Disease Dataset** (70,000 clinical records) and the **CDC Behavioral Risk Factor Surveillance System (BRFSS 2015)**.

---

## 1. Five Core Research Questions & Visualizations

| # | Research Question | Visualization Type | Statistical & Epidemiological Methodology |
|---|---|---|---|
| **1** | Which age groups have the highest recorded cardiovascular disease prevalence? | **Interactive Line Chart** | Ascending 5-year age bands; calculates positive outcome prevalence over eligible cohort; dynamic non-hardcoded textual finding. |
| **2** | How does disease prevalence vary across cardiovascular risk factors? | **Interactive 2D Heatmap** | Cross-tabulates clinical biomarkers (BP categories, cholesterol, glucose) and behavioral factors (smoking, activity, BMI); displays sample counts & percentages; non-causal observational finding. |
| **3** | Which features contribute most to the machine learning model's predictions? | **Interactive Horizontal Bar Chart** | Random Forest Mean Decrease in Gini Impurity ranking; strict exclusion of target variables and record IDs; dynamic statement of predictor hierarchy. |
| **4** | Which machine learning model best distinguishes the recorded cardiovascular disease outcome? | **Multi-Model ROC Curve** | Evaluates **Logistic Regression**, **Random Forest**, and **Support Vector Machine (SVM)** on identical 20% stratified held-out test sets; reports ROC-AUC with random reference baseline (y = x); includes integrated individual risk factor prediction form. |
| **5** | How does recorded cardiovascular disease prevalence vary across income and education groups? | **Grouped Bar Chart with 95% CIs** | Analyzes CDC BRFSS 2015 socioeconomic strata; accounts for complex survey weights (`_LLCPWT`); reports 95% confidence intervals; dynamic finding of highest observed burden. |

> **Final Dashboard Restriction**: The application contains strictly these five analytical visualizations. No pie charts, extra bar charts, confusion matrix heatmaps, prediction gauges, or 6th visualizations are introduced.

---

## 2. Project Architecture

```
cardiopredict/
├── app.R                          # Primary Shiny UI layout, sidebar navigation, reactive server logic
├── R/
│   ├── utils.R                    # Formatting, Wilson score confidence intervals, theme configs
│   ├── data_processing.R          # Semicolon/comma CSV & XPT import, BP filtering, category decoding
│   ├── age_prevalence.R           # Visualization 1: Line chart & dynamic finding
│   ├── risk_factor_heatmap.R      # Visualization 2: 2D prevalence heatmap & dynamic finding
│   ├── feature_importance.R       # Visualization 3: Random Forest feature ranking & statement
│   ├── model_evaluation.R         # Visualization 4: Train Logistic Regression, RF, SVM & ROC plot
│   └── socioeconomic_analysis.R   # Visualization 5: BRFSS income/education bar chart & 95% CIs
├── data/
│   ├── README.md                  # Download instructions and codebook reference
│   ├── cardio_train_sample.csv    # Benchmark Kaggle data sample
│   └── brfss2015_sample.csv       # Benchmark CDC BRFSS 2015 data sample
└── README.md                      # Complete system documentation
```

---

## 3. Setup Instructions (RStudio & Posit Cloud)

### Prerequisites

Ensure you have R (>= 4.2.0) installed.

### Step 1: Install Required R Packages

Execute the following in the R console:

```r
required_packages <- c(
  "shiny",
  "bslib",
  "plotly",
  "dplyr",
  "tidyr",
  "readr",
  "haven",
  "randomForest",
  "e1071",
  "caret",
  "pROC",
  "DT"
)

new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages) > 0) {
  install.packages(new_packages, dependencies = TRUE)
}
```

### Step 2: Run the Dashboard

In RStudio or Posit Cloud terminal/console, set your working directory to the project folder and launch:

```r
shiny::runApp()
```

Or open `app.R` in RStudio and click **"Run App"** in the top right toolbar.

---

## 4. Dataset Acquisition & Placement

### Dataset A: Kaggle Cardiovascular Disease Dataset
- **URL**: https://www.kaggle.com/datasets/sulianova/cardiovascular-disease-dataset
- Download `cardio_train.csv` and place in `data/cardio_train.csv`.
- Features 70,000 examination records with target `cardio` (0/1). Age is converted from days to years via `age / 365.25`.

### Dataset B: CDC BRFSS 2015
- **URL**: https://www.cdc.gov/brfss/annual_data/annual_2015.html
- Download `LLCP2015.XPT` or exported CSV and place in `data/LLCP2015.XPT` or `data/brfss2015.csv`.
- Uses verified composite outcome: History of Myocardial Infarction (`CVDINFR4`) or Angina / Coronary Heart Disease (`CVDCRHD4`).

---

## 5. Machine Learning Methodology

1. **Partitioning**: 80% training / 20% test split stratified by the cardiovascular target outcome using reproducible random seed (`seed = 42`).
2. **Preprocessing**: Validated physiological plausibility boundaries (Systolic 60–240 mmHg, Diastolic 40–160 mmHg, Systolic > Diastolic). Standardized scaling and transformations are derived solely on training partitions to prevent target leakage.
3. **Model Selection**:
   - **Logistic Regression**: Standard linear log-odds classifier.
   - **Random Forest**: 150 ensemble decision trees evaluating Gini impurity decrease.
   - **Support Vector Machine (SVM)**: Radial basis kernel (RBF) with calibrated probability estimation.
4. **Metric**: Multi-model ROC curves plotted on held-out test set with Area Under the Curve (ROC-AUC) evaluated via trapezoidal numerical integration.

---

## 6. Limitations & Academic Disclaimer

- **Cross-Sectional Observational Nature**: Observed statistical correlations and risk factor patterns do not establish biological or temporal causation.
- **Self-Reported Survey Data**: BRFSS responses are subject to recall bias, social desirability bias, and non-response attrition.
- **Scope of Prediction**: Models predict the cross-sectional presence or historical occurrence of diagnosed cardiovascular conditions within examined cohorts; they do not predict future longitudinal incidence.
- **Clinical Disclaimer**: CardioPredict is intended solely for academic research, epidemiological visualization, and machine learning education. It is not an FDA-cleared diagnostic device and should never replace qualified clinical judgment.
