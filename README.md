# Prediction of Myocardial Rupture

This repository contains an analysis and implementation of prediction methods to forecast the occurrence of myocardial rupture (RAZRIV) in patients following myocardial infarction based on clinical data collected within the first 48 hours of hospitalization.

---

## Overview

Myocardial rupture is a critical complication of myocardial infarction, responsible for up to 15% of hospital deaths following heart attacks. This project aims to:
1. Predict the occurrence of myocardial rupture using patient data collected within the first 48 hours after admission.
2. Evaluate the performance of classification methods (k-NN and Naïve Bayes) under different preprocessing, sampling, and scaling strategies.

### Key Features
- Dataset: Collected at Krasnoyarsk Interdistrict Clinical Hospital (Russia) between 1992 and 1995.
- Total Observations: 1700
- Total Features: 124 (Reduced to 112 post-preprocessing)
- Target Variable: `RAZRIV` (Binary outcome: 0 = No rupture, 1 = Rupture)

---

## Data and Preprocessing

### Preprocessing Steps
1. **Missing Value Treatment**:
   - Features with >20% missing values were removed.
   - Remaining missing values were imputed based on feature types:
     - **Binary Features**: Mode
     - **Ordinal/Partially Ordered Features**: Median
     - **Real Features**: Mean

2. **Class Imbalance**:
   - Outcomes were highly imbalanced (96.82% No Rupture vs. 3.18% Rupture).
   - Three strategies were implemented:
     - No sampling
     - Oversampling (minority class)
     - Undersampling (majority class)

3. **Feature Scaling**:
   - Min-Max scaling was applied where appropriate for k-NN classification.

---

## Classification Methods

### 1. k-Nearest Neighbours (k-NN)
- Explores the relationship between patient features and the likelihood of myocardial rupture.
- Parameters optimized:
  - Number of Neighbours (\( k \)): 1–20
  - Distance Metrics: Manhattan, Euclidean
  - Sampling Strategies: No sampling, Oversampling, Undersampling
- Performance Metric: Area Under the Curve (AUC) of the Receiver Operating Characteristic (ROC).

### 2. Naïve Bayes Classifier
- Uses probabilistic methods under the assumption of feature independence.
- Parameters optimized:
  - Kernel Density Estimation: TRUE/FALSE
  - Laplace Smoothing: 0, 0.5, 1
  - Bandwidth Adjustment: 0.01–1
  - Sampling Strategies: No sampling, Oversampling, Undersampling
- Performance Metric: AUC of the ROC.

---

## Results

### Key Findings
- k-NN performed better than Naïve Bayes across all scenarios.
- Down-sampling yielded the highest AUC values for both classifiers, highlighting the effectiveness of addressing the class imbalance.

| **Method**      | **Sampling**   | **Scaling** | **Best AUC** |
|------------------|----------------|-------------|--------------|
| k-NN            | Down-sampling | Min-Max     | 0.82         |
| Naïve Bayes     | Down-sampling | None        | 0.87         |

---

## Technologies
- **Programming Language**: R
- **Libraries**:
  - `caret` (Classification and Training)
  - `ggplot2` (Visualization)
  - `naivebayes` (Naïve Bayes)
  - `kknn` (k-NN Implementation)

---

## Usage

1. **Clone the repository**:
   ```bash
   git clone https://github.com/anibalpires4/Prediction-of-Myocardial-Rupture.git
