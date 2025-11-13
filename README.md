
# 📊 Credit Balance Prediction Project: Advanced Regression Workflow

This repository presents a regression modeling workflow implemented in **R**. It covers the full process from data ingestion and preprocessing to exploratory analysis, feature engineering, model selection, diagnostics, and final prediction generation. 

---

## 🎯 Project Objective

The primary objective is to build a robust regression model to predict **credit card balances**. By applying advanced techniques such as variable transformation, multicollinearity checks, stepwise model selection, cross-validation, and outlier handling, this workflow maximizes predictive performance and ensures interpretability for real-world decision-making.

---

## 📁 Repository Contents

```
---

## 🛠️ End-to-End Workflow

### 1. Data Understanding & Preprocessing

* Loaded and cleaned the dataset
* Conducted exploratory data analysis (EDA) including:
  * Summary statistics
  * Histograms and density plots
  * Q-Q plots for normality
  * Correlation analysis
* Performed preprocessing:
  * Removed irrelevant columns (`X` index)
  * One-hot encoding for categorical variables (`gender`, `student`, `married`, `ethnicity`)
  * Skewness analysis and variable transformations (`log`, `sqrt`)
  * Multicollinearity check using **VIF**
  * Outlier and influential point detection (standardized residuals, Cook’s distance, leverage)

### 2. Model Development

* Tested multiple regression formulations:
  * Original balance
  * Square root of balance
  * Log-transformed balance
* Stepwise model selection using **AIC** and **BIC**
* Evaluated residuals for:
  * Homoscedasticity
  * Normality
  * Independence
* Box-Cox transformation for potential variance stabilization
* Refined predictor selection based on statistical significance and multicollinearity

### 3. Model Evaluation & Validation

* Conducted **10-fold cross-validation** for robust RMSE estimates
* Compared models with and without:
  * Outliers
  * `is_married` variable
* Selected the final model based on:
  * Lowest CV RMSE
  * Adequate residual diagnostics
  * Adjusted R² and predictive interpretability

### 4. Final Model

* Formula:  
  `balance ~ rating + income_sqrt + is_student + age`
* Key insights:
  * Credit rating strongly predicts balance
  * Student status increases balance
  * Income transformed via square root stabilizes variance
  * Age has a small negative effect

---

## 📈 Key Visualizations

* Histograms and density plots for skewness detection
* Q-Q plots for normality assessment
* Residuals vs fitted values plots for diagnostics
* Cook’s distance and leverage plots for outlier detection
* Stepwise model comparison tables (AIC, BIC, R²)

---

## 🚀 Getting Started

### Step 1: Clone the Repository

```bash
git clone https://github.com/yourusername/credit-balance-regression.git
cd credit-balance-regression
```


## 📦 Dependencies

Ensure the following libraries are installed:

```
library(dplyr)      # Data manipulation
library(ggplot2)    # Data visualization
library(janitor)    # Data cleaning
library(car)        # VIF analysis
library(caret)      # Cross-validation
library(MASS)       # Stepwise regression
library(moments)    # Skewness calculation
library(purrr)      # Functional programming
```

---

## 💻 Author

**Salamot Fakoya**
Data Science (3rd Year | UTDallas)
[LinkedIn](https://www.linkedin.com/in/salamot-fakoya-650325224/) • [GitHub](https://github.com/Sal-Fakoya)

---

## 📃 License

This project is licensed for academic and non-commercial educational use only.

---

