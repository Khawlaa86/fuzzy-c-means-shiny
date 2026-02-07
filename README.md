# Fuzzy C-Means Clustering – Shiny Application

This Shiny application implements **Fuzzy C-Means clustering** for customer segmentation using
Annual Income and Spending Score.

## Features
- Interactive selection of:
  - Number of clusters
  - Fuzziness parameter (m)
- Visualization of fuzzy clusters
- Cluster centers (denormalized)
- Membership probabilities per customer
- Fuzzy Cross-Entropy evaluation metric

## Technologies
- R
- Shiny / Shinydashboard
- e1071 (Fuzzy C-Means)
- ggplot2 + Plotly
- dplyr

## Dataset
Mall Customers dataset:
- `annual_income`
- `spending_score`

## How to Run
```r
install.packages(c("shiny", "shinydashboard", "e1071", "ggplot2",
                   "dplyr", "readr", "plotly"))
shiny::runApp()
