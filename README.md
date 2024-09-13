# Analysis of Factors Affecting the Value of High-Tech Product Exports from Poland

## Project Objective
The goal of this project is to analyze the factors that influence the rising value of high-tech product exports from Poland, with a particular focus on economic, employment, and R&D variables. The dataset includes observations from Eurostat for the years 2013-2023, covering various economic indicators such as firm numbers, employment rates, and GDP contributions.

## Tested Models
Several econometric models were developed and tested to identify the relationship between export values and potential influencing factors:
- **Model 1 (OLS Regression)**: Tested all variables, but none were statistically significant. Despite a high R² (99.74%), the model showed signs of overfitting.
- **Model 2 (OLS after Hellwig Method selection)**: Narrowed down key variables (X2, X4, X6, X9, X10), further analysis led to the selection of X3 (R&D expenditure) and X10 (total exports), yielding statistically significant results with no issues of autocorrelation or multicollinearity.

## Conclusion
The analysis revealed that R&D expenditure and the value of total exports are significant drivers of high-tech product exports from Poland. The final model explains 98.45% of the variance in the dependent variable, demonstrating strong predictive power.

