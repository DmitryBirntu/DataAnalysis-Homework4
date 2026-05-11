Assignment
This assignment is part of Practical class 4 in the Data Analysis course. The work is based on the dataset `data_for_analysis` and includes correlation analysis, regression analysis, model selection using the Bayesian Information Criterion (BIC), and logistic regression for binary outcome prediction.According to the task statement, 
it is necessary to perform correlation analysis between variables, obtain a table with correlation coefficients and significance assessment using the permutation method, perform regression analysis, select the best model by BIC, and fit logistic regression models using hormone variables to predict the binary outcome.


Software and R version
The analysis was performed in the R environment using the script `practice-4.R`. The exact R version is not specified in the attached materials, so the code is expected to run in a recent standard R 4.x environment.
The practical work may require additional R packages such as `pROC` for ROC and AUC analysis, while permutation significance can be implemented either with package-based tools or with a custom permutation function written in base R.

Procedures used
The following procedures were used in this assignment:

1. Reading the dataset with `read.csv()`.
2. Testing the normality of `lipids1` and `lipids2` using the Shapiro-Wilk test.
3. Constructing histograms and Q-Q plots for `lipids1` and `lipids2` to visually inspect the distribution shape.
4. Performing Spearman correlation analysis between `lipids1` and other lipid variables.
5. Applying a permutation method with 10,000 repetitions to assess the significance of correlation coefficients.
6. Creating a table with correlation coefficients and permutation p-values.
7. Building five regression models for the relationship between `lipids1` and `lipids2`: linear, second-degree polynomial, third-degree polynomial, exponential, and logarithmic.
8. Comparing regression models using BIC and selecting the model with the lowest BIC value as the best one.
9. Building logistic regression models with hormone variables for predicting the binary variable `outcome`.
10. Comparing logistic models using AIC and BIC, generating predicted probabilities, constructing a confusion matrix, plotting the ROC curve, calculating AUC, performing stepwise variable selection, and computing odds ratios with 95% confidence intervals.

Results summary
The practical work produces several analytical outputs. First, normality assessment and graphical inspection are used to justify the choice of Spearman correlation for lipid variables.
Second, a table of correlation coefficients and permutation p-values is created to assess the reliability of observed correlations.[file:115] Third, five regression models are compared using BIC, and the model with the smallest BIC is selected as the most appropriate one for the relationship between `lipids1` and `lipids2`.
In the final part, logistic regression models are fitted to predict the binary outcome using hormone variables.[file:115] Model performance is evaluated using AIC, BIC, confusion matrix, ROC curve, and AUC, while coefficient interpretation is based on odds ratios and 95% confidence intervals.
