# Analysis of Kickstarters - CS 510 Midterm
## Goal
The goal of the analysis is to determine how to best predict the money pledged to a given kickstarter using various linear methods. Comparing an ordinary linear model, boostrapped linear model, and bayesian linear model, I evaluate several different metrics to determine which method provided the most accurate result. The final draft of the analysis is stored in the [**midterm_complete.R**](https://github.com/ander428/CS-510-Midterm/blob/main/midterm_complete.R) file. The midterm_first_draft.R file is stored to document the project before initial feedback.

## Data
This project is for Computing for Scientists (CS 510) at Chapman University. The dataset used is [kickstarters over the course of 2018](https://www.kaggle.com/kemical/kickstarter-projects) containing various information about the kickstarters' goals, category, etc. Here is a brief data dictionary:

- **Name**: name of the kickstarter
- **Category**": a specific/narrow category the kickstarter falls under
- **Main Category**: the most general category that the kickstarter falls under
- **deadline**: the deadline date to meet the kickstarter goal
- **state**: results of the kickstarter, i.e., whether it met its goal or not
- **backers**: number of people who have contributed
- **Currency**: currency used by backers to fund kickstarter
- **Country**: the country where the kickstarter is based
- **usd pledge real**: conversion of pledged in US dollars of the pledged column (conversion from Fixer.io API).
- **usd goal real**: conversion in US dollars of the goal column (conversion from Fixer.io API).

## Analysis
### Required packages
The packages used for this project include *tidyverse, rstanarm, yardstick, ggplot2, ggridges, and data.table* which can be installed by running the following:

```r
install.packages("tidyverse")
install.packages("rstanarm")
install.packages("yardstick")
install.packages("ggplot2")
install.packages("data.table")
install.packages("ggridges")
```

### Algorithms
**Ordinary Linear Regression (OLS)**: This was coded from scratch using the following formula: 
<img src="https://github.com/ander428/CS-510-Midterm/blob/main/plots/render.png"/>

**Bootstrapped OLS**: This used the OLS algorithm within a bootstrap. Both the mean and median aggregate coefficients were evaluated.

**Bayesian Linear Regression**: This was taken from the *rstanarm* package.

### Results
<img src="https://github.com/ander428/CS-510-Midterm/blob/main/plots/Error_Plot.png"/>

This graph shows the result of the performance metrics. Using the bootstrap model with the mean aggregate coefficients is clearly the worst model having the lowest r-squared while having the highest error.

The best model seems to be subjective between the OLS model and the bootstrap model with the median aggregate depending if mean absolute error or root mean squared error is a more appropriate metric. For this dataset, RMSE seems to be the more appropriate metric as there is high variance. This leads me to declare the bootstrap model the better model. 

Both bayesian models resulted in nearly the same exact numbers as the OSL model in every metric. This means that the bayesian model is not a better option for analysis as it is much more computationally heavy, yet yeilds very similar results.
