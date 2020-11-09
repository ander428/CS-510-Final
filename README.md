# Analysis of Kickstarters - CS 510 Midterm

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

## Goal
The goal of the analysis is to determine how to best predict the money pledged to a given kickstarter using various linear methods. Comparing an ordinary linear model, boostrapped linear model, and bayesian linear model, I evaluate several different metrics to determine which method provided the most accurate result.

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

**Bootstrapped OLS**: This used the OLS algorithm within a bootstrap. Both the mean and median aggregate coefficients were evaluated.

**Bayesian Linear Regression**: This was taken from the *rstanarm* package.
