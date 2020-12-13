## ----setup, include=FALSE----------------------------------------------------------------------------------------------
packages <- c("tidyverse", "rstanarm", "data.table",
             "yardstick", "ggplot2", "ggridges", "knitr")

# conditionally load packages that are not installed
setup.env <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE, repo = "http://cran.us.r-project.org")
      library(x, character.only = TRUE)
    }
  }
)

setup.env


## ----options,include=FALSE---------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = T, include=F, warning = FALSE)


## ----define_functions--------------------------------------------------------------------------------------------------
options(scipen = 999)
set.seed(1818)

### define functions ###
# create linear regression algorithm using linear algebra
linear_regression <- function(f, df) {
  y <- model.frame(f, data=df)[,1]
  X <- model.matrix(f, data = df)
  
  # Calculate Coefficients
  # Betas = (X^T * X)^-1 * X^T * y
  Betas <- solve(t(X) %*% X) %*% t(X) %*% y
  
  Betas
}

# reformat regression coefs for easy view
lm_summary <- function(lm_mod) {
  summary_df <- data.frame(
    mean = sapply(lm_mod, mean),
    median = sapply(lm_mod, median),
    sd = sapply(lm_mod, sd),
    row.names = names(lm_mod))
  summary_df
}

# make linear regression predictions using coefs
# type 0 == classical
# type 1 == sampling method
predict_lm <- function(lm_mod, newdata, type) {
  preds <- NULL
  
  if(type == 0) {
    X <- newdata %>% select(rownames(lm_mod)[-1]) # remove intercept
    
    for(i in 1:nrow(X)) {
      preds[i] <- lm_mod[1]
      
      for(j in 1:ncol(X)) {
        preds[i] <- preds[i] + (lm_mod[j+1] * X[i,j])
      }
    }
  }
  
  # sampling
  else if(type == 1) {
    X <- newdata %>% select(rownames(lm_mod[-1,])) # remove intercept
    
    for(i in 1:nrow(X)) {
      preds$mean[i] <- lm_mod[1,1] # mean intercept
      preds$median[i] <- lm_mod[1,2] # median intercept
      
      for(j in 1:ncol(X)) {
        preds$mean[i] <- preds$mean[i] + (lm_mod[j+1, 1] * X[i,j])
        preds$median[i] <- preds$median[i] + (lm_mod[j+1, 2] * X[i,j])
      }
    }
  }
  
  else {
    print("invalid type")
  }
  preds
}



## ----data_cleaning-----------------------------------------------------------------------------------------------------
### data cleaning ###

kickstarters <- fread("datasets/ks-projects-201801.csv")

# view data frame structure
kickstarters %>% str()

# save dimensions for original data
d_kickstarters <- dim(kickstarters)

kickstarters_clean <- kickstarters %>%
  # create length variable
  mutate(length = as.numeric(as.Date(deadline)-as.Date(launched))) %>%
  # remove unneeded variables
  select_if(is.numeric) %>%
  select(-c(ID, goal, pledged, `usd pledged`))

# save dimensions for original data
d_kickstarters_clean <- dim(kickstarters_clean)

### Initial Visualizations ###
backers_timeseries <- ggplot(kickstarters, aes(as.Date(deadline),backers, color=fct_lump(main_category,6))) +
  geom_line() +
  labs(title="Figure 1. Backers by category over time", x="Deadline", y="Backers", color="Main Category") 
backers_timeseries

ggsave("plots/backers_timeseries.png", plot=backers_timeseries)

pledged_density <- ggplot(kickstarters, aes(usd_pledged_real, fct_lump(state,3), fill=fct_lump(state,3))) +
  geom_density_ridges(alpha = 0.4) +
  xlim(0,10000) + # excludes heavy outliers
  labs(title="Figure 2. Density of amount pledged by kickstarter success", 
       x="Amount Pledged (USD $)", y="Success State") +
  guides(fill=FALSE) 

ggsave("plots/pledged_density.jpg", plot=pledged_density)


## ----colnames, include=T, echo=F, results = 'asis'---------------------------------------------------------------------
columns <- colnames(kickstarters_clean)
for (i in 1:length(columns)) {
  cat(paste(columns[i]))
  
  if (i == length(columns)-1) {
    cat(", and ")
  }
  else if (i != length(columns)) {
    cat(", ")
  }
  
}


## ----plot1, include=T, echo=FALSE, message=FALSE-----------------------------------------------------------------------
suppressMessages(print(backers_timeseries))


## ----plot2, include=T, echo=FALSE, warning=FALSE-----------------------------------------------------------------------
suppressMessages(pledged_density)


## ----modeling, cache=TRUE----------------------------------------------------------------------------------------------
### modeling ###

# create regression formula
formula_1 <- usd_pledged_real ~ backers + usd_goal_real + length

# classical regression
lm_mod <- linear_regression(formula_1, kickstarters_clean)

# bootstrap classical linear regression
n_boot <- 1000
n_samples <- 100
mod_boot <- list()
for(i in 1:n_boot) {
  idx <- sample(1:nrow(kickstarters_clean), n_samples)
  mod_temp <- linear_regression(formula_1, kickstarters_clean %>% slice(idx))
  mod_boot[[i]] <- t(mod_temp)
}

mod_boot <- as.data.frame(do.call(rbind, mod_boot))

# bayesian linear regression
bayes_mod <- stan_glm(formula_1, data=kickstarters_clean, seed=1818)
summary(bayes_mod)

rm(kickstarters, mod_temp) # clear memory of uneeded variables

# predictions
setDF(kickstarters_clean) # convert dataset back to dataframe

preds_lm <- predict_lm(lm_mod, kickstarters_clean, type = 0) # classical

boot_coefs <- lm_summary(mod_boot)
preds_boot <- predict_lm(boot_coefs, kickstarters_clean, type = 1) # bootstrap

bayes_coefs <- data.frame(mean = bayes_mod$stan_summary[1:length(kickstarters_clean),1],
                          median = bayes_mod$coefficients)

preds_bayes <- predict_lm(bayes_coefs, kickstarters_clean, type = 1) # bayesian

preds_df <- data.frame(
  classical = preds_lm,
  boot_mean = preds_boot$mean,
  boot_median = preds_boot$median,
  bayes_mean = preds_bayes$mean,
  bayes_median = preds_bayes$median,
  true = kickstarters_clean$usd_pledged_real
)


## ----model_performance-------------------------------------------------------------------------------------------------
### performance metrics ###

m_set <- metric_set(rsq, rmse, mae)

error <- list()
error[[1]] <- data.frame(m_set(kickstarters_clean, preds_df$true, preds_df$classical), method = "Classical")
# A tibble: 3 x 3
#   .metric .estimator .estimate
#   <chr>   <chr>          <dbl>
# 1 rsq     standard       0.566
# 2 rmse    standard   59910.   
# 3 mae     standard    5892.  

error[[2]] <- data.frame(m_set(kickstarters_clean, preds_df$true, preds_df$boot_mean), method = "Boot.Mean")
#   .metric .estimator .estimate
#   <chr>   <chr>          <dbl>
# 1 rsq     standard       0.353
# 2 rmse    standard   85129.   
# 3 mae     standard    7274.  

error[[3]] <- data.frame(m_set(kickstarters_clean, preds_df$true, preds_df$boot_median), method = "Boot.Median")
#   .metric .estimator .estimate
#   <chr>   <chr>          <dbl>
# 1 rsq     standard       0.542
# 2 rmse    standard   61707.   
# 3 mae     standard    5461. 

error[[4]] <- data.frame(m_set(kickstarters_clean, preds_df$true, preds_df$bayes_mean), method = "Bayes.Mean")
# A tibble: 3 x 3
#   .metric .estimator .estimate
#   <chr>   <chr>          <dbl>
# 1 rsq     standard       0.566
# 2 rmse    standard   59910.   
# 3 mae     standard    5893.   

error[[5]] <- data.frame(m_set(kickstarters_clean, preds_df$true, preds_df$bayes_median), method = "Bayes.Median")
# A tibble: 3 x 3
# .metric .estimator .estimate
#   <chr>   <chr>          <dbl>
# 1 rsq     standard       0.566
# 2 rmse    standard   59910.   
# 3 mae     standard    5893.

# All linear regression methods seemed to produce similar results with the exception of the bootstap model
# that uses the mean coefficients. The bootstrap using median had the edge on all other models
# Interestingly, the standard classical model has nearly identical results to the bayesian model.

### visualizations ###

# error plot
error_df <- data.frame(data.table::rbindlist(error, use.names = TRUE, fill = F, idcol = T))
error_df <- error_df %>% select(-.id)

error_plot <- ggplot(error_df, aes(x=method, y=.estimate, fill=method)) +
  geom_bar(stat = "identity") +
  facet_wrap(~.metric, scales = "free") +
  labs(title = "Performance Metrics by Model") +
  guides(fill=FALSE) +
  theme(axis.text.x = element_text(angle = 90))
error_plot

ggsave("plots/Error_Plot.png", plot=error_plot)


## ----plot3, include=T, echo=FALSE--------------------------------------------------------------------------------------
error_plot

