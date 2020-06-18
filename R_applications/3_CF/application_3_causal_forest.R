#########################################################################
##                                                                     ##
##                                                                     ##
##                  Application 3: Causal Forest                       ##
##                                                                     ##
##                                                                     ##
#########################################################################


# Clean the Workspace
rm(list=ls())

# Set Working Directory
setwd("...")

# Install Packages
#install.packages("haven")
#install_github("grf")
#install.packages("ggplot2")



# Upload Packages
library(haven)
library(grf)
library(ggplot2)


# Upload dta file
gok_data <- read_dta("GOK_data.dta")

# Navigate Data
summary(gok_data)
table(gok_data$treatment)

# Select Predictors and Build the Formulae
variables <- c("PA", "ST", "TNN", "Opl", "thuisloos","trekkend", "change_school",
                "primary_retention", "man", "BULO", "GONschool","leerkracht_fulltime",
                "leerkracht_diploma", "leerkracht_age", "leerkracht_seniority",
                "leerkracht_female", "directie_age", "leerkracht_unexperienced0_10",
                "directie_unexperienced0_10", "directie_seniority")
X <-as.data.frame(as.matrix(gok_data[variables]))
c.forest <- causal_forest(X, as.vector(gok_data$progress_school), as.vector(gok_data$treatment))

# Predict using the forest
c.pred <- predict(c.forest, X)
ATE <- mean(c.pred$predictions)
ATE

# Predict with confidence intervals; growing more trees is now recommended
c.forest <- causal_forest(X, as.vector(gok_data$progress_school), as.vector(gok_data$treatment), num.trees = 4000)
c.pred <- predict(c.forest, X, estimate.variance = TRUE)

# Function to plot
plot_het_eff <- function(cf_preds, ci = FALSE, z = 1.96) {
  if (is.null(cf_preds$predictions) || nrow(matrix(cf_preds$predictions)) == 0)
    stop("cf_preds must include a matrix called 'predictions'")
  
  out <- ggplot(
    mapping = aes(
      x = rank(cf_preds$predictions), 
      y = cf_preds$predictions
    )
  ) +
    geom_point() +
    labs(x = "Rank", y = "Estimated Treatment Effect") +
    theme_light()
  
  if (ci && nrow(matrix(cf_preds$variance.estimates)) > 0) {
    out <- out +
      geom_errorbar(
        mapping = aes(
          ymin = cf_preds$predictions + z * sqrt(cf_preds$variance.estimates),
          ymax = cf_preds$predictions - z * sqrt(cf_preds$variance.estimates), alpha = 0.15
        )
      )
  }
  
  return(out)
}

# Plot the Forest
plot <- plot_het_eff(c.pred, ci = TRUE)
plot  + geom_hline(yintercept=0, linetype="dashed", 
                   color = "red", size=1)


