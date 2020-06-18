#########################################################################
##                                                                     ##
##                                                                     ##
##                  Application Extra: Causal Tree                     ##
##                                                                     ##
##                                                                     ##
#########################################################################


# Clean the Workspace
rm(list=ls())

# Set Working Directory
setwd("...")

# Install Packages
#install.packages("haven")
#install.packages("devtools")
#install_github("susanathey/causalTree")
#install.packages("chron")


# Upload Packages
library(haven)
library(causalTree)
library(rpart.plot)
library(chron)


# Upload dta file
gok_data <- read_dta("GOK_data.dta")

# Navigate Data
summary(gok_data)
table(gok_data$treatment)

# Select Predictors and Build the Formulae
predictors <- c("PA", "ST", "TNN", "Opl", "thuisloos","trekkend", "change_school",
                "primary_retention", "man", "BULO", "GONschool","leerkracht_fulltime",
                "leerkracht_diploma", "leerkracht_age", "leerkracht_seniority",
                "leerkracht_female", "directie_age", "leerkracht_unexperienced0_10",
                "directie_unexperienced0_10", "directie_seniority")
formula_progress_school <- as.formula(paste("progress_school ~", paste(predictors, collapse="+")))
print(formula_progress_school)
formula_certificate <- as.formula(paste("certificate ~", paste(predictors, collapse="+")))
print(formula_certificate)


# Causal Tree for Outcome Progress School
tree_progress <- causalTree(formula_progress_school,
                     data = gok_data, treatment = gok_data$treatment,
                     split.Rule = "CT", cv.option = "CT",
                     split.Honest = T, cv.Honest = T,
                     cp = 0, minsize = 20, propensity = 0.5)

# Plot Causal Tree
rpart.plot(tree_progress, cex=1.05,  box.palette="GnBu",
           branch.lty=1, shadow.col="gray",
           nn=TRUE, main="Causal Tree Progress School", prefix="ATE\n")


# Causal Tree for Outcome Certificate
tree_certificate <- causalTree(formula_certificate,
                            data = gok_data, treatment = gok_data$treatment,
                            split.Rule = "CT", cv.option = "CT",
                            split.Honest = T, cv.Honest = T,
                            cp = 0, minsize = 20, propensity = 0.5)

# Plot Causal Tree
rpart.plot(tree_certificate, cex=1.05,  box.palette="GnBu",
           branch.lty=1, shadow.col="gray",
           nn=TRUE, main="Causal Tree Progress School", prefix="ATE\n")

