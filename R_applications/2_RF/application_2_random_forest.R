#########################################################################
##                                                                     ##
##                                                                     ##
##                   Application 2: Random Forest                      ##
##                                                                     ##
##                                                                     ##
#########################################################################

# Clean the Workspace
rm(list=ls())

# Set Working Directory
setwd("...")

# Install Packages
#install.packages("haven")
#install.packages("randomForest")
#install.packages("pROC")
#install.packages("ggplot2")
#install.packages("pdp")
#install.packages("vip")

# Upload Packages
library(haven)
library(randomForest)
library(pROC)
library(ggplot2)
library(pdp)           # for partial dependence plots
library(vip)           # for variable importance plots


# Upload dta file
data <- read_dta("PISA_LOW_FLS.dta")

# Select the Variables 
whichvars = c("DUMMY_LOW", # Response variable
              "AGE", "BELANGNdum", # Background
              "HEDRES","WEALTH","ST013Q01TA","IMMIG", "MISCED","FISCED","BMMJ1","BFMJ2","EMOSUPS", # SES
              "REPEAT","OUTHOURS","MMINS","LMINS","PV1MATH","PV1READ","ANXTEST","MOTIVAT") # Ability
student_level_data <- data[whichvars]

# Depict Data
summary(student_level_data)
table(student_level_data$DUMMY_LOW)

# Divide Data into Training and Testing Data
set.seed(2020)
index <- sample(nrow(student_level_data), size = nrow(student_level_data)*0.5, replace = FALSE)
train <- student_level_data[index,]
test <- student_level_data[-index,]

# Select Predictors and Build the Formula
predictors <- c("AGE", "BELANGNdum", "HEDRES","WEALTH",
                "ST013Q01TA","IMMIG", "MISCED","FISCED",
                "BMMJ1","BFMJ2","EMOSUPS", "REPEAT",
                "OUTHOURS","MMINS","LMINS","PV1MATH",
                "PV1READ","ANXTEST","MOTIVAT")
formula <- as.formula(paste("as.factor(DUMMY_LOW) ~", paste(predictors, collapse="+")))
print(formula)


# Random forest with the randomForest package
set.seed(2020)
obj_rf <- randomForest(formula, data = train,
                       mtry=8, ntree=500, importance=TRUE)
obj_rf

# Variables Importance Plot
varImpPlot(obj_rf)
vip(obj_rf,  bar = FALSE, horizontal = FALSE)

# Partial Dependecy Plot
partial(obj_rf, pred.var="PV1MATH", plot = TRUE, rug = TRUE,
            plot.engine = "ggplot2")

# 3D Partial Dependency Plot
pd <- partial(obj_rf, pred.var = c("PV1MATH", "PV1READ")) # Compute partial dependence data
plotPartial(pd, levelplot = FALSE, zlab = "FLS", colorkey = TRUE, 
                    screen = list(z = -30, x = -60))

# Random forest for Test Data
rf <- randomForest(formula, data = train,
                       ytest = as.factor(test$DUMMY_LOW), xtest = test[predictors], 
                       mtry=8, ntree=500, importance=TRUE)

# Generate table that compares true outcomes of the testing set with predicted outcomes of random forest
rf_tab= table(true = test$DUMMY_LOW, pred = rf$test$predicted)
rf_tab

# Generate ROC object based on predictions in testing set
rf_roc=roc(test$DUMMY_LOW ~ rf$test$votes[,2])

# Calculate AUC value of predictions in testing set
rf_auc=pROC::auc(rf_roc)
rf_auc

# Plot ROC
plot(rf_roc)

# Alternative (Correct!) Performance Measure (F-1 Score)
f1_score <- function(predicted, expected, positive.class) {
  cm = as.matrix(table(expected, predicted))
  
  precision <- diag(cm) / colSums(cm)
  recall <- diag(cm) / rowSums(cm)
  f1 <-  ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))
  f1 <- f1[positive.class]
  
  #Assuming that F1 is zero when it's not possible compute it
  f1[is.na(f1)] <- 0
  
  return(f1)
}

f1_rf <- f1_score(rf$test$predicted, test$DUMMY_LOW, "1")
f1_rf
