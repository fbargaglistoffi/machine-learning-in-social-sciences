#########################################################################
##                                                                     ##
##                                                                     ##
##               Application 1: Classification Tree                    ##
##                                                                     ##
##                                                                     ##
#########################################################################

# Clean the Workspace
rm(list=ls())

# Set Working Directory
setwd("...")

# Install Packages
#install.packages("haven")
#install.packages("rpart")
#install.packages("pROC")
#install.packages("rpart.plot")
#install.packages("rattle")

# Upload Packages
library(haven)
library(rpart)
library(pROC)
library(rpart.plot)
library(rattle)

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

# Grow an "rpart" tree
set.seed(2020)
dt <- rpart(formula, method = "class", data = train,
            control = rpart.control(minsplit = nrow(train)*0.01, cp=0))

printcp(dt) # display the results
summary(dt) # detailed summary of splits
plotcp(dt) # visualize cross-validation results

# Prune the Tree
pruned <- prune(dt, cp = 0.0077)

# Plot tree
plot(dt, uniform=TRUE, main="Classification Tree")
text(dt, use.n=TRUE, all=TRUE, cex=.8)

# Fancier tree plot
fancyRpartPlot(dt)
fancyRpartPlot(pruned)

# Get predicted values
dt.pred <- predict(dt, newdata = test, type='class')
pruned.pred <- predict(pruned, newdata = test, type='class')

# Generate table that compares true outcomes of the testing set with predicted outcomes of decisiontree
dt_tab = table(true = test$DUMMY_LOW, pred = dt.pred)
pruned_tab = table(true = test$DUMMY_LOW, pred = pruned.pred)
dt_tab
pruned_tab

# Generate ROC object based on predictions in testing set
dt_roc = roc(test$DUMMY_LOW ~ as.double(dt.pred))
pruned_roc = roc(test$DUMMY_LOW ~ as.double(pruned.pred))

# Calculate Area Under the Roc-Curve (AUC) value of predictions in testing set
dt_auc = auc(dt_roc)
pruned_auc = auc(pruned_roc)
cbind(dt_auc, pruned_auc)

# Plot ROC
plot(dt_roc)

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

f1_dt <- f1_score(dt.pred, test$DUMMY_LOW, "1")
f1_pruned <- f1_score(pruned.pred, test$DUMMY_LOW, "1")
cbind(f1_dt, f1_pruned)
