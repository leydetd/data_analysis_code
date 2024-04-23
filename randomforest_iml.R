# General Random Forest, iml objects, and Surrogate Model Script
# Dave Leydet
# April 23, 2024




setwd("~/Desktop/University of Utah PhD /Research/r_code/")

library(tidyverse)

# load cleaned data
x = read.csv("filepath")

# check
str(x)

# remove unnecessary variables from df
x = x %>% 
  select(-variable_names)

# factor variables as needed
x$variable_name1 = as.factor(x$variable_name1)
x$variable_name2 = as.factor(x$variable_name2)

# check
str(x)

# z score the target/dependent variable (*as needed*)
x = x %>% 
  mutate(zscore = scale(target_variable, center = TRUE, scale = TRUE))

# Check
str(x)


# Random Forest Run

# load libraries
library(randomForest) #rf package
library(iml) #interpretable machine learning package

# subset the variables we want to use for this model
# remove zscore and adm1 (adm1 has too many factors)
x.rf.df = x %>% 
  select(-variable_names)

# check
str(x.rf.df)


st = Sys.time()

# Train the random forest 
set.seed(77)
rf = randomForest(target_variable ~ .,
                  data = x.rf.df)

et = Sys.time()
elapsed = et-st
print(elapsed)

##pseudo r2 value
mean(rf$rsq)


## iml Predictor Objects

## Create a data frame with the features minus the target
df = x.rf.df %>% 
  select(-con)

# Store the data and con in the predictor container along with the randomForest parameters
predictor = Predictor$new(model = rf,
                                  data = df,
                                  y = x.rf.df$target_variable)

## Store the features in a FeatureImp object
## loss argument specifies the performance measure for error
importance = FeatureImp$new(predictor = predictor,
                            loss = "rmse")

##Plot
plot(importance)

## View the the feature importance percentiles
importance$results


## Reminder this allows us to interpret areas of the curve that may be more important than others
##grid.size argument is the number of quantiles specified

ale = FeatureEffect$new(predictor = predictor,
                        feature = "variable_of_interest",
                        grid.size = 10)

ale$plot()

##Plot all of the feature effects at once

effs = FeatureEffects$new(predictor = predictor,
                          grid.size = 10)

# Visualize
plot(effs)

#PDPs
rf.variable_of_interest = Partial$new(predictor = adm.imp.predictor,
                     feature = "variable_of_interest",
                     aggregation = "pdp",
                     ice = TRUE)

# center (this centers the impact of y hat on a starting value)
rf.variable_of_interest$center(min(x.rf.df$variable_of_interest))

p1 = plot(rf.variable_of_interest) + ggtitle("PDP/ICE Plot")

p1


st = Sys.time()
##Set up the interactions wrapper
##Play around with grid.size

interact = Interaction$new(predictor = predictor,
                               grid.size = 15)

et = Sys.time()
print(et-st)

##Plot the features to see how the interact with any other feature in the data
# scale 0 - 1 with 1 meaning that 100% of the variance is explained with interactions with the other features
plot(interact)


# Surrogate Model

tree = TreeSurrogate$new(predictor = predictor,
                         maxdepth = 2)


# Visualize
plot(tree)


# Visualize Simplified version
plot(tree$tree)


## Check the model performance 

print(tree$r.squared)


## **END**













