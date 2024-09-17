library(palmerpenguins)
library(tidyverse)
library(randomForest)
library(iml)

# Read in Palmer Penguin data
data("penguins")

# Remove NAs
penguins <- penguins %>%
  drop_na()

# Training dataset
# Remove year and island from the dataframe
penguins_train <- penguins %>%
  select(species, bill_length_mm, bill_depth_mm, 
         flipper_length_mm, flipper_length_mm, body_mass_g, sex)


# build a random forest
penguin_rf <- randomForest(species ~ ., penguins_train)

# view the random forest object
penguin_rf


## Set up iml
# Create a dataframe without the species variable (which in this case is the target)
X <- penguins_train[which(names(penguins_train) != "species")]


# Create a new predictor object
# Store the data (X) and target (Species) in the predictor container along with the randomForest parameters
predictor <- Predictor$new(penguin_rf, data = X,
                           y = penguins_train$species,
                           type = "prob")

# PDP object
pdp <- FeatureEffect$new(predictor, 
                         feature="bill_length_mm", center.at=mean(X$bill_length_mm), 
                         method="pdp")

# ICE object
ice <- FeatureEffect$new(predictor, 
                         feature="bill_length_mm", center.at=mean(X$bill_length_mm), 
                         method="ice")

# Plot the ICE object
ice$plot()


## This contains all the individual plots
## **This is a key step**
pdp_res <- pdp$results
ice_res <- ice$results


## Get Island from original (filtered) data and put it in the new ice_res dataframe
## How does this connect to the .id numbers?

ice_res$island <- penguins$island[ice_res$.id]
ice_res$sex <- penguins$sex[ice_res$.id]



## And plot
ggplot(ice_res, aes(x = bill_length_mm, y = .value, group = .id, col = island)) +
  geom_line() +
  facet_wrap(~.class)

ggplot(ice_res, aes(x = bill_length_mm, y = .value, group = .id, col = sex)) +
  geom_line() +
  facet_wrap(~.class)

ggplot(ice_res, aes(x = bill_length_mm, y = .value)) +
  geom_line(aes(group = .id, col = sex), alpha = 0.5) +
  facet_wrap(~.class) +
  geom_line(data = pdp_res, size = 2)
