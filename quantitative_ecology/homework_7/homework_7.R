# import packages ----------------------------------------------------------------------------------
library(ggplot2)
library(plyr)
library(reshape2)

# Read in data sets --------------------------------------------------------------------------------
emap_data <- read.table("emap.build08.txt", header = TRUE)
val_data_1 <- read.table(file = "emap.val1.08.txt", header = TRUE)
val_data_2 <- read.table(file = "emap.val2.08.txt", header = TRUE)

# Log transform some variables ---------------------------------------------------------------------
log_var <- function(data, var) { # a function that logs the column (var) in the data.frame (data)
  data[var] <- log(data[var] + 1)
  names(data)[names(data) == var] <- paste0('LOG.', var) # add "LOG." to column name
  return(data)
}
emap_data <- log_var(emap_data, "TOT.RD")
val_data_1 <- log_var(val_data_1, "TOT.RD")
val_data_2 <- log_var(val_data_2, "TOT.RD")

# Define model from homework 4 ---------------------------------------------------------------------
model <- lm(formula = SECMEAN ~ AG.TOT + AV.DEP + LOG.TOT.RD + AV.DEP*LOG.TOT.RD,
            data = emap_data)
model_summary <- summary(model)

# Add predicted values to all data sets ------------------------------------------------------------
emap_data$predicted <- predict(model)
val_data_1$predicted <- predict(model, newdata = val_data_1)
val_data_2$predicted <- predict(model, newdata = val_data_2)

# Combine data sets for graphing -------------------------------------------------------------------
data_sets <- list(emap_data, val_data_1, val_data_2)
names(data_sets) <- c("Original", "Validation 1", "Validation 2") #used in graphing
data <- ldply(data_sets, .id = "data_set") #converts a list of data.frames into one data.frame, like rbind

# Calculate statistics -----------------------------------------------------------------------------
mse <- function(observed, predicted, coefficient_count) { # calculates mean square error
  sum((observed - predicted)^2) / (length(observed) - coefficient_count)
}
mspe <- function(observed, predicted) { # calculates mean square prediction error
  sum((observed - predicted)^2) / (length(observed))
}
r2 <- function(observed, predicted, original_mean) { # calculates R-squared
   1 - sum((observed - predicted)^2) / sum((observed - original_mean)^2)
}
#ddply takes a data.frame, splits it based on the value of a factor, applies a function to each peice, and combines the results into a new data.frame
stats <- cbind(ddply(data, "data_set", function(x) mse(x$SECMEAN, x$predicted, 4)), 
               ddply(data, "data_set", function(x) mspe(x$SECMEAN, x$predicted))[2],
               ddply(data, "data_set", function(x) r2(x$SECMEAN, x$predicted, mean(emap_data$SECMEAN)))[2])
names(stats) <- c("data_set", "MSE", "MSPE", "R-squared")
stats <- melt(stats) #changes format of data.frame to one the ggplot2 likes for graphing

# Make statistics graph ----------------------------------------------------------------------------
stats_plot <- ggplot(stats, aes(x = data_set, y = value)) +
  geom_bar(stat = "identity") + 
  facet_wrap( ~ variable, scales = "free") + # splits graph by statistic type
  labs(x = "Data set fitted to original model", y = "Statistic value",
       title = "Model fit statistics for original and validation data sets") +
  theme_classic(base_size = 20) # sets theme and base text size
png(filename = "stats.png", width = 1000, height = 400) # prepares for png output 
print(stats_plot)
dev.off() #saves file and turns off png output

# Make scatter plots -------------------------------------------------------------------------------
scatter_plot <- ggplot(data, aes(x = predicted, y = SECMEAN)) +
  geom_smooth(method = lm) + # adds best fit line (blue)
  geom_point(alpha = .3) + # adds transparent points
  geom_abline(slope = 1) + # adds 1:1 line (black)
  facet_grid( ~ data_set) + # splits graph by "data_set" column in "data"
  labs(x = "Predicted", y = "Observed", title = "Results of internal and external validation") +
  theme_classic(base_size = 20) # sets theme and base text size
png(filename = "scatter.png", width = 1000, height = 350) # prepares for png output 
print(scatter_plot)
dev.off() #saves file and turns off png output
