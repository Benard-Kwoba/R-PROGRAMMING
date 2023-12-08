# Install and load necessary packages
# install.packages(c("readr", "ggplot2", "tidyr"))
library(readr)
library(ggplot2)
library(tidyr)

# Read the dataset from "PearEcoDynamics.csv"
pear_data <- read_csv2("PearEcoDynamics.csv")

# Convert commas to dots and change to numeric
pear_data[, c("water", "N", "drymatter")] <- lapply(pear_data[, c("water", "N", "drymatter")], function(x) as.numeric(sub(",", ".", x)))

# Handling non-finite values (e.g., NA or NaN)
# Remove rows with NA in the greenfly column
pear_data <- pear_data[complete.cases(pear_data$greenfly), ]

# Melt the dataset for better visualization
melted_data <- gather(pear_data, key = "variable", value = "value", water, N, drymatter)

# Scatter plot for treatments and greenfly count
ggplot(pear_data, aes(x = trt, y = greenfly)) +
  geom_point(aes(color = trt), position = position_jitter(width = 0.2)) +
  labs(title = "Greenfly Count by Treatment",
       x = "Treatment",
       y = "Greenfly Count") +
  theme_minimal()

# Boxplot for environmental factors and greenfly count
ggplot(melted_data, aes(x = factor(1), y = value, fill = factor(trt))) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free", ncol = 2) +
  labs(title = "Greenfly Count by Environmental Factors",
       x = NULL,
       y = "Greenfly Count") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# STATISTICAL ANALYSIS FOR YOUR INFORMATION
# ...................................................DESCRIPTIVE STATISTICS..............................................

# Summary statistics
summary(pear_data$greenfly)

# Mean
mean(pear_data$greenfly)

# Median
median(pear_data$greenfly)

# Standard deviation
sd(pear_data$greenfly)

# ___________________Group-wise Comparisons:___________________
#Compare the 'greenfly' variable across different levels of another categorical variable (e.g., 'trt').
# ANOVA
anova_model <- aov(greenfly ~ trt, data = pear_data)
summary(anova_model)

# Post-hoc tests (if ANOVA indicates significant differences)
install.packages("TukeyC")
library(TukeyC)
TukeyHSD(anova_model)

# ___________________________Correlation Analysis:_______________
# Explore relationships between 'greenfly' and continuous variables (e.g., 'water', 'N', 'drymatter').
# Pearson correlation
cor(pear_data$greenfly, pear_data$water, use = "complete.obs")

# Scatter plot with regression line
plot(pear_data$water, pear_data$greenfly)
abline(lm(greenfly ~ water, data = pear_data), col = "red")
# __________________________--Regression Analysis:____________________
# Linear regression
lm_model <- lm(greenfly ~ water + N + drymatter, data = pear_data)
summary(lm_model)

#............................................................REGRESSION ANALYSIS......................................
# Since you're interested in understanding the factors influencing the 'greenfly' variable, a multiple linear regression model might be #appropriate. This will allow you to assess the relationship between 'greenfly' and the environmental factors 'water', 'N', and 'drymatter'.

# Read the dataset from "PearEcoDynamics.csv"
pear_data <- read_csv2("PearEcoDynamics.csv")

# Convert commas to dots and change to numeric
pear_data[, c("water", "N", "drymatter")] <- lapply(pear_data[, c("water", "N", "drymatter")], 
function(x) as.numeric(sub(",", ".", x)))

# Handling non-finite values (e.g., NA or NaN)
# Remove rows with NA in the greenfly column
pear_data <- pear_data[complete.cases(pear_data$greenfly), ]

# Perform multiple linear regression
lm_model <- lm(greenfly ~ water + N + drymatter, data = pear_data)
summary(lm_model)

# .....................................................ANALYSIS OF VARIANCE.................................
# To perform multiple group comparisons on your data, we can use analysis of variance (ANOVA) to compare means across different levels of a #categorical variable. Additionally, we can use post-hoc tests to identify specific group differences. Considering the potential issue of #multiple comparisons, adjusting the p-values is important to control the overall Type I error rate.
# using the 'trt' variable for multiple group comparisons:

# Read the dataset from "PearEcoDynamics.csv"
pear_data <- read_csv2("PearEcoDynamics.csv")

# Convert commas to dots and change to numeric
pear_data[, c("water", "N", "drymatter")] <- lapply(pear_data[, c("water", "N", "drymatter")], 
function(x) as.numeric(sub(",", ".", x)))

# Handling non-finite values (e.g., NA or NaN)
# Remove rows with NA in the greenfly column
pear_data <- pear_data[complete.cases(pear_data$greenfly), ]

# Perform ANOVA
anova_model <- aov(greenfly ~ trt, data = pear_data)

# Post-hoc tests with Tukey correction for multiple comparisons
posthoc <- TukeyHSD(anova_model)

# Display ANOVA results
summary(anova_model)

# Display post-hoc test results
posthoc

# .....................VISUALIZATION OF THE RESULTS OF MULTIPLE GROUP COMPARISONS WITH APPROACH JUSTIFICATION.........
# To visualize the results of multiple group comparisons, you can create a plot that displays the means and confidence intervals for each #treatment group. A common choice is a grouped bar plot, where each treatment is represented by a bar, and the height of the bar corresponds to #the mean, with error bars indicating the confidence interval.
# Assuming you have the tukey_results dataframe from the Tukey HSD test
tukey_results <- data.frame(
  Comparison = c("B-A", "C-A", "C-B"),
  Difference = c(-0.8333333, -2.8333333, -2.0000000),
  CI_lower = c(-2.916834, -4.916834, -4.083501),
  CI_upper = c(1.25016739, -0.74983261, 0.08350072),
  p_adj = c(0.5935563, 0.0058183, 0.0619890)
)

# Create a grouped bar plot
library(ggplot2)

ggplot(tukey_results, aes(x = Comparison, y = Difference, fill = p_adj < 0.05)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), position = position_dodge(0.9), width = 0.2) +
  labs(x = "Treatment Comparison", y = "Mean Difference in Greenfly") +
  ggtitle("Multiple Group Comparisons of Greenfly Means") +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "lightblue")) +
  theme_minimal()
