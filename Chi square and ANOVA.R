install.packages("data.table")   #install data.table library
install.packages("plyr")   #install plyr library
install.packages("FSA")   #install FSA library
install.packages("FSAdata")   #install FSA data library
install.packages("magrittr")   #install magrittr library
install.packages("dplyr")   #install dplyr library
install.packages("plotrix")   #install plotrix library
install.packages("ggplot2")   #install ggplot2 library
install.packages("moments")   #install moments library
install.packages("readxl")    #install readxl library
install.packages("psych")     #install psych library
install.packages("corrplot")  #install corrplot library
install.packages("car")       #install car library
install.packages("leaps")    #install leaps library
install.packages("skimr")   #install skimr library
install.packages("tidyr")

library(tidyr)
library(psych)  # import psych library
library(readxl)  # import readxl library
library(plyr)  # import plyr library
library(data.table) #import data.table library
library(FSA)  # import FSA library
library(FSAdata)  # import FSAdata library
library(magrittr)  # import magrittr library
library(dplyr)  # import dplyr library
library(plotrix)  # import plotrix library
library(ggplot2)  # import ggplot2 library
library(moments)  # import moments library
library(corrplot) #import corrplot library
library(car)     #import car library
library(leaps)   #import leaps library
library(skimr)   #import skimr library


# Section 11 -1
# 6.BLOOD TYPES
# H0: A =0.2 ,B=0.28 ,O=0.36 , AB=0.16 
# H1: DISTRIBUTIONS ARE NOT EQUAL
alpha <- 0.10
df <- 3
critical_value <- qchisq(1 - alpha, df)
observed <- c(12, 8, 24, 6)
expected <- c(50 * 0.20, 50 * 0.28, 50 * 0.36, 50 * 0.16)
chi_square <- sum((observed - expected)^2 / expected)
if (chi_square > critical_value) {
  decision <- "Reject H0"
} else {
  decision <- "Fail to reject H0"
}
cat("Critical Value:", critical_value, "\n")
cat("Test Value:", chi_square, "\n")
cat("Decision:", decision, "\n")
# as the p value is greater than 10%, its failing the rejection of null hypothesis.



# 8.On_time performance by airlines
# Step 1: State the hypotheses and identify the claim
# H0: The observed proportions match the government's statistics.
# Ha: The observed proportions do not match the government's statistics.
alpha <- 0.05
# Observed counts
observed <- c(125, 40, 10, 25)
total_observed <- sum(observed)
# Expected proportions from government's statistics
expected_proportions <- c(0.708, 0.120, 0.082, 0.090)
# Expected counts based on expected proportions
expected <- expected_proportions * total_observed
# Degrees of freedom
df <- length(observed) - 1
# Step 2: Find the critical value
critical_value <- qchisq(1 - alpha, df)
# Step 3: Compute the test value (chi-square statistic)
chi_square_statistic <- sum((observed - expected)^2 / expected)

# Step 4: Make the decision
if (chi_square_statistic > critical_value) {
  decision <- "Reject null hypothesis. The results differ from the government's statistics."
} else {
  decision <- "Fail to reject null hypothesis. The results do not differ significantly from the government's statistics."
}
# Step 5: Summarize the results
result <- paste("At a significance level of", alpha, ",", decision)
# Display the summary
cat("Critical Value:", critical_value, "\n")
cat("Chi-Square Statistic:", chi_square_statistic, "\n")
cat("Decision:", decision, "\n")
cat("Summary:", result, "\n")



# section 11-2
#8.ETHNICITY AND MOVIE ATTENDANCE
# H0 : movie attendance dependent on ethinicity
# H1 : movie attendance independent on ethinicity
# Set the significance level
alpha <- 0.05
# Define the data in rows
row1 <- c(724000, 335000, 174000, 107000)
row2 <- c(370000, 292000, 152000, 140000)
# Number of rows
rows <- 2
# Create a matrix with the data
mtrx <- matrix(c(row1, row2), nrow = rows, byrow = TRUE)
# Assign row and column names
rownames(mtrx) <- c(2013, 2014)
colnames(mtrx) <- c("Caucasian", "Hispanic", "African American", "Other")
# Perform the chi-square test
result2 <- chisq.test(mtrx)
# Calculate the critical value
df <- (rows - 1) * (ncol(mtrx) - 1)
critical_value <- qchisq(1 - alpha, df)
# Make the decision based on the p-value
decision <- ifelse(result2$p.value > alpha, "Fail to reject null hypothesis", "Reject null hypothesis")
# Print the results
cat("Critical Value:", critical_value, "\n")
cat("Chi-Square Test Statistic:", result2$statistic, "\n")
cat("P-Value:", result2$p.value, "\n")
cat("Decision:", decision, "\n")
ifelse(result2$p.value>alpha, "fail to reject null hypothesis", " reject null hypothesis")
# as the p value is less than 5 % the null hypotehsis got rejected.



# 10. women in the miltary.
# H0 : relation exist between the rank and armed branch of armed forces.
# H1 : realtion doesnot exist between the armed and ranked branches of armed forces.
# Set the significance level
alpha <- 0.05
# Define the data in rows
row3 <- c(10791, 62491)
row4 <- c(7816, 42750)
row5 <- c(932, 9525)
row6 <- c(11819, 54344)
# Number of rows
rows <- 4
# Create a matrix with the data
mtrx1 <- matrix(c(row3, row4, row5, row6), nrow = rows, byrow = TRUE)
# Assign row and column names
rownames(mtrx1) <- c("ARMY", "NAVY", "MARINE CORPS", "AIRFORCE")
colnames(mtrx1) <- c("officers", "enlisted")
# Perform the chi-square test
result3 <- chisq.test(mtrx1)
# Calculate the critical value
df <- (rows - 1) * (ncol(mtrx1) - 1)
critical_value <- qchisq(1 - alpha, df)
# Make the decision based on the p-value
decision <- ifelse(result3$p.value > alpha, "Fail to reject null hypothesis", "Reject null hypothesis")
# Print the results
cat("Critical Value:", critical_value, "\n")
cat("Chi-Square Test Statistic:", result3$statistic, "\n")
cat("P-Value:", result3$p.value, "\n")
cat("Decision:", decision, "\n")

ifelse(result3$p.value > alpha, "fail to reject null hypothesis", "reject null hypothesis")
# As the p value is very small we are rejectiing the null hypothesis


#section 12-1
# 8. Sodium contents of food
# H0: mean1 = mean2 = mean 3
# H1: diffrenece between means exist

alpha <- 0.05

condiments <- data.frame('sodium' = c(270,130,230,180,80,70,200), 'food' = rep('condiments',7),stringsAsFactors = FALSE)

cereals <- data.frame('sodium'= c(260,220,290,290,200,320,140), 'food' = rep('cereals', 7), stringsAsFactors = FALSE)

desserts <- data.frame('sodium' = c(100,180,250,250,300,360,300,160), 'food' = rep('desserts',8), stringsAsFactors = FALSE)

sodium <- rbind(condiments,cereals,desserts)
sodium$food <- as.factor(sodium$food)

anova <- aov(sodium ~ food, data = sodium)

anova

summary(anova)

a.summary <- summary(anova)

p.value <- a.summary[[1]][[1,"Pr(>F)"]]
p.value

ifelse(p.value>alpha, "fail to reject null hypothesis", "reject null hypothesis")


# section 12-2
# 10. sales for leading comapnies
# H0: mean1 = mean2 = mean3
# H1: difference between the mean exist.
alpha = 0.01
cereal <- data.frame("sales" = c(578,320,264,249,237), 'food' = rep('cereal', 5), stringsAsFactors = FALSE)
choclatecandy <- data.frame('sales'= c(311,106,109,125,173), 'food'= rep('Choclatecandy',5), stringsAsFactors = FALSE)
coffee <- data.frame('sales' = c(261,185,302,689), 'food' = rep('coffee', 4), stringsAsFactors = FALSE)
sales <- rbind(cereal,choclatecandy, coffee)
sales$food <- as.factor(sales$food)
anova1 <- aov(sales ~ food, data = sales)
anova1
summary(anova1)
b.summary <- summary(anova1)
p.value <- b.summary[[1]][[1,"Pr(>F)"]]
p.value
ifelse(p.value>alpha, "fail to reject null hypothesis", "reject null hypothesis")
TukeyHSD(anova1)



# 12. per-pupil Expenditures
# H0: mean1 = mean2 = mean3
# H1: difference between the mean exist
alpha = 0.05
easternthird <- data.frame("expenditure" = c(4946,5953,6202,7243,6113), "sections" = rep('eastenthird',5),stringsAsFactors = FALSE)
middlethird <- data.frame("expenditure" = c(6149,7451,6000,6479), "sections"= rep("middlethird",4), stringsAsFactors = FALSE)
westernthird <- data.frame("expenditure" = c(5282,8605,6528,6911), "sections" = rep("westernthird",4), stringsAsFactors = FALSE)
expenditure <- rbind(easternthird,middlethird,westernthird)
expenditure$sections <- as.factor(expenditure$sections)
anova2 <- aov(expenditure ~ sections, data = expenditure)
anova2
summary(anova2)
c.summary <- summary(anova2)
p.value <- c.summary[[1]][[1, "Pr(>F)"]]
p.value
ifelse(p.value>alpha, "fail to reject null hypothesis", "reject null hypothesis")
TukeyHSD(anova2)


#section 12-3
# 10 increasing plant growth.

H0<- "There is no interaction between the two factors (grow-light and plant food) o
n plant growth."
H1<- "There is an interaction between the two factors on plant growth."

# Given data
alpha <- 0.05
PlantGrowth <- data.frame(
  Plant_food = c("a1", "a2"),
  Light1 = c("9.2,9.4,8.9", "7.1,7.2,8.5"),
  Light2 = c("8.5,9.2,8.9", "5.5,5.8,7.6"),
  stringsAsFactors = FALSE
)
# Split the "Light" columns and reshape the data
garden <- PlantGrowth %>%
  gather(Light, Inches, Light1:Light2) %>%
  separate_rows(Inches, sep = ",") %>%
  mutate(Inches = as.numeric(Inches))
# Perform two-way ANOVA
anova_result <- aov(Inches ~ Plant_food * Light, data = garden)
# Display ANOVA summary
summary(anova_result)
# Extract p-values for each effect
p_values <- summary(anova_result)[[1]][, "Pr(>F)"]
# Compare p-values to alpha and make decisions
interaction_p_value <- p_values[1]
plant_food_p_value <- p_values[2]
light_p_value <- p_values[3]
interaction_decision <- ifelse(interaction_p_value > alpha, "No interaction observed", "Interaction observed")
plant_food_decision <- ifelse(plant_food_p_value > alpha, "No difference in mean growth concerning plant food", "Difference in mean growth concerning plant food")
light_decision <- ifelse(light_p_value > alpha, "No difference in mean growth concerning light", "Difference in mean growth concerning light")
#Decision
p_value <- interaction_p_value
decision <- ifelse(p_value > alpha, "Reject null hypothesis. The results are statistically significant.", "Fail to reject null hypothesis. The results are not statistically significant.")
decision
# Degrees of freedom
df_interaction <- (length(unique(garden$Plant_food)) - 1) * (length(unique(garden$Light)) - 1)
df_plant_food <- length(unique(garden$Plant_food)) - 1
df_light <- length(unique(garden$Light)) - 1

# Calculate the critical values
critical_value_interaction <- qf(1 - alpha, df_interaction, df_plant_food * df_light)
critical_value_plant_food <- qf(1 - alpha, df_plant_food, df_interaction * df_light)
critical_value_light <- qf(1 - alpha, df_light, df_interaction * df_plant_food)

# Display the critical values
critical_value_interaction
critical_value_plant_food
critical_value_light


# Summarize the results
interaction_decision
plant_food_decision
light_decision
# Optionally, plot the cell means
# Cell means
cellmeans <- c(8.33, 7.58, 9.08, 6.95)
# Create a bar plot
barplot(cellmeans, 
        names.arg = c("Plant Food A - Light 1", "Plant Food A - Light 2", "Plant Food B - Light 1", "Plant Food B - Light 2"),
        main = "Cell Means of Growth",
        xlab = "Cell",
        ylab = "Mean Growth (Inches)",
        col = "blue",
        ylim = c(0, 10)) 



#on your own 
#importing data set
baseball <- read.csv("C:\\Users\\heman\\Downloads\\baseball.csv")
baseball
#Exploratory data analysis
eda=psych::describe(baseball)
eda
summary(baseball)
glimpse(baseball)
skim(baseball)
unique(baseball$Team)
unique(baseball$Year)
unique(baseball$League)
# Scatter Plot: Runs Scored vs Batting Average
plot(baseball$RS, baseball$BA, main = "Runs Scored vs Batting average",
     xlab = "Runs Scored", ylab = "Batting Average",
     pch = 18, col = "black", frame = FALSE)
# Create a new variable for easy-to-understand labels
baseball$Playoff_Status <- ifelse(baseball$Playoffs == 1, "Qualified", "Not Qualified")
# Bar Plot: Number of Playoff Teams by League (2012)
ggplot(baseball, aes(x = League, fill = factor(Playoffs))) +
  geom_bar() +
  labs(title = "Number of Playoff Teams by League (2012)",
       x = "League",
       y = "Count") +
  scale_fill_manual(values = c("0" = "purple", "1" = "orange"),
                    labels = c("0" = "No", "1" = "Yes")) +
  scale_x_discrete(labels = c("AL" = "American League", "NL" = "National League")) +
  theme_minimal()
alpha=0.05
# Find the critical value (α = 0.05) (programmatically).
#Find the critical value (α = 0.05) (programmatically).
critical_value<-critical_value <- qchisq(0.95, df = 3)
critical_value

#  'w' is a column in your 'baseball' dataset
w <- baseball$W

# Extract the test statistic and p-value
test_statistic <- test_value$statistic
p_value <- test_value$p.value

# Print the test statistic and p-value
cat("Chi-Square Test Statistic:", test_statistic, "\n")
cat("P-Value:", p_value, "\n")




if (test_value$p.value > 0.05) {
  print("Do not reject the null hypothesis")
} else {
  print("Reject the null hypothesis")
}




# Two-way ANOVA test for crop_data.csv dataset
#importing crp dataset
crop = read.csv("C:\\Users\\heman\\Downloads\\crop_data.csv")
crop
#Significant level:
alpha=0.05
an2 <- aov(yield ~ fertilizer + density, data= crop)
an2
summary(an2)
an2.summary <- summary(an2)
p.value <- an2.summary[[1]][[1,"Pr(>F)"]]
p.value
ifelse(p.value>alpha, "Fail to reject the null hypothesis", "Reject the null hypothesis")



