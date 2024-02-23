# Import libraries ----
library(PerformanceAnalytics)
library(dplyr)
library(ggplot2)
library(nnet)
library(plotly)
library(tidyr)
library(car)
library(colorspace)
library(corrgram)
library(GGally)
library(corrplot)

# Import the data frame ----
#choose.files()
cancer <- read.csv("C:\\Users\\saran\\OneDrive\\Desktop\\TSoM\\Data Handling\\Final Project\\cancer_reg-1.csv")
## Analyse the data frame
str(cancer)
summary(cancer) 
head(cancer)
## We found as our main target variable death Rate
## Variable analysis


# Tidy the data ----

## Duplicate Handling ----

### We choose Geography (the county's name) as the key column and look for any duplicate 
ifelse(anyDuplicated(cancer$Geography) != 0,
       "Duplicate observations are found!",
       "No Duplicate observations found!");
sum(duplicated(cancer$Geography))
### We found 137 duplicates in the data frame
cancer[(duplicated(cancer$Geography) | duplicated(cancer$Geography, fromLast = T)),]
### After analyzing the duplicates, we found that they have the same data, the only difference is that some duplicates have missing values. And there is no order (sometimes the missing values case come first and other times comes second)
### To deal with the duplicates, first we count how many NA's every row in the data frame has
cancer$na.count <- rowSums(is.na(cancer))
### Then we sorted the df in increasing order based on the NA's count, so the cases with less missing values appear first. That allow us to effecetively use a top to bottom duplicate handling approach.
cancer <- cancer[order(cancer$na.count),]
cancer <- cancer[!duplicated(cancer$Geography),]
### We run this line again to confirm that there are no more duplicates.
ifelse(anyDuplicated(cancer$Geography) != 0,
       "Duplicate observations are found!",
       "No Duplicate observations found!");
### We remove the NA's count column
cancer <- cancer[-(ncol(cancer))]
### We save the changes in the dataframe
write.csv(cancer,"C:\\Users\\saran\\OneDrive\\Desktop\\TSoM\\Data Handling\\Final Project\\cancer_nodup.csv", row.names = F)




## Missing values handling ----
cancer <- read.csv("C:\\Users\\saran\\OneDrive\\Desktop\\TSoM\\Data Handling\\Final Project\\cancer_nodup.csv")

### First we identify the names of the columns with missing values
na_columns <- colnames(cancer)[colSums(is.na(cancer)) > 0]
for(i in na_columns){
    cat(i, "have", colSums(is.na(cancer[i])), "missing values.\n")
}
### We found that 5 columns have missing values ("deathRate" (436), "incidenceRate"(82),"PctSomeCol18_24"(2285) ,"PctEmployed16_Over"(152), "PctPrivateCoverageAlone"(169))

### PctPrivateCoverageAlone Regression Model ----
### As this variable is directly related with the other coverage variables, we use a regression model to input the missing values
### First we create the train and test samples
sami <- sort(sample(nrow(cancer), nrow(cancer)*.8))
train <- cancer[sami,]
test <- cancer[-sami,]
### Then we create a linear model
lm_cover <- lm(PctPrivateCoverageAlone ~ PctPrivateCoverage + PctPublicCoverageAlone + PctPublicCoverage, data=train)
summary(lm_cover)
### We use the model to predict values in the test set and compare
test$Predic_Priv <- predict(lm_cover, newdata=test)
test[c("PctPrivateCoverageAlone","Predic_Priv")]
test$predic_var_cover <- ((test$PctPrivateCoverageAlone - test$Predic_Priv) / test$PctPrivateCoverageAlone)
mean(test$predic_var_cover, na.rm = T)
### We obtain a mean error of less than 1% (-.001) 
### Then we use the model to input the missing values
cancer$Predict_Priv <- predict(lm_cover, newdata=cancer)
cancer$PctPrivateCoverageAlone[is.na(cancer$PctPrivateCoverageAlone)] <- cancer$Predict_Priv[is.na(cancer$PctPrivateCoverageAlone)]

### PctEmployed16_Over Regression Model ----
### As this variable is directly related with the other employment, we use a regression model to input the missing values
### Then we create a linear model
str(cancer)
lm_emp <- lm(PctEmployed16_Over ~ PctUnemployed16_Over, data=train)
summary(lm_emp)
### We use the model to predict values in the test set and compare
test$Predic_emp <- predict(lm_emp, newdata=test)
test[c("PctEmployed16_Over","Predic_emp")]
test$predic_var_emp <- ((test$PctEmployed16_Over - test$Predic_emp) / test$PctEmployed16_Over)
mean(test$predic_var_emp, na.rm = T)
### We obtain a mean error of less than 1% (-.009) 
### Then we use the model to input the missing values
cancer$Predict_emp <- predict(lm_cover, newdata=cancer)
cancer$PctEmployed16_Over[is.na(cancer$PctEmployed16_Over)] <- cancer$Predict_emp[is.na(cancer$PctEmployed16_Over)]


### incidenceRate Regression Model ----
########## Put here the reasoning behind and the rest run before
lm_incidence <- lm(incidenceRate ~ PctWhite + PctBlack + PctAsian + PctOtherRace, data = train)
summary(lm_incidence)
### We use the model to predict values in the test set and compare
test$Predic_Inc <- predict(lm_incidence, newdata = test)
test[c("incidenceRate","Predic_Inc")]
test$predic_var_inc <- ((test$incidenceRate - test$Predic_Inc) / test$incidenceRate)
mean(test$predic_var_inc, na.rm = T)
### We obtain a mean error of 1.8% (.018)
### Then we use the model to input the missing values
cancer$Predic_Inc <- predict(lm_incidence, newdata = cancer)
cancer$incidenceRate[is.na(cancer$incidenceRate)] <- cancer$Predic_Inc[is.na(cancer$incidenceRate)]

### deathRate Regression Model ----
########## Put here the reasoning behind and the rest run before
lm_death <- lm(deathRate ~ incidenceRate + medIncome, data = train)
summary(lm_death)
### We use the model to predict values in the test set and compare
test$Predic_Death <- predict(lm_death, newdata = test)
test[c("deathRate", "Predic_Death")]
test$predic_var_death <- ((test$deathRate - test$Predic_Death) / test$deathRate )
mean(test$predic_var_death, na.rm = T)
### We obtain a mean error of 2.1% (.021)
### Then we use the model to input the missing values
cancer$Predic_Death <- predict(lm_death, newdata = cancer)
cancer$deathRate[is.na(cancer$deathRate)] <- cancer$Predic_Death[is.na(cancer$deathRate)]

na_columns <- colnames(cancer)[colSums(is.na(cancer)) > 0]
for(i in na_columns){
    cat(i, "have", colSums(is.na(cancer[i])), "missing values.\n")
}

### we removed some variables we are not going to use and save changes
cancer <- cancer[,-which(names(cancer) %in% c("binnedInc", "PctNoHS18_24", "PctHS18_24", "PctSomeCol18_24", "PctHS25_Over", "Predict_Priv", "Predic_Inc", "Predic_Death"))]
write.csv(cancer,"C:\\Users\\saran\\OneDrive\\Desktop\\TSoM\\Data Handling\\Final Project\\cancer_nomiss.csv", row.names = F)




## Handle Outliers ----
cancer <- read.csv("C:\\Users\\saran\\OneDrive\\Desktop\\TSoM\\Data Handling\\Final Project\\cancer_nomiss.csv")
### We create this function that receives a column and as result creates a flag column wich tells if the value on that row is an outlier or not
columns <- colnames(cancer)[sapply(cancer, is.numeric)]
columns_o <- columns
is_outlier <- function(columns){
    for (col_name in columns){
        out_col <- paste0(col_name, "_isout")
        cancer[,out_col] <<- cancer[,col_name]< quantile(cancer[,col_name],0.25,na.rm=TRUE) - 1.5* IQR(cancer[,col_name],na.rm=TRUE) |
            cancer[,col_name] > quantile(cancer[,col_name], 0.75,na.rm=TRUE) + 1.5* IQR(cancer[,col_name],na.rm=TRUE) 
    }
}
is_outlier(columns)

### We created a function to handle the outliers replacing them with a floor and ceiling approach
### Then the function replace every outlier with the bottom or ceiling
handle_out <- function(columns) {
    for (col_name in columns) {
        out_col <- paste0(col_name, "_isout")
        no_out <- paste0(col_name, "_no.out")
        cancer[, no_out] <<- ifelse(cancer[, out_col] == TRUE,
            ifelse(cancer[, col_name] > mean(cancer[, col_name]),
                quantile(cancer[, col_name], 0.75) + 1.5 * IQR(cancer[, col_name]),
                quantile(cancer[, col_name], 0.25) - 1.5 * IQR(cancer[, col_name])),
            cancer[, col_name])
    }
}
handle_out(columns)
### Finally we remove the flag columns and rename the new columns
isout_columns <- grep("_isout$", names(cancer), value = TRUE)
cancer <- cancer[, !names(cancer) %in% isout_columns]
cancer <- cancer[, !names(cancer) %in% columns_o]
remove_suffix <- function(data) {
    names(data) <- sub("_no.out$", "", names(data))
    return(data)
}
cancer <- remove_suffix(cancer)

## We save changes
write.csv(cancer,"C:\\Users\\saran\\OneDrive\\Desktop\\TSoM\\Data Handling\\Final Project\\cancer_no_out.csv", row.names = F)
cancer <- read.csv("C:\\Users\\saran\\OneDrive\\Desktop\\TSoM\\Data Handling\\Final Project\\cancer_no_out.csv")

# Transform ----
### To transform the geography data first we split the name to separate the state from the county in two columns
### This line was used to replace a single value that had special characters
cancer[910,1] <- "Dona Ana County, New Mexico"
cancer <- separate(cancer, Geography, into = c("County", "State"), sep = ", ", extra = "merge", remove = FALSE)
for(i in 1:nrow(cancer)){
    cancer$Region[i] <- ifelse(cancer$State[[i]] %in% c("Maine", "Pennsylvania", "Rhode Island", "Connecticut", "Massachusetts", "New Hampshire", "Vermont", "New York", "New Jersey"), "Northeast", ifelse(cancer$State[i] %in% c("Iowa", "Missouri", "Minnesota", "Michigan", "Kansas", "North Dakota", "Indiana", "South Dakota", "Ohio", "Wisconsin", "Illinois", "Nebraska"), "Midwest", ifelse(cancer$State[i] %in% c("Louisiana", "Arkansas", "Delaware", "Kentucky", "Florida", "North Carolina", "Mississippi", 'District of Columbia', "Georgia", "South Carolina", "Virginia", "West Virginia", "Maryland", "Alabama", "Tennessee", "Texas", "Oklahoma"), "South", 
ifelse(cancer$State[i] %in% c("Colorado", "Nevada", "Hawaii", "Utah", "Montana", 'Washington', "Idaho", "Arizona", "New Mexico", "Wyoming", "Alaska", "Oregon", "California"), "West"))))
}
cancer$Region <- factor(cancer$Region)
write.csv(cancer,"C:\\Users\\saran\\OneDrive\\Desktop\\TSoM\\Data Handling\\Final Project\\cancer_region.csv", row.names = F)

# Variable Analysis ----
cancer <- read.csv("C:\\Users\\saran\\OneDrive\\Desktop\\TSoM\\Data Handling\\Final Project\\cancer_region.csv")
str(cancer)

## Normality test ----
lapply(cancer[sapply(cancer, is.numeric)], shapiro.test)
# $ avgAnnCount            : 2.2e-16   # data not normally distributed
# $ avgDeathsPerYear       : 2.2e-16   # data not normally distributed
# $ deathRate              : 4.458e-10 # data not normally distributed  
# $ incidenceRate          : 2.2e-16   # data not normally distributed
# $ medIncome              : 2.2e-16   # data not normally distributed
# $ popEst2015             : 2.2e-16   # data not normally distributed
# $ povertyPercent         : 2.2e-16   # data not normally distributed
# $ studyPerCap            : 2.2e-16   # data not normally distributed
# $ MedianAge              : 3.594e-12 # data not normally distributed   
# $ MedianAgeMale          : 6.121e-12 # data not normally distributed 
# $ MedianAgeFemale        : 7.878e-12 # data not normally distributed
# $ AvgHouseholdSize       : 2.2e-16   # data not normally distributed
# $ PercentMarried         : 2.2e-16   # data not normally distributed
# $ PctBachDeg18_24        : 2.2e-16   # data not normally distributed
# $ PctBachDeg25_Over      : 2.2e-16   # data not normally distributed
# $ PctEmployed16_Over     : 5.862e-10 # data not normally distributed   
# $ PctUnemployed16_Over   : 2.2e-16   # data not normally distributed
# $ PctPrivateCoverage     : 2.345e-14 # data not normally distributed
# $ PctPrivateCoverageAlone: 0.0001161 # data not normally distributed  
# $ PctEmpPrivCoverage     : 7.189e-05 # data not normally distributed  
# $ PctPublicCoverage      : 0.004533  # data not normally distributed
# $ PctPublicCoverageAlone : 5.543e-14 # data not normally distributed  
# $ PctWhite               : 2.2e-16   # data not normally distributed
# $ PctBlack               : 2.2e-16   # data not normally distributed
# $ PctAsian               : 2.2e-16   # data not normally distributed
# $ PctOtherRace           : 2.2e-16   # data not normally distributed
# $ PctMarriedHouseholds   : 3.656e-15 # data not normally distributed  
# $ BirthRate              : 2.2e-16   # data not normally distributed


# Descriptive questions ----

########### 1 How are incident rates distributed across different racial groups? ----
temp <- round(cancer[,c("incidenceRate", "PctWhite","PctBlack","PctAsian","PctOtherRace")], digits = 2); temp

# Shapiro Test - Normality
lapply(temp, FUN = shapiro.test)

summary(cancer$PctWhite)
summary(cancer$PctBlack)
summary(cancer$PctAsian)
summary(cancer$PctOtherRace)

# As per the Shapiro test none of the variables are normally distributed. 
# Hence we find the correlation as per Spearman method. 

# Correlation Pearson - Spearman
cor(temp$incidenceRate, temp$PctWhite, method = "spearman")
# Weak Positive Linear Association
cor(temp$incidenceRate, temp$PctBlack, method = "spearman")
# Weak Positive Linear Association
cor(temp$incidenceRate, temp$PctAsian, method = "spearman")
# Weak Positive Linear Association
cor(temp$incidenceRate, temp$PctOtherRace, method = "spearman")
# Weak Negative Linear Association

library(PerformanceAnalytics)
chart.Correlation(temp, method = "spearman")

# Plot the heatmap with additional details
library(corrplot)
cor_matrix <- cor(temp[, c("incidenceRate", "PctWhite", "PctBlack", "PctAsian", "PctOtherRace")])
corrplot(cor_matrix, method = "color", 
         title = "Correlation of Incidence Rates Across Racial Groups",  # Main title
         type = "lower",   # Show correlation matrix in the upper part
         order = "hclust", # Order columns and rows based on hierarchical clustering
         tl.col = "black", # Color of text labels
         tl.srt = 45,      # Rotation of text labels
         addCoef.col = "#7b9194", # Add correlation coefficients to the plot
         number.cex = 1.25) # Size of the correlation coefficients


########### 2. How Do Poverty and Employment Affect Cancer Mortality Rates? Exploring the Socioeconomic Dimensions of Health Outcomes.
temp1 <- round(cancer[,c("deathRate", "povertyPercent","PctEmployed16_Over")], digits = 2); temp1

# Shapiro Test - Normality
lapply(temp1, FUN = shapiro.test)

# As per the Shapiro test none of the variables are normally distributed. 
# Hence we find the correlation as per Spearman method. 

# Correlation Pearson - Spearman method
cor(temp1$deathRate, temp1$povertyPercent, method = "spearman")
# 0.4564428 Weak Positive Linear Association
cor(temp1$deathRate, temp1$PctEmployed16_Over, method = "spearman")
# -0.4364413 Weak Negative Linear Association

library(PerformanceAnalytics)
chart.Correlation(temp1, method = "spearman")

# Creating a scatter plot to show the relationship between poverty rate and cancer mortality, with points colored to represent varying employment rates, 
# enhanced by a regression line to indicate the trend.
ggplot(temp1, aes(x = povertyPercent, y = deathRate)) +
  geom_point(aes(color = PctEmployed16_Over)) +
  geom_smooth(method = "lm") +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Cancer Mortality vs. Poverty Rate",
       subtitle = "Colored by Employment Rate",
       x = "Poverty Rate (%)",
       y = "Cancer Mortality Rate")


########### 3. Assessing the Relationship Between Insurance Coverage and the Prevalence of Cancer Cases: A Detailed Investigation into the Impact of Insurance Status on Cancer Diagnosis Rates. ----
temp2 <- round(cancer[,c("incidenceRate", "PctPrivateCoverage", "PctPrivateCoverageAlone", "PctEmpPrivCoverage", "PctPublicCoverage", "PctPublicCoverageAlone")], digits = 2); temp2

# Shapiro Test - Normality
lapply(temp2, FUN = shapiro.test)

# As per the Shapiro test none of the variables are normally distributed. 
# Hence we find the correlation as per Spearman method. 

# Correlation Pearson - Spearman
cor(temp2$incidenceRate, temp2$PctPrivateCoverage, method = "spearman")
#  Weak Positive Linear Association
cor(temp2$incidenceRate, temp2$PctPrivateCoverageAlone, method = "spearman")
# # Weak Positive Linear Association
cor(temp2$incidenceRate, temp2$PctEmpPrivCoverage, method = "spearman")
# Weak Positive Linear Association
cor(temp2$incidenceRate, temp2$PctPublicCoverage, method = "spearman") 
# Weak Positive Linear Association
cor(temp2$incidenceRate, temp2$PctPublicCoverageAlone, method = "spearman")
# Weak Positive Linear Association

library(PerformanceAnalytics)
chart.Correlation(temp2)

# Create a matrix of scatterplots
library(GGally)
ggpairs(temp2[, c("incidenceRate", "PctPrivateCoverage", "PctPrivateCoverageAlone", "PctEmpPrivCoverage", "PctPublicCoverage", "PctPublicCoverageAlone")])


########### 4. Exploring the Influence of Age and Gender on Cancer Rates: A Detailed Look at How Median Age and Gender Differences Impact Cancer Incidence ----
temp3 <- round(cancer[,c("incidenceRate", "MedianAge","MedianAgeMale","MedianAgeFemale")], digits = 2); temp3

# Shapiro Test - Normality
lapply(temp3, FUN = shapiro.test)

# As per the Shapiro test none of the variables are normally distributed. 
# Hence we find the correlation as per Spearman method. 

# Correlation Pearson - Spearman
cor(temp3$incidenceRate, temp3$MedianAge, method = "spearman")
#  Weak Positive Linear Association
cor(temp3$incidenceRate, temp3$MedianAgeMale, method = "spearman")
# # Weak Positive Linear Association
cor(temp3$incidenceRate, temp3$MedianAgeFemale, method = "spearman")
# Weak Positive Linear Association

library(PerformanceAnalytics)
chart.Correlation(temp3, method = "spearman")

# Create a 3D Scatter plot to visualise the data
library(plotly)
plot_ly(temp3, x = ~MedianAge, y = ~MedianAgeMale, z = ~MedianAgeFemale, color = ~incidenceRate, type = "scatter3d", mode = "markers") %>%
  layout(title = "3D Scatter Plot of Cancer Incidence by Age and Gender")


########### 5.  How does the incidence rate / death rate vary across different geographic regions by state? ----
### Death rate between regions ----
Levene_death_reg <- leveneTest(cancer$deathRate, cancer$Region, data = cancer)
Levene_death_reg$`Pr(>F)`< .05
### We have used Welch ANOVA since there isnt homogeneity of variances
Waov_results <- oneway.test(cancer$deathRate ~ cancer$Region, data = cancer)
Waov_results$p.value < .05
aov_results <- aov(cancer$deathRate ~ cancer$Region, data = cancer)
summary(aov_results)
### p-value is 2.2e-16 < 0.05, meaning that we reject H0 so the means of the dependent variables are not equal across different regions.
Mean_death_region <- aggregate(cancer$deathRate ~ cancer$Region, data = cancer, FUN = mean)
Mean_death_region_arr <- arrange(Mean_death_region, Mean_death_region$`cancer$deathRate`)
Mean_death_region_arr
#### Tukey's Honesty Significant difference (HSD Test)
tukey_death <- TukeyHSD(aov_results)
#### From running Tukey's test, we notice that the highest difference between means is West-South regions (-27.69) and (-2.27) between Northeast - Midwest.
#### Chart ----
### West region has the lowest mean of death rate(159.65) while South region has the highest (188.15)
par(las = 1)
barplot(Mean_death_region_arr$`cancer$deathRate`, main = "Average death rate per region", xlab = "Average death rate", ylab = "Region", names.arg = Mean_death_region_arr$`cancer$Region`, col = c("#E6FFFF", "#CCFFFF", "#99FFFF", "#80FFFF"), horiz = TRUE)

### Incidence rate between regions ----
Levene_incidence <- leveneTest(cancer$incidenceRate, cancer$Region, data = cancer)
Levene_incidence
### p-value < 2.2e-16 ***, meaning that there isn't homogeneity of variances
### We have used Welch ANOVA since there isn't homogeneity of variances
Waov_incidence <- oneway.test(cancer$incidenceRate ~ cancer$Region, data = cancer)
Waov_incidence
summary(Waov_incidence)
### p-value < 2.2e-16, meaning that we reject H0 so the means of the dependent variables are not equal across different levels of the factor.
### If we also assume homogeneity of variances, the results will not be much different.
aov_incidence <- aov(cancer$incidenceRate ~ cancer$Region, data = cancer)
aov_incidence
summary(aov_incidence)
### p-value <2e-16 ***, we also reject H0 in this case as well.

###Tukey's Honesty Significant difference (HSD Test)
tukey_aov_incidence <- TukeyHSD(aov_incidence)
### From running Tukey test on standard ANOVA, we notice that the highest difference between means is on West-Northeast regions (-64.51) and the lowest is between South-Midwest regions (0.47).
Mean_incidence_rate_region <- aggregate(cancer$incidenceRate ~ cancer$Region, data = cancer, FUN = mean)
Mean_incidence_rate_region
Mean_incidence_rate_region_arranged <- arrange(Mean_incidence_rate_region, Mean_incidence_rate_region$`cancer$incidenceRate`)
Mean_incidence_rate_region_arranged

#### Chart ----
#West region has the lowest mean of incidence rate (420.37), while Northeast region has the highest rate of incidence (484.88)
par(las = 1)
barplot(Mean_incidence_rate_region_arranged$`cancer$incidenceRate`, main = "Average incidence rate per region", xlab = "Average incidence rate", ylab = "Regions", names.arg = Mean_incidence_rate_region_arranged$`cancer$Region`, col = c("#FFE6FF", "#FFCCFF",
                                                                                                                                                                                                                                            "#FFB3FF", "#FF80FF" ), horiz = TRUE)

### Analysis for death and incidence rate by state ----
cancer$State <- as.factor(cancer$State)
cancer
Levene_test_results_death_state <- leveneTest(cancer$deathRate, cancer$State, data = cancer)
Levene_test_results_death_state
### p-value is 2.2e-16 ***, meaning that there isn't homogeneity of variances

### If we assume homogeneity of variances, we will still reject H0
aov_results_death_state <- aov(cancer$deathRate ~ cancer$State, data = cancer)
aov_results_death_state
summary(aov_results_death_state)
### p - value <2e-16 ***, meaning that the means of the dependent variables are not equal across different levels of the factor
### Tukey's Honesty Significant difference (HSD Test)
tukey_result_aov_death_state <- TukeyHSD(aov_results_death_state)
tukey_result_aov_death_state
### Considering the high number of counties, we have decided to use the aggregate functions to check the means of death&incidence rate as per states.

## State analysis using aggregate function
### Calculating the mean of death rate as per states
Mean_death_rate <- aggregate(cancer$deathRate ~ cancer$State, data = cancer, FUN = mean)
Mean_death_rate
max(Mean_death_rate$`cancer$deathRate`)
min(Mean_death_rate$`cancer$deathRate`)

Mean_incidence_rate <- aggregate(cancer$incidenceRate ~ cancer$State, data = cancer, FUN = mean)
Mean_incidence_rate
max(Mean_incidence_rate$`cancer$incidenceRate`)
min(Mean_incidence_rate$`cancer$incidenceRate`)
Mean_death_rate_arranged <- arrange(Mean_death_rate, Mean_death_rate$`cancer$deathRate`)
Mean_death_rate_arranged
### The county with the lowest mean of death rate is Utah (141.60), while the county with the highest mean of death rate is Kentucky (209.59)
#### Charts ----
par(las = 1)
barplot(Mean_death_rate_arranged$`cancer$deathRate`, main = "Average death rate per state", xlab = "Average death rate", ylab = "States", names.arg = Mean_death_rate_arranged$`cancer$State`, col = terrain.colors(51), horiz = TRUE)

Mean_incidence_rate_arranged <- arrange(Mean_incidence_rate, Mean_incidence_rate$`cancer$incidenceRate`)
Mean_incidence_rate_arranged
#The country with the highest mean of incidence rate is Arizona (361.78) and the country with the highest mean of incidence rate is Kentucky (509.55)
par(las = 1)
barplot(Mean_incidence_rate_arranged$`cancer$incidenceRate`, main = "Average incidence rate per state", xlab = "Average incidence rate", ylab = "States", names.arg = Mean_incidence_rate_arranged$`cancer$State`, col = topo.colors(51), horiz = TRUE)



### Chart ----
chart.Correlation(temp2)

install.packages("GGally")
library(GGally)
# Create a matrix of scatterplots
ggpairs(temp2[, c("incidenceRate", "PctPrivateCoverage", "PctPrivateCoverageAlone", "PctEmpPrivCoverage", "PctPublicCoverage", "PctPublicCoverageAlone")])


# Predictive questions ----

## Predictive Question 1 ----
### HOW DOES RACE PREDICT THE INCIDENCE AND MORTALITY RATES OF CANCER? AN ANALYSIS OF RACIAL INFLUENCES ON CANCER OUTCOMES
###We also ran this model initially to predict incidence rate across different racial groups
sami <- sort(sample(nrow(cancer), nrow(cancer)*.8))
train <- cancer[sami,]
test <- cancer[-sami,]
lm_incidence <- lm(incidenceRate ~ PctWhite + PctBlack + PctAsian + PctOtherRace, data = train)
summary(lm_incidence)
### We use the model to predict values in the test set and compare
test$Predic_Inc <- predict(lm_incidence, newdata = test)
test[c("incidenceRate","Predic_Inc")]
test$predic_var_inc <- ((test$incidenceRate - test$Predic_Inc) / test$incidenceRate)
mean(test$predic_var_inc)
### We obtain a mean error of 1.8% (.018 approx.), and came to a conclusion that race was a good indicator on predicting incidence rate.

#We also run the same model to check whether racial groups were also a good indicator of death rate.

lm_death <- lm(deathRate ~ PctWhite + PctBlack + PctAsian + PctOtherRace, data = train)
summary(lm_death)
### We use the model to predict values in the test set and compare
test$Predic_Death <- predict(lm_death, newdata = test)
test[c("deathRate","Predic_Death")]
test$predic_var_death <- ((test$deathRate - test$Predic_Death) / test$deathRate)
mean(test$predic_var_death)
### We obtain a mean error of -0.027 (.027 approx.), meaning that racial belonging is a good predictor of death rate.


## Prediction Question 2 Does marriage affect death & incidence rate? ----
temporary_dataset <- cancer[, c("deathRate", "incidenceRate", "PercentMarried", "PctMarriedHouseholds")]
cor(temporary_dataset$deathRate, temporary_dataset$PercentMarried, method = "spearman")
cor(temporary_dataset$incidenceRate, temporary_dataset$PercentMarried, method = "spearman")
cor(temporary_dataset$deathRate, temporary_dataset$PctMarriedHouseholds, method = "spearman")
cor(temporary_dataset$incidenceRate, temporary_dataset$PctMarriedHouseholds, method = "spearman")
#From running Spearman correlation, we notice that there is quite a significant negative correlation for this dataset between death rate and % of married Households (-0.28) and quite a significant correlation between death rate and % of married individuals.
#Therefore, we have also created a model to predict the death rate and incident rate from knowing if you are married.
sami <- sort(sample(nrow(cancer), nrow(cancer)*.8))
train <- cancer[sami,]
test <- cancer[-sami,]
#Creating the linear model for death rate in  terms of marriage indicator
lm_cancer_marriage <- lm(deathRate ~ PercentMarried + PctMarriedHouseholds, data = train)
lm_cancer_marriage
summary(lm_cancer_marriage)
#Residual standard error for this case is 24.08 and p-value: < 2.2e-16.
#Testing the model with test dataset (20%)
cancer_predict_married <- predict(lm_cancer_marriage, newdata = test)
cancer_predict_married
test$predict_married_values <- cancer_predict_married
test$prediction_married_deviation <- ((test$deathRate - test$predict_married_values)/test$deathRate)
mean(test$prediction_married_deviation)
#The mean is -0.02543025, meaning that PctMarriedHouseholds and PercentMarried are good indicators of death rate.

plot(lm_cancer_marriage)

#Creating the linear model for incidence rate in  terms of marriage indicator
lm_cancer_marriage_incidence <- lm(incidenceRate ~ PercentMarried + PctMarriedHouseholds, data = train)
lm_cancer_marriage_incidence
summary(lm_cancer_marriage_incidence)
#Residual standard error for this case is 47.68 and p-value: < 2.982e-12.
#Testing the model with test dataset (20%)
cancer_predict_married_incidence_rate <- predict(lm_cancer_marriage_incidence, newdata = test)
cancer_predict_married_incidence_rate
test$predict_married_values_incidence <- cancer_predict_married_incidence_rate
test$prediction_married_incidence_deviation <- ((test$deathRate - test$predict_married_values_incidence)/test$deathRate)
mean(test$prediction_married_incidence_deviation)
#The mean is -1.574991, meaning that PctMarriedHouseholds and PercentMarried are not good indicators of incidence rate, but good predictors of death rate.

## Predictive Question 3 death rate - Study per capita 
sami <- sort(sample(nrow(cancer), nrow(cancer)*.8))
train <- cancer[sami,]
test <- cancer[-sami,]

research_lm <- lm(cancer$deathRate ~ cancer$studyPerCap + cancer$AvgHouseholdSize + cancer$medIncome, data = train)
summary(research_lm)
research_lm
cancer$predict_research <- predict(research_lm, newdata = test)
cancer[c("deathRate","predict_research")]
cancer$predic_researchdiff <- ((cancer$deathRate - cancer$predict_research) / cancer$deathRate)
mean(cancer$predic_researchdiff)

# Cluster analyisis ----

## Cluster by deathrate ----
cancer <- read.csv("C:\\Users\\saran\\OneDrive\\Desktop\\TSoM\\Data Handling\\Final Project\\cancer_region.csv")
cancer_d <- cancer[6:8]
cancer_d
fit <- kmeans(cancer_d$deathRate, 4)
fit$cluster;
fit$iter;

cl_d1 <- cancer_d[fit$cluster==1,]
cl_d2 <- cancer_d[fit$cluster==2,]
cl_d3 <- cancer_d[fit$cluster==3,]
cl_d4 <- cancer_d[fit$cluster==4,]

cancer_d$deathClass <- fit$cluster
cancer_d$deathClass[cancer_d$deathClass == 1] <- "A"
cancer_d$deathClass[cancer_d$deathClass == 2] <- "B"
cancer_d$deathClass[cancer_d$deathClass == 3] <- "C"
cancer_d$deathClass[cancer_d$deathClass == 4] <- "D"

plot(cancer$deathRate, 
     pch=21,
     bg=fit$cluster * 6 + 3)

plot_3d_death <- plot_ly(
    x = cancer_d$medIncome,
    y = cancer_d$deathRate,
    z = cancer_d$incidenceRate,
    type = "scatter3d",
    mode = "markers",
    size = 7,
    color = fit$cluster * 6 + 4
) %>%
    plotly::layout(scene = list(xaxis = list(title = 'Median Income'),
                                yaxis = list(title = 'Death Rate'),
                                zaxis = list(title = 'Incidence rate')))

plot_3d_death

## Cluster marriage,(TEST) ----
cancer <- read.csv("C:\\Users\\saran\\OneDrive\\Desktop\\TSoM\\Data Handling\\Final Project\\cancer_region.csv")
cancer_d <- cancer[c(6,15,16)]
str(cancer)
str(cancer_d)
cancer_d
fit <- kmeans(cancer_d$deathRate, 4)
fit$cluster;
fit$iter;

cl_d1 <- cancer_d[fit$cluster==1,]
cl_d2 <- cancer_d[fit$cluster==2,]
cl_d3 <- cancer_d[fit$cluster==3,]
cl_d4 <- cancer_d[fit$cluster==4,]

cancer_d$deathClass <- fit$cluster
cancer_d$deathClass[cancer_d$deathClass == 1] <- "A"
cancer_d$deathClass[cancer_d$deathClass == 2] <- "B"
cancer_d$deathClass[cancer_d$deathClass == 3] <- "C"
cancer_d$deathClass[cancer_d$deathClass == 4] <- "D"

plot(cancer$deathRate, 
     pch=21,
     bg=fit$cluster * 6 + 3)

plot_3d_death <- plot_ly(x=cancer_d$deathRate,
                         y=cancer_d$incidenceRate,
                         z=cancer_d$medIncome,
                         type="scatter3d",
                         mode="markers",
                         
                         size=7,
                         color=fit$cluster * 6 + 4)
plot_3d_death

plot_3d_death <- plot_3d_death %>% layout(scene = list(xaxis = list(title = 'Death Rate'),
                                                       yaxis = list(title = 'Incidence Rate'),
                                                       zaxis = list(title = 'Median income')))



### Hierarchical based cluster ----

### State level data frame with average death rate
state_cancer_df <- aggregate(deathRate ~ State, data = cancer, FUN = mean)

### Add state abbreviation . Handled DC state exception
state_cancer_df$state <-  state.abb[match(state_cancer_df$State, state.name)]
state_cancer_df[state_cancer_df$State == "District of Columbia","state" ] <- "DC" 

### Perform distance matrix computation on the data frame
state_cancer_df.dist <- dist(state_cancer_df$deathRate)

###Perform hierarchical clustering
fit <- hclust(state_cancer_df.dist, method = "average")

### Plot Dendrogram ----
plot(fit, main = "Hierarchical Clustering Dendrogram - Average death rate by State", xlab = "state",labels=state_cancer_df$state)

### Cluster the tree into groups of data
clusters <- cutree(fit, k = 6)

### Add cluster labels to the data frame
state_data_with_clusters <- cbind(state_cancer_df, Cluster = clusters)
#### Factor the cluster as categorical 
state_data_with_clusters$Cluster <- factor(state_data_with_clusters$Cluster)
#### Chart US ----
plot_usmap(data = state_data_with_clusters, region = "state" , values = "Cluster",labels = TRUE ,color = "white") +
    labs(title = "Cluster Visualization based on Death Rates") +
    theme(legend.position = "right")