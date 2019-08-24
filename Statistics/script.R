# Uploading the libraries
library(psych)
library(broom)
library(stringr)
library(ggplot2)
library(caTools)
library(purrr)
library(dplyr)
library(plyr)
library(sjPlot)
library(sjmisc)

# Uploading code for mupltiplot
source("~/Documents/Projects/Statistics/multiplot.R")

# Setting the theme for linear regression plot
set_theme(base = theme_minimal())


# Uploading the data
demographicDB <- read.csv("~/Documents/Projects/Statistics/demographic_data.csv")

# summary(demographicDB) First check the summary figures for the data set and note that there are blank cells in the table

# Cleaning data

demographicDB = demographicDB[,-1] # I remove the first variable containing participant's ID because it will not be used throughout the analysis and is not relevant for this study.
colnames(demographicDB) = c("Diagnosis", "Gender", "Age", "Ethnicity", "Pain_VAS") # I rename a column with the pain scale for easier access. 

# Converting missings into NAs for pain variable
demographicDB$Pain_VAS[demographicDB$Pain_VAS==""] <- NA
demographicDB$Diagnosis[demographicDB$Diagnosis==""] <- NA
# Converting pain variable into numeric type
demographicDB$Pain_VAS = as.character(demographicDB$Pain_VAS)
demographicDB$Pain_VAS = as.numeric(demographicDB$Pain_VAS)

demographicDB$Ethnicity = as.character(demographicDB$Ethnicity)
demographicDB$Ethnicity = ifelse(str_detect(demographicDB$Ethnicity,"caucasian"),"Caucasian",demographicDB$Ethnicity)
demographicDB$Ethnicity = as.factor(demographicDB$Ethnicity)

demographicDB_summary = demographicDB[,-2]
pander::pander(summary(demographicDB_summary), caption = "Summary Statistics")

# Figure 1, histogram of pain levels
df.plot <- ggplot(demographicDB, aes(x = Pain_VAS)) +
  geom_histogram(binwidth=1, colour = "white",fill="gray35")+
  theme_bw()+
  theme(panel.border=element_blank(),
        axis.title = element_text(size = 9),
        legend.position = "none",
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank())+ labs(x="Pain level", y="Number of patients") 
df.plot

# Fugure 2, violin plots, distribution of pain levels across patients with different ethnicity and gender
demographicDB_violin = demographicDB %>%
  group_by(Ethnicity) %>%
  filter(n()>10)
dp1 <- ggplot(demographicDB_violin, aes(x=Ethnicity, y=Pain_VAS, fill=Ethnicity)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(x="Ethnicity", y = "Pain level")

dp1 = dp1 + scale_fill_grey()  +
  theme_bw()+
  theme(panel.border=element_blank(),
        axis.title = element_text(size = 9),
        legend.position = "none",
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank()) 

dp2 <- ggplot(demographicDB, aes(x=Gender, y=Pain_VAS, fill=Gender)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(x="Gender", y = "Pain level")

dp2 <-dp2 + scale_fill_grey()  +
  theme_bw()+
  theme(panel.border=element_blank(),
        axis.title = element_text(size = 9),
        legend.position = "none",
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank()) 

multiplot(dp1,dp2, cols=2)

# Figure 3, boxplot, distribution of pain levels across patients with different diagnosis

dp <- ggplot(data = subset(demographicDB, !is.na(Diagnosis)), aes(x=Diagnosis, y=Pain_VAS, fill=Diagnosis,na.rm = TRUE)) + 
  geom_boxplot()+
  labs(x="Diagnosis", y = "Pain level")

dp + coord_flip()+ scale_fill_grey(start = 0, end = 0.5)  +
  theme_bw()+
  theme(panel.border=element_blank(),
        axis.title = element_text(size = 9),
        legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks = element_blank()) 

# Tables to check skewness and kurtosis. Diagnosis
table_1 = describeBy(demographicDB$Pain_VAS,demographicDB$Diagnosis)
table_1 <- ldply (table_1, data.frame)
table_1 = table_1[,-(14)]
table_1 = table_1[,-(7:11)]
table_1 = table_1[,-(2)]
row.names(table_1) = table_1[,1]
table_1 = table_1[,-(1)]

colnames(table_1) = c("N", "Mean", "SD", "Median", "Skew", "Kurtosis")

table_1$Mean = round(table_1$Mean, 2)
table_1$SD = round(table_1$Median, 2)
table_1$Skew = round(table_1$Skew, 2)
table_1$Kurtosis = round(table_1$Kurtosis, 2)

knitr::kable(table_1, caption = "Comparison of Skewness and Kurtosis Coefficients between the Diagnosis Groups", floating.environment="sidewaystable")

# Tables to check skewness and kurtosis. Gender

table_2 = describeBy(demographicDB$Pain_VAS,demographicDB$Gender)
table_2 <- ldply (table_2, data.frame)
table_2 = table_2[,-(14)]
table_2 = table_2[,-(7:11)]
table_2 = table_2[,-(2)]
row.names(table_2) = table_2[,1]
table_2 = table_2[,-(1)]

colnames(table_2) = c("N", "Mean", "SD", "Median", "Skew", "Kurtosis")

table_2$Mean = round(table_2$Mean, 2)
table_2$SD = round(table_2$Median, 2)
table_2$Skew = round(table_2$Skew, 2)
table_2$Kurtosis = round(table_2$Kurtosis, 2)

knitr::kable(table_2, caption = "Comparison of Skewness and Kurtosis Coefficients between the Gender Groups", floating.environment="sidewaystable")

# Testing variables on homogenity of variances using Bartlett's test
bartlett.test(demographicDB$Pain_VAS,demographicDB$Gender) # p value is not significant at 0.005 level, the variance is homogeneous, correction does not needed.
bartlett.test(demographicDB$Pain_VAS,demographicDB$Diagnosis) # p value is not significant at 0.005 level, the variance is homogeneous, correction does not needed.
table_3 = data.frame("Varibale" = c("Gender", "Diagnosis"),"K-squared" = c(1.2859, 8.1294), "p-value" = c(0.2568, 0.2288))
pander::pander(table_3, caption = "Results of Bartlett's Tests for Homogeneity of Variances")

# Two-sample T-test
t1=t.test(Pain_VAS~Gender,demographicDB, var.equal=T) # p-value = 0.001774
table_4 <- map_df(list(t1), tidy)
table_4 = table_4[,-(6:9)]
colnames(table_4) = c("Mean-1", "Mean-2", "T-statistic", "P-value",   "DF")
pander::pander(table_4, caption = "Results of Two Sample T-test")

# One-way ANOVA
# Compute the analysis of variance
res.aov <- aov(Pain_VAS ~ Diagnosis, data = demographicDB) # p value 6.18e-05
# Summary of the analysis
pander::pander(summary(res.aov), caption = "One-way ANOVA")
# Tukey Honest Significant Differences plot
with(par(mai=c(1,2.5,1,1)),{plot(TukeyHSD(res.aov), las=1,cex.axis=0.4)})

# Scatter plot: age and pain levels
ggplot(demographicDB, aes(x=Age, y=Pain_VAS)) +
  geom_point( color="gray33", alpha=0.85) +
  theme_bw()+
  theme(panel.border=element_blank(),
        axis.title = element_text(size = 9),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank()) +
  ylab('Pain level') +
  xlab('Age')

# Regression model
set.seed(101) 
sample = sample.split(demographicDB, SplitRatio = .8)
train = subset(demographicDB, sample == TRUE)
test  = subset(demographicDB, sample == FALSE)

model1 = lm(Pain_VAS~Age+Gender+Diagnosis, train)
plot_model(model1,sort.est = TRUE,show.values = TRUE, value.offset = .3)

# VIF check

pander::pander(car::vif(model1),caption = "VIF")

# Evaluation of the performance of the data

pred <- predict(model1,  newdata = test)

table4 = as.data.frame(cbind(test$Pain_VAS, pred))
colnames(table4) = c("Actual", "Predicted")
table4$Difference = (table4$Actual-table4$Predicted)
table4$Predicted = round(table4$Predicted,1)
table4$Difference = round(table4$Difference,1)
row.names(table4) = 1:159
pander::pander(head(table4,5),caption = "First five Actual vs Predicted values and their difference")

# Root Mean Squared Error
table4 = subset(table4, !is.na(Difference))
sqrt(mean(table4$Difference^2))