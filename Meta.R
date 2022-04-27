#Using the mada package for the meta analysis. 
library(mada) #if you haven't already install it, you can install it using install.packages("mada")
# Loading required package: mvtnorm
# Loading required package: ellipse
# 
# Attaching package: ‘ellipse’
# 
# The following object is masked from ‘package:graphics’:
#   
#   pairs
# 
# Loading required package: mvmeta
# This is mvmeta 1.0.3. For an overview type: help('mvmeta-package').

library(meta)#Necessary for some univariate statistics

#Importing data
library(readxl)
Data <- read_excel("Library/CloudStorage/OneDrive-Personal/My Drive/CANDILAB/META/Data.xlsx") #This dataset consists of the highest scores from each independent sample from each study
# New names:                                                                                                                                                                                                                           
#   • `Activity` -> `Activity...71`
# • `Connectivity` -> `Connectivity...72`
# • `Activity` -> `Activity...74`
# • `Connectivity` -> `Connectivity...75`
# > View(Data)

#Calculating a confusion matrix for each participant, consisting of TP, TN, FP, FN. 
#Most studies did not provide these estimates, but provided sensitivity and specificity estimates. Using those and the sample size, you can calculate these metrics. 

library(dplyr)

Data <- Data %>% mutate(TP=Sens*P_n)
Data <- Data %>% mutate(TN=Spec*(C_n))
Data <- Data %>% mutate(FP=C_n-TN) 
Data <- Data %>% mutate(FN=P_n-TP)

#Calculating a univariate descriptive value known as Diagnostic Odds Ratio (DOR)

DOR <- metabin(TP,(TP+FP),FN,(FN+TN),sm="OR", comb.fixed=FALSE, comb.random = TRUE, 
               method="Inverse", Study, data=Data)
print(DOR)
# Number of studies combined: k = 55
# Number of observations: o = 7063
# Number of events: e = 3630
# 
# OR            95%-CI     z  p-value
# Random effects model 13.4405 [9.9894; 18.0839] 17.16 < 0.0001
# 
# Quantifying heterogeneity:
#   tau^2 = 0.8528 [0.4883; 1.4985]; tau = 0.9235 [0.6988; 1.2241]
# I^2 = 74.7% [67.1%; 80.5%]; H = 1.99 [1.74; 2.26]
# 
# Test of heterogeneity:
#   Q d.f.  p-value
# 213.19   54 < 0.0001
# 
# Details on meta-analytical method:
#   - Inverse variance method
# - Restricted maximum-likelihood estimator for tau^2
# - Q-profile method for confidence interval of tau^2 and tau
# - Continuity correction of 0.5 in studies with zero cell frequencies

#Creating a forest plot
forest(DOR, digits=3, rightcols=c("effect","ci"), xlab="Diagnostic Odds Ratio")

#Nice, all worked, but just realized that my calculation of the confusion matrix isn't rounded, which will look weird, so rounding it. 
#It is important to note that what I am creating here is an estimation. 
#Rounding the four columns. 

Data$TP <- round(Data$TP)
Data$TN <- round(Data$TN)
Data$FP <- round(Data$FP)
Data$FN <- round(Data$FN)
#Re-ran the code with the rounded data.

#While sensitivity and specificity estimates were provided by each study, they need to be logit transformed for the meta analysis. 
#Will also create forest plots for these values. 

sensitivity_logit <- metaprop(Data$TP, Data$TP+Data$FN, fixed=FALSE, random=TRUE, sm="PLOGIT", method.ci="CP", studlab=Data$Study)
print(sensitivity_logit, digits=3)
# Number of studies combined: k = 55
# Number of observations: o = 3630
# Number of events: e = 2854
# 
# proportion         95%-CI
# Random effects model      0.782 [0.743; 0.817]
# 
# Quantifying heterogeneity:
#   tau^2 = 0.4885; tau = 0.6989; I^2 = 85.3% [81.6%; 88.2%]; H = 2.61 [2.33; 2.92]
#   
#   Test of heterogeneity:
#     Q d.f.  p-value             Test
#   367.18   54 < 0.0001        Wald-type
#   440.97   54 < 0.0001 Likelihood-Ratio
#   
#   Details on meta-analytical method:
#     - Random intercept logistic regression model
#   - Maximum-likelihood estimator for tau^2
#   - Logit transformation
#   - Continuity correction of 0.5 in studies with zero cell frequencies
#   (only used to calculate individual study results)
forest(sensitivity_logit, digits=3, rightcols=c("effect", "ci"), xlab="Sensitivity")

specificity_logit <- metaprop(Data$TN, Data$TN+Data$FP, fixed=FALSE, random=TRUE, sm="PLOGIT", method.ci="CP", studlab=Data$Study)
print(specificity_logit, digits=3)
# Number of studies combined: k = 55
# Number of observations: o = 3434
# Number of events: e = 2774
# 
# proportion         95%-CI
# Random effects model      0.795 [0.764; 0.823]
# 
# Quantifying heterogeneity:
#   tau^2 = 0.2958; tau = 0.5439; I^2 = 74.3% [66.6%; 80.2%]; H = 1.97 [1.73; 2.25]
#   
#   Test of heterogeneity:
#     Q d.f.  p-value             Test
#   210.31   54 < 0.0001        Wald-type
#   246.03   54 < 0.0001 Likelihood-Ratio
#   
#   Details on meta-analytical method:
#     - Random intercept logistic regression model
#   - Maximum-likelihood estimator for tau^2
#   - Logit transformation
forest(specificity_logit, digits=3, rightcols = c("effect","ci"), xlab="Specificity")

#These statistics were run for the entire dataset as a whole, which I hypothesized would have substantial heterogeneity, and lord-behold, I was right. 
#Now breaking down these studies by the research question they will answer. I will do this before I run bivariate models. 

#I have a variable in the dataset known as class.type, which broke down the classifications into five categories
#1: SUD vs HC
#2: Homogeneous antisocial groups
#3: Substance users vs HC
#4: Homogenous substance user groups
#5: Antisocial vs HC

#I'll first recode this variable so that it is dichotomous: heterogeneous vs homogeneous. 

Data <- Data %>% mutate(hetero.homo=recode(Class.type, "1" = "1", "2"="2", "3" = "1", "4"="2","5"="1"))

#Rerunning the above analysis, but including hetero.homo as a subgroup variable. This will separate the results by classification. 
DOR_dichot <- metabin(TP,(TP+FP),FN,(FN+TN),sm="OR", comb.fixed=FALSE, comb.random = TRUE, 
               method="Inverse", Study, data=Data, byvar = hetero.homo)
print(DOR_dichot)
# Number of studies combined: k = 55
# Number of observations: o = 7064
# Number of events: e = 3630
# 
# OR            95%-CI     z  p-value
# Random effects model 13.3054 [9.8910; 17.8986] 17.11 < 0.0001
# 
# Quantifying heterogeneity:
#   tau^2 = 0.8523 [0.4960; 1.5361]; tau = 0.9232 [0.7042; 1.2394]
# I^2 = 74.5% [66.9%; 80.4%]; H = 1.98 [1.74; 2.26]
# 
# Test of heterogeneity:
#   Q d.f.  p-value
# 211.78   54 < 0.0001
# 
# Results for subgroups (random effects model):
#   k      OR            95%-CI  tau^2    tau      Q   I^2
# hetero.homo = 1  37 13.8958 [9.6113; 20.0903] 0.9626 0.9811 170.19 78.8%
# hetero.homo = 2  18 11.8939 [7.2882; 19.4103] 0.5812 0.7623  41.35 58.9%
# 
# Test for subgroup differences (random effects model):
#   Q d.f. p-value
# Between groups   0.25    1  0.6189
# Details on meta-analytical method:
#   - Inverse variance method
# - Restricted maximum-likelihood estimator for tau^2
# - Q-profile method for confidence interval of tau^2 and tau
# - Continuity correction of 0.5 in studies with zero cell frequencies
forest(DOR_dichot, digits = 3, rightcols = c("effect","ci"), xlab = "Diagnostic Odds Ratio")

sensitivity_dichot <- metaprop(Data$TP, Data$TP+Data$FN, fixed=FALSE, random=TRUE, 
                               sm="PLOGIT", method.ci="CP", studlab=Data$Study, subgroup = Data$hetero.homo)
print(sensitivity_dichot, digits=3)
# Number of studies combined: k = 55
# Number of observations: o = 3630
# Number of events: e = 2854
# 
# proportion         95%-CI
# Random effects model      0.782 [0.743; 0.817]
# 
# Quantifying heterogeneity:
#   tau^2 = 0.4885; tau = 0.6989; I^2 = 85.3% [81.6%; 88.2%]; H = 2.61 [2.33; 2.92]
#   
#   Test of heterogeneity:
#     Q d.f.  p-value             Test
#   367.18   54 < 0.0001        Wald-type
#   440.97   54 < 0.0001 Likelihood-Ratio
#   
#   Results for subgroups (random effects model):
#     k proportion         95%-CI  tau^2    tau      Q   I^2
#   hetero.homo = 1  37      0.783 [0.738; 0.822] 0.4345 0.6591 325.89 89.0%
#   hetero.homo = 2  18      0.782 [0.693; 0.850] 0.7102 0.8428  38.63 56.0%
#   
#   Test for subgroup differences (random effects model):
#     Q d.f. p-value
#   Between groups   0.00    1  0.9735
#   
#   Details on meta-analytical method:
#     - Random intercept logistic regression model
#   - Maximum-likelihood estimator for tau^2
#   - Logit transformation
#   - Continuity correction of 0.5 in studies with zero cell frequencies
#   (only used to calculate individual study results)
forest(sensitivity_dichot, digits=3, rightcols=c("effect", "ci"), xlab="Sensitivity")

specificity_dichot <- metaprop(Data$TN, Data$TN+Data$FP, fixed=FALSE, random=TRUE, 
                               sm="PLOGIT", method.ci="CP", studlab=Data$Study, subgroup = Data$hetero.homo)
print(specificity_dichot, digits=3)
# Number of studies combined: k = 55
# Number of observations: o = 3434
# Number of events: e = 2774
# 
# proportion         95%-CI
# Random effects model      0.795 [0.764; 0.823]
# 
# Quantifying heterogeneity:
#   tau^2 = 0.2958; tau = 0.5439; I^2 = 74.3% [66.6%; 80.2%]; H = 1.97 [1.73; 2.25]
#   
#   Test of heterogeneity:
#     Q d.f.  p-value             Test
#   210.31   54 < 0.0001        Wald-type
#   246.03   54 < 0.0001 Likelihood-Ratio
#   
#   Results for subgroups (random effects model):
#     k proportion         95%-CI  tau^2    tau      Q   I^2
#   hetero.homo = 1  37      0.797 [0.766; 0.824] 0.1562 0.3952  65.01 44.6%
#   hetero.homo = 2  18      0.787 [0.709; 0.849] 0.6068 0.7789 143.48 88.2%
#   
#   Test for subgroup differences (random effects model):
#     Q d.f. p-value
#   Between groups   0.06    1  0.8136
#   
#   Details on meta-analytical method:
#     - Random intercept logistic regression model
#   - Maximum-likelihood estimator for tau^2
#   - Logit transformation
forest(specificity_dichot, digits=3, rightcols=c("effect", "ci"), xlab="Specificity")

#Breaking it down by the five classifications. 

DOR_5 <- metabin(TP,(TP+FP),FN,(FN+TN),sm="OR", comb.fixed=FALSE, comb.random = TRUE, 
                      method="Inverse", Study, data=Data, byvar = Data$Class.type)
print(DOR_5)
# Number of studies combined: k = 55
# Number of observations: o = 7064
# Number of events: e = 3630
# 
# OR            95%-CI     z  p-value
# Random effects model 13.3054 [9.8910; 17.8986] 17.11 < 0.0001
# 
# Quantifying heterogeneity:
#   tau^2 = 0.8523 [0.4960; 1.5361]; tau = 0.9232 [0.7042; 1.2394]
# I^2 = 74.5% [66.9%; 80.4%]; H = 1.98 [1.74; 2.26]
# 
# Test of heterogeneity:
#   Q d.f.  p-value
# 211.78   54 < 0.0001
# 
# Results for subgroups (random effects model):
#   k      OR            95%-CI  tau^2    tau     Q   I^2
# Class.type = 1  20 13.8512 [9.0909; 21.1040] 0.5594 0.7479 64.51 70.5%
# Class.type = 2   5 25.4833 [7.2488; 89.5872] 1.4881 1.2199 19.11 79.1%
# Class.type = 3  11 13.8153 [5.3063; 35.9688] 2.3946 1.5475 96.48 89.6%
# Class.type = 4  13  9.3587 [5.7120; 15.3334] 0.3116 0.5582 21.66 44.6%
# Class.type = 5   6 13.8554 [8.0025; 23.9891] 0.0920 0.3033  4.58  0.0%
# 
# Test for subgroup differences (random effects model):
#   Q d.f. p-value
# Between groups   2.94    4  0.5686
# 
# Details on meta-analytical method:
#   - Inverse variance method
# - Restricted maximum-likelihood estimator for tau^2
# - Q-profile method for confidence interval of tau^2 and tau
# - Continuity correction of 0.5 in studies with zero cell frequencies
forest(DOR_5, digits=3, rightcols=c("effect", "ci"), xlab="Diagnostic Odds Ratio")

sensitivity_5 <- metaprop(Data$TP, Data$TP+Data$FN, fixed=FALSE, random=TRUE, 
                               sm="PLOGIT", method.ci="CP", studlab=Data$Study, subgroup = Data$Class.type)
print(sensitivity_5, digits=3)
# Number of studies combined: k = 55
# Number of observations: o = 3630
# Number of events: e = 2854
# 
# proportion         95%-CI
# Random effects model      0.782 [0.743; 0.817]
# 
# Quantifying heterogeneity:
#   tau^2 = 0.4885; tau = 0.6989; I^2 = 85.3% [81.6%; 88.2%]; H = 2.61 [2.33; 2.92]
#   
#   Test of heterogeneity:
#     Q d.f.  p-value             Test
#   367.18   54 < 0.0001        Wald-type
#   440.97   54 < 0.0001 Likelihood-Ratio
#   
#   Results for subgroups (random effects model):
#     k proportion         95%-CI  tau^2    tau      Q   I^2
#   Class.type = 1  20      0.785 [0.733; 0.829] 0.2648 0.5146 114.48 83.4%
#   Class.type = 2   5      0.791 [0.690; 0.865] 0.1209 0.3477   6.61 39.4%
#   Class.type = 3  11      0.770 [0.652; 0.856] 0.8399 0.9164 142.46 93.0%
#   Class.type = 4  13      0.772 [0.645; 0.864] 0.9949 0.9974  30.97 61.2%
#   Class.type = 5   6      0.807 [0.752; 0.852] 0.0261 0.1614   8.22 39.2%
#   
#   Test for subgroup differences (random effects model):
#     Q d.f. p-value
#   Between groups   0.73    4  0.9480
#   
#   Details on meta-analytical method:
#     - Random intercept logistic regression model
#   - Maximum-likelihood estimator for tau^2
#   - Logit transformation
#   - Continuity correction of 0.5 in studies with zero cell frequencies
#   (only used to calculate individual study results)
forest(sensitivity_5, digits=3, rightcols=c("effect", "ci"), xlab="Sensitivity")

specificity_5 <- metaprop(Data$TN, Data$TN+Data$FP, fixed=FALSE, random=TRUE, 
                               sm="PLOGIT", method.ci="CP", studlab=Data$Study, subgroup = Data$Class.type)
print(specificity_5, digits=3)
# Number of studies combined: k = 55
# Number of observations: o = 3434
# Number of events: e = 2774
# 
# proportion         95%-CI
# Random effects model      0.795 [0.764; 0.823]
# 
# Quantifying heterogeneity:
#   tau^2 = 0.2958; tau = 0.5439; I^2 = 74.3% [66.6%; 80.2%]; H = 1.97 [1.73; 2.25]
#   
#   Test of heterogeneity:
#     Q d.f.  p-value             Test
#   210.31   54 < 0.0001        Wald-type
#   246.03   54 < 0.0001 Likelihood-Ratio
#   
#   Results for subgroups (random effects model):
#     k proportion         95%-CI  tau^2    tau      Q   I^2
#   Class.type = 1  20      0.794 [0.766; 0.818]      0      0  16.20  0.0%
#   Class.type = 2   5      0.862 [0.749; 0.929] 0.4612 0.6791  18.59 78.5%
#   Class.type = 3  11      0.807 [0.733; 0.865] 0.4143 0.6437  39.91 74.9%
#   Class.type = 4  13      0.750 [0.651; 0.829] 0.5521 0.7431 124.29 90.3%
#   Class.type = 5   6      0.753 [0.661; 0.826] 0.0388 0.1969   6.31 20.8%
#   
#   Test for subgroup differences (random effects model):
#     Q d.f. p-value
#   Between groups   3.84    4  0.4283
#   
#   Details on meta-analytical method:
#     - Random intercept logistic regression model
#   - Maximum-likelihood estimator for tau^2
#   - Logit transformation
forest(specificity_5, digits=3, rightcols=c("effect", "ci"), xlab="Diagnostic Odds Ratio")
