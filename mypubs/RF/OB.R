dat=read.csv("datOB.csv", T)


#This shows the first six lines of the database
head(dat)
#  Subject  Stem StimulusType FirstResponse SecondResponse ThirdResponse Onset ClusterOnset PossibleWithB
#1      A1  bukl         verb             O           <NA>          <NA>     b           no          TRUE
#2      A1   zup         verb            OB           <NA>          <NA>     z           no          TRUE
#3      A1   tul         verb            OB           <NA>          <NA>     t           no          TRUE
#4      A1   sur         verb             O           <NA>          <NA>     s           no          TRUE
#5      A1 razhn         verb            OB           <NA>          <NA>     r           no          TRUE
#6      A1  guzv         verb             O           <NA>          <NA>     g           no          TRUE
#   Place    Manner StressStimulus Gender Age AgeGroup EducationLevel EducationField SubjectGroup
#1 labial      stop           root   male  59   oldest         Higher        Science            A
#2 dental fricative           root   male  59   oldest         Higher        Science            A
#3 dental      stop           root   male  59   oldest         Higher        Science            A
#4 dental fricative           root   male  59   oldest         Higher        Science            A
#5 dental  sonorant           root   male  59   oldest         Higher        Science            A
#6  velar      stop           root   male  59   oldest         Higher        Science            A
                                                                           
#This gives an overview of the database
summary(dat)        

#This activates the package needed for the linear mixed effects model analysis
library(lme4)


################## Verbal and adjectival stimuli together: The role of StimulusType #################

######################
#######BEST OVERALL MODEL:
######################

dat.lmer = lmer(FirstResponse ~ StimulusType + ClusterOnset + Manner + (1|Stem) + (1|Subject), data=dat, family="binomial")
print(dat.lmer, corr=FALSE)

#Generalized linear mixed model fit by the Laplace approximation 
#Formula: FirstResponse ~ StimulusType + ClusterOnset + Manner + (1 | Stem) +      (1 | Subject) 
#  Data: dat 
#  AIC  BIC logLik deviance
# 2882 2929  -1433     2866
# Random effects:
# Groups  Name        Variance Std.Dev.
# Subject (Intercept) 1.05713  1.02817 
# Stem    (Intercept) 0.14785  0.38451 
# Number of obs: 2630, groups: Subject, 60; Stem, 46

# Fixed effects:
#                 Estimate Std. Error z value Pr(>|z|)    
#(Intercept)       -1.1771     0.3195  -3.684  0.00023 ***
#StimulusTypeverb   1.3807     0.2830   4.879 1.07e-06 ***
#ClusterOnsetyes   -0.6723     0.2085  -3.225  0.00126 ** 
#Mannerfricative   -0.1066     0.2743  -0.388  0.69771    
#Mannersonorant     1.2565     0.2975   4.223 2.41e-05 ***
#Mannerstop        -0.4178     0.2829  -1.477  0.13970    
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

#This creates a plot of the three relevant factors for the choice of the prefix OB-:
library(languageR)
par(mar=c(5,4,1,1), oma=c(1,1,1,1))
ylimit=c(0,1)
par(mfrow=c(1,3))
plotLMER.fnc(dat.lmer, pred="ClusterOnset", ylimit=ylimit, addlines=TRUE, ylab="p(ob)", cexsize = 0.9)
plotLMER.fnc(dat.lmer, pred="Manner", ylimit=ylimit, addlines=TRUE, ylab="p(ob)", cexsize = 0.9)
plotLMER.fnc(dat.lmer, pred="StimulusType", ylimit=ylimit, addlines=TRUE, ylab="p(ob)", cexsize = 0.9)


###################################### Only verbal stimuli:The role of Stress ##########################

#Now we will look only at the responses to verbal stimuli (A,B) and specifically see whether stress patern of the stimulus (root vs. suffix) had an impact. We create a database datVerb which only includes the data for the verbal stimuli.
datVerb = dat[dat$StimulusType == "verb",]
datVerb$StimulusType = datVerb$StimulusType[drop=TRUE]
datVerb$StressStimulus = datVerb$StressStimulus[drop=TRUE]

# This turns out to be the model:
datVerb.lmer = lmer(FirstResponse ~ ClusterOnset + 
    StressStimulus * Age + Manner + 
    (1|Stem) + (1|Subject), 
    family="binomial", data=datVerb)
print(datVerb.lmer, corr=FALSE)
#  AIC  BIC logLik deviance
# 1545 1596 -762.3     1525
#Random effects:
# Groups  Name        Variance Std.Dev.
# Stem    (Intercept) 0.15019  0.38754 
# Subject (Intercept) 0.92050  0.95943 
#Number of obs: 1327, groups: Stem, 46; Subject, 30
#
#Fixed effects:
#                         Estimate Std. Error z value Pr(>|z|)   
#(Intercept)              -1.02931    0.72403  -1.422  0.15513   
#ClusterOnsetyes          -0.59640    0.23551  -2.532  0.01133 * 
#StressStimulussuffix     -5.12788    2.07949  -2.466  0.01367 * 
#Age                       0.02366    0.02222   1.065  0.28693   
#Mannerfricative           0.14928    0.31595   0.472  0.63659   
#Mannersonorant            1.07911    0.34768   3.104  0.00191 **
#Mannerstop               -0.12425    0.32493  -0.382  0.70216   
#StressStimulussuffix:Age  0.25547    0.08568   2.982  0.00287 **

library(multcomp)
par(mar=c(5,10,2,2), oma=c(3,4,1,1))
#This will draw a plot to visualize the results for onset Manner of articulation
plot(glht(datVerb.lmer, linfct=mcp(Manner="Tukey")))

library(languageR)
par(mar=c(5,4,1,1), oma=c(1,1,1,1))
ylimit=c(0,1)
par(mfrow=c(1,3))
#This will draw a plot for each significant fixed effect factor:
plotLMER.fnc(datVerb.lmer, pred="ClusterOnset", ylimit=ylimit, addlines=TRUE,
    ylab="p(ob)", cexsize = 0.9)
plotLMER.fnc(datVerb.lmer, pred="Manner", ylimit=ylimit, addlines=TRUE,
    ylab="p(ob)", cexsize = 0.9)
plotLMER.fnc(datVerb.lmer, pred="Age", addlines=TRUE, ylimit=ylimit,
    intr=list("StressStimulus", levels(datVerb$StressStimulus), "end"),
    ylab="p(ob)", cexsize = 0.9)
par(mfrow=c(1,1))

############################################################################################


#This shows deviations of each individual stimulus nonce stem as well as each specific subject from the average adjustment (which equals zero) in their preferences in the choice of prefix.
ranef(dat.lmer)

#####################
### NDL
#####################
library(ndl)
#modeling the age effect:

datVerb.young = datVerb[datVerb$Age <= 25,]

datVerb.young.ndl = ndlClassify(FirstResponse ~ ClusterOnset + 
    StressStimulus + Age + Manner + Stem, data=datVerb.young)

datVerb.old = datVerb[datVerb$Age > 26,]
datVerb.old.ndl = ndlClassify(FirstResponse ~ ClusterOnset + 
    StressStimulus + Age + Manner + Stem, data=datVerb.old)

ndlStatistics(datVerb.old.ndl)$C
#[1] 0.8911458
ndlStatistics(datVerb.old.ndl)$accuracy
#[1] 0.7941176
ndlStatistics(datVerb.old.ndl)$crosstable
#    O  OB
#O  31  29
#OB 13 131
ndlStatistics(datVerb.young.ndl)$crosstable
#     O  OB
#O  321 149
#OB 193 276



ndlStatistics(datVerb.young.ndl)$C
#[1] 0.6917548
ndlStatistics(datVerb.young.ndl)$accuracy
#[1] 0.6357827

datVerb.old$Predict = datVerb.old.ndl$activationMatrix[,2]>=0.5

datVerb.old.ndl2 = ndlClassify(Predict~ ClusterOnset + 
    StressStimulus + Age + Manner + Stem, data=datVerb.old)
somers2(datVerb.old.ndl2$activationMatrix[,2], 
    as.numeric(datVerb.old$FirstResponse=="OB"))
#         C        Dxy          n    Missing 
#  0.838831   0.677662 204.000000   0.000000 

m = cbind(datVerb.old.ndl2$weightMatrix[,2],
          datVerb.old.ndl$weightMatrix[,2],
          datVerb.young.ndl$weightMatrix[,2])
colnames(m) = c("oldL", "old", "young")
cor(m)
#           oldL       old     young
#oldL  1.0000000 0.8590483 0.3417775
#old   0.8590483 1.0000000 0.3092846
#young 0.3417775 0.3092846 1.0000000

summary(step(lm(young~oldL+old, data=data.frame(m))))
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)   
#(Intercept)  0.01714    0.01681   1.020  0.31241   
#oldL         0.19882    0.07440   2.672  0.00993 **
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
#
#Residual standard error: 0.124 on 54 degrees of freedom
#Multiple R-squared: 0.1168,     Adjusted R-squared: 0.1005 
#F-statistic: 7.142 on 1 and 54 DF,  p-value: 0.009935 


datVerb.old$Predict2 = datVerb.old.ndl2$activationMatrix[,2]>=0.5
datVerb.old.ndl3 = ndlClassify(Predict2~ ClusterOnset + 
    StressStimulus + Age + Manner + Stem, data=datVerb.old)
somers2(datVerb.old.ndl3$activationMatrix[,2], 
    as.numeric(datVerb.old$FirstResponse=="OB"))
#          C         Dxy           n     Missing 
#  0.8350116   0.6700231 204.0000000   0.0000000 


m = cbind(
          datVerb.old.ndl3$weightMatrix[,2],
          datVerb.old.ndl2$weightMatrix[,2],
          datVerb.old.ndl$weightMatrix[,2],
          datVerb.young.ndl$weightMatrix[,2])
colnames(m) = c("oldLL", "oldL", "old", "young")


summary(step(lm(young~oldLL+old, data=data.frame(m))))
# oldLL
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)   
#(Intercept)  0.01740    0.01664   1.045  0.30051   
#oldLL        0.19825    0.06971   2.844  0.00628 **
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 #

#Residual standard error: 0.1231 on 54 degrees of freedom
#Multiple R-squared: 0.1303,	Adjusted R-squared: 0.1142 
#F-statistic: 8.088 on 1 and 54 DF,  p-value: 0.006281


summary(step(lm(young~oldL+old, data=data.frame(m))))
# oldL
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)   
#(Intercept)  0.01714    0.01681   1.020  0.31241   
#oldL         0.19882    0.07440   2.672  0.00993 **
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

#Residual standard error: 0.124 on 54 degrees of freedom
#Multiple R-squared: 0.1168,	Adjusted R-squared: 0.1005 
#F-statistic: 7.142 on 1 and 54 DF,  p-value: 0.009935 


summary(step(lm(young~oldL+oldLL, data=data.frame(m))))
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)   
<3(Intercept)  0.01740    0.01664   1.045  0.30051   
#oldLL        0.19825    0.06971   2.844  0.00628 **
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

#Residual standard error: 0.1231 on 54 degrees of freedom
#Multiple R-squared: 0.1303,	Adjusted R-squared: 0.1142 
#F-statistic: 8.088 on 1 and 54 DF,  p-value: 0.006281 

###############
###NOTE: To simplify, old “dat2” (merged) = “dat” and old “dat3” (verb data only) = “datVerb” above
###############