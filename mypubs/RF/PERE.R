### logistic regression
library(rms)
dat = read.csv("datPERE.csv",T)
dat$LogFreqPrefVerb  = log(dat$FreqPrefVerb+1)
dat.dd = datadist(dat)
options(datadist="dat.dd")
dat.lrm = lrm(Prefix ~ ShiftTrans + PrefixStacking + ShiftAspect + 
    PerfectiveType + SemanticGroup + LogFreqPrefVerb, data=dat) 
dat.lrm
#                      Model Likelihood     Discrimination    Rank Discrim.    
#                         Ratio Test            Indexes          Indexes       
#Obs          1834    LR chi2     437.28    R2       0.591    C       0.950    
# pere        1727    d.f.            26    g        7.137    Dxy     0.900    
# pre          107    Pr(> chi2) <0.0001    gr    1257.125    gamma   0.902    
#max |deriv| 0.002                          gp       0.100    tau-a   0.099    
#                                           Brier    0.028                  
                              Coef     S.E.       Wald Z Pr(>|Z|)
#Intercept                     -13.3521   206.3505 -0.06  0.9484  
#ShiftTrans=intr-tr             -0.8535     0.6208 -1.37  0.1691  
#ShiftTrans=no-intr              9.9114    97.3326  0.10  0.9189  
#ShiftTrans=no-tr                8.8622    97.3304  0.09  0.9275  
#ShiftTrans=tr-intr             -0.2529     0.8604 -0.29  0.7688  
#ShiftTrans=tr-tr               -0.7114     0.3516 -2.02  0.0430  
#PrefixStacking=stacked          2.7981     0.4944  5.66  <0.0001 
#ShiftAspect=imp-pf             -1.1993     1.1889 -1.01  0.3131  
#ShiftAspect=no-imp            -11.9414    97.3314 -0.12  0.9024  
#ShiftAspect=no-pf             -10.4717    97.3389 -0.11  0.9143  
#ShiftAspect=pf-pf              -0.3693     1.2061 -0.31  0.7594  
#PerfectiveType=not applicable  11.3181   206.3495  0.05  0.9563  
#PerfectiveType=specialized     11.0768   206.3462  0.05  0.9572  
#SemanticGroup=div               0.1514     0.6095  0.25  0.8038  
#SemanticGroup=intrch           -1.8945     0.8023 -2.36  0.0182  
#SemanticGroup=mix             -11.1531   137.9996 -0.08  0.9356  
#SemanticGroup=ovc-dur          -0.8701     0.6821 -1.28  0.2021  
#SemanticGroup=overdo           -3.1482     0.7311 -4.31  <0.0001 
#SemanticGroup=redo            -13.2123    31.3180 -0.42  0.6731  
#SemanticGroup=seria           -11.4999    56.0448 -0.21  0.8374  
#SemanticGroup=super            -0.1728     0.6935 -0.25  0.8032  
#SemanticGroup=thorough        -11.5533   154.1634 -0.07  0.9403  
#SemanticGroup=transf           -2.4456     0.6330 -3.86  0.0001  
#SemanticGroup=transf_met        0.2987     0.5502  0.54  0.5872  
#SemanticGroup=turn            -11.6393   150.1876 -0.08  0.9382  
#SemanticGroup=very             22.5995 43383.5286  0.00  0.9996  
#LogFreqPrefVerb                 0.3725     0.0645  5.77  <0.0001 


#### PerfectiveType is not significant, we remove it (I've checked this
#### also with AIC and model comparisons)

dat.lrm = lrm(Prefix ~ ShiftTrans + PrefixStacking + ShiftAspect + 
    PerfectiveType + SemanticGroup + LogFreqPrefVerb, data=dat) 
dat.lrm


probabilityPre = plogis(predict(dat.lrm))
tab = table(dat$Prefix=="pre", probabilityPre >= 0.5)
tab
#        FALSE TRUE
#  FALSE  1718    9
#  TRUE     57   50

sum(diag(tab))/sum(tab)
# [1] 0.9640131
table(dat$Prefix)
######comparison with a default model that always assumes pere
##Default model is correct 94% of the time
#pere  pre 
#1727  107 
1-(107/nrow(dat))
# [1] 0.9416576

prop.test(c(107, 67), rep(nrow(dat),2))
#        2-sample test for equality of proportions with continuity correction
#
#data:  c(107, 67) out of rep(nrow(dat), 2) 
#X-squared = 9.1767, df = 1, p-value = 0.002451
#alternative hypothesis: two.sided 
#95 percent confidence interval:
# 0.00752464 0.03609586 
#sample estimates:
#    prop 1     prop 2 
#0.05834242 0.03653217 




dat.glm0 = glm(Prefix ~ 1, data=dat, family="binomial") 
dat.glm1 = glm(Prefix ~ ShiftTrans, data=dat, family="binomial") 
dat.glm2 = glm(Prefix ~ ShiftTrans + PrefixStacking, data=dat, 
    family="binomial") 
dat.glm3 = glm(Prefix ~ ShiftTrans + PrefixStacking + ShiftAspect , 
    data=dat, 
    family="binomial") 
dat.glm4 = glm(Prefix ~ ShiftTrans + PrefixStacking + ShiftAspect + 
    SemanticGroup, data=dat, 
    family="binomial") 
dat.glm5 = glm(Prefix ~ ShiftTrans + PrefixStacking + ShiftAspect + 
    SemanticGroup + LogFreqPrefVerb, data=dat, 
    family="binomial") 


anova(dat.glm0, dat.glm1, dat.glm2, dat.glm3, dat.glm4, dat.glm5,
      test="Chisq")
aics = c(AIC(dat.glm0), AIC(dat.glm1), AIC(dat.glm2), AIC(dat.glm3), 
         AIC(dat.glm4), AIC(dat.glm5))
abs(diff(aics))

  Resid. Df Resid. Dev Df Deviance  Pr(>Chi) Red in AIC Predictor   
1      1833     815.70                          
2      1828     789.17  5   26.527 7.051e-05 16.52680  ShiftTrans
3      1827     739.16  1   50.007 1.532e-12 48.00715  PrefixStacking
4      1823     694.90  4   44.260 5.665e-09 36.26014  ShiftAspect
5      1810     415.90 13  279.000 < 2.2e-16 253.00036 SemanticGroup
6      1809     379.56  1   36.339 1.658e-09 34.33874  LogPrefixVerb

SemGr > PrefSt > ShiftA > LogPrefVerb > ShiftTr


#################6#######################################################
# TREE & FOREST
########################################################################
library(party)
library(lattice)
dat = read.csv("datPERE.csv",T)
head(dat)
dat.ctree = ctree(Prefix ~ ShiftTrans + PrefixStacking + ShiftAspect + 
    PerfectiveType + SemanticGroup + FreqBase + FreqPrefVerb, data=dat) 
dat.ctree

#	 Conditional inference tree with 12 terminal nodes

#Response:  Prefix 
#Inputs:  ShiftTrans, PrefixStacking, ShiftAspect, PerfectiveType, SemanticGroup, FreqBase, #FreqPrefVerb 
#Number of observations:  1834 

#1) SemanticGroup == {bridge, div, intrch, mix, ovc-dur, overdo, redo, seria, super, #thorough, transf, turn}; criterion = 1, statistic = 399.589
#  2) SemanticGroup == {intrch, mix, overdo, redo, seria, thorough, transf, turn}; criterion #= 1, statistic = 93.294
#    3) PrefixStacking == {not stacked}; criterion = 1, statistic = 27.473
#      4)*  weights = 1053 
#    3) PrefixStacking == {stacked}
#      5) SemanticGroup == {transf}; criterion = 1, statistic = 50.621
#        6)*  weights = 27 
#      5) SemanticGroup == {intrch, mix, overdo, redo, seria, thorough, turn}
#        7) FreqPrefVerb <= 331; criterion = 1, statistic = 22.351
#          8)*  weights = 261 
#        7) FreqPrefVerb > 331
#          9)*  weights = 7 
#  2) SemanticGroup == {bridge, div, ovc-dur, super}
#    10) ShiftAspect == {imp-pf, no-imp, no-pf}; criterion = 1, statistic = 80.013
#      11) FreqPrefVerb <= 948; criterion = 0.981, statistic = 12.427
#        12) SemanticGroup == {div, ovc-dur, super}; criterion = 0.991, statistic = 15.639
#          13)*  weights = 215 
#        12) SemanticGroup == {bridge}
#          14) FreqBase <= 116; criterion = 0.968, statistic = 7.997
#            15)*  weights = 22 
#          14) FreqBase > 116
#            16)*  weights = 23 
#      11) FreqPrefVerb > 948
#        17)*  weights = 22 
#    10) ShiftAspect == {imp-imp, pf-pf}
#      18)*  weights = 43 
#1) SemanticGroup == {transf_met, very}
#  19) PrefixStacking == {stacked}; criterion = 1, statistic = 70.66
#    20)*  weights = 32 
#  19) PrefixStacking == {not stacked}
#    21) ShiftAspect == {imp-imp}; criterion = 0.992, statistic = 18.306
#      22)*  weights = 13 
#    21) ShiftAspect == {imp-pf, no-imp, no-pf, pf-pf}
#      23)*  weights = 116 

plot(dat.ctree)


locations = where(dat.ctree)
table(dat$Prefix,locations)
#      locations
#          4    6    8    9   13   15   16   17   18   20   22   23
#  pere 1047   19  257    5   22   17  211   17   22    2    6  102
#  pre     6    8    4    2    0    6    4    5   21   30    7   14

x = treeresponse(dat.ctree)
probabilityPreCtree = sapply(x, FUN=function(v)return(v[2]))
prefixes = dat$Prefix
prefixes01 = as.numeric(dat$Prefix)-1
somers2(probabilityPreCtree, prefixes01)
#           C          Dxy            n      Missing 
#   0.9242893    0.8485786 1834.0000000    0.0000000 

dat.cforest = cforest(Prefix ~ ShiftTrans + PrefixStacking + ShiftAspect + 
    PerfectiveType + SemanticGroup + FreqBase + FreqPrefVerb, data=dat) 

dat.cforest.varimp = varimp(dat.cforest)
library(lattice)
dotplot(sort(dat.cforest.varimp))

x = treeresponse(dat.cforest)
is.list(x)
#[1] TRUE
probabilityPre = sapply(x, FUN=function(v)return(v[2]))
tab = table(dat$Prefix=="pre", probabilityPre >= 0.5)
tab
#
#       
#        FALSE TRUE
#  FALSE  1725    2
#  TRUE     65   42
#
sum(diag(tab))/sum(tab)
#[1] 0.9645583

table(dat$Prefix)

probabilityPreCforest = sapply(x, FUN=function(v)return(v[2]))
prefixes = dat$Prefix
prefixes01 = as.numeric(dat$Prefix)-1
somers2(probabilityPreCforest, prefixes01)
#           C          Dxy            n      Missing 
#   0.9758995    0.9517991 1834.0000000    0.0000000 

dat.cforest = cforest(Prefix ~ ShiftTrans + PrefixStacking + ShiftAspect + 
    PerfectiveType + SemanticGroup + FreqBase + FreqPrefVerb, data=dat) 

dat.cforest.varimp = varimp(dat.cforest)
dotplot(sort(dat.cforest.varimp))
sort(dat.cforest.varimp)
#    ShiftTrans PerfectiveType       FreqBase   FreqPrefVerb 
# -2.967359e-06   9.792285e-05   4.540059e-04   3.290801e-03 
#   ShiftAspect PrefixStacking  SemanticGroup 
#  1.292878e-02   1.775964e-02   3.802967e-02 

x = treeresponse(dat.cforest)
is.list(x)
#[1] TRUE
probabilityPre = sapply(x, FUN=function(v)return(v[2]))
tab = table(dat$Prefix=="pre", probabilityPre >= 0.5)
tab
#	       FALSE TRUE
#  FALSE  1725    2
#  TRUE     62   45
 sum(diag(tab))/sum(tab)
#[1] 0.9651036



#############################  naive discrimination learning ###############################

library(ndl)
dat = read.csv("datPERE.csv",T, stringsAsFactors=FALSE)

dat.w = estimateWeights(dat, method="awk")
dat.a = estimateActivations(dat, dat.w)
#head(dat.a$activationMatrix)   # works only for newest ndl package version
head(dat.a)
#           pre         pere
#[1,] 1.0822103 -0.082210344
#[2,] 1.0948455 -0.094845519
#[3,] 1.0016727 -0.001672675
#[4,] 0.9886311  0.011368950
#[5,] 1.0948455 -0.094845519
#[6,] 1.0818039 -0.081803894
#### dat.a = dat.a$activationMatrix
dat.a2 = dat.a + abs(min(dat.a))
rsums = apply(dat.a2, 1, sum)
dat.p = dat.a2/cbind(rsums, rsums)

tab = table(dat$Prefix, dat.p[,1] >= 0.5)
tab
#       FALSE TRUE
#  pere  1646   81
#  pre     50   57
sum(diag(tab))/sum(tab)
# [1] 0.9285714

# now weight things properly for how often they occur

dat$FreqPre = (dat.p[,1] >= 0.5) * dat$Frequency
dat$FreqPere = (dat.p[,2] > 0.5) * dat$Frequency
dat$Counts = as.numeric(dat$PrePredicted)*dat$Frequency

tab1 = tapply(dat$FreqPre, list(dat$Prefix, dat$PrePredicted), sum)
#     FALSE  TRUE
#pere     0 12083
#pre      0 92206
tab2 = tapply(dat$FreqPere, list(dat$Prefix, dat$PrePredicted), sum)
#      FALSE TRUE
#pere 637674    0
#pre   33462    0

tab = cbind(tab2[,1], tab1[,2])
sum(diag(tab))/sum(tab)
# [1] 0.9412645

# baseline:
cnts = tapply(dat$Frequency, dat$Prefix, sum)
cnts
#  pere    pre 
#649757 125668 
#
# error rate for just choosing the majority outcome:
cnts["pre"]/sum(cnts)
#      pre 
#0.1620634 
1-cnts["pre"]/sum(cnts)
#      pre 
#0.8379366





dat.ndl = ndlClassify(Prefix~PrefixStacking+ShiftAspect+SemanticGroup+
    ShiftTrans+PerfectiveType, frequency=dat$Frequency, data=dat)

dat.ndl$weightMatrix
#                                    pere          pre
#PerfectiveTypenatural        0.243320426  0.018851858
#PerfectiveTypenotapplicable  0.274290022 -0.012117737
#PerfectiveTypespecialized    0.024580706  0.237591578
#PrefixStackingnotstacked     0.438123271 -0.044864844
#PrefixStackingstacked        0.104067883  0.289190544
#SemanticGroupbridge          0.080884152 -0.024704376
#SemanticGroupdiv            -0.098682022  0.154861798
#SemanticGroupintrch          0.191655239 -0.135475463
#SemanticGroupmix             0.159638338 -0.103458562
#SemanticGroupovc-dur         0.104022625 -0.047842850
#SemanticGroupoverdo          0.135057385 -0.078877610
#SemanticGroupredo            0.218870986 -0.162691210
#SemanticGroupseria           0.175455563 -0.119275788
#SemanticGroupsuper          -0.332667571  0.388847346
#SemanticGroupthorough        0.189313657 -0.133133882
#SemanticGrouptransf          0.217845517 -0.161665742
#SemanticGrouptransf.met     -0.284965846  0.341145621
#SemanticGroupturn            0.188831901 -0.132652126
#SemanticGroupvery           -0.403068770  0.459248545
#ShiftAspectimp-imp          -0.153115270  0.310418641
#ShiftAspectimp-pf            0.270182035 -0.112878664
#ShiftAspectno-imp            0.013344554  0.143958817
#ShiftAspectno-pf             0.222012946 -0.064709576
#ShiftAspectpf-pf             0.189766889 -0.032463518
#ShiftTransintr-intr          0.082980617  0.048105526
#ShiftTransintr-tr            0.121317523  0.009768619
#ShiftTransno-intr            0.134760281 -0.003674139
#ShiftTransno-tr              0.104667544  0.026418598
#ShiftTranstr-intr            0.002442947  0.128643195
#ShiftTranstr-tr              0.096022242  0.035063901


ndlStatistics(dat.ndl)$C
#[1] 0.9652423
ndlStatistics(dat.ndl)$crosstable
#       pere   pre
#pere 637674 12083
#pre   33462 92206
ndlStatistics(dat.ndl)$accuracy
# [1] 0.9412645


ndlStatistics(dat.ndl)
#
#$n.data
#[1] 775425
#
#$df.null
#[1] 1550850
#
#$df.model
#[1] 1550790
#
#$loglikelihood.null
#[1] -343571.9
#
#$loglikelihood.model
#[1] -243555.2
#
#$deviance.null
#[1] 687143.9
#
#$deviance.model
#[1] 487110.4
#
#$R2.likelihood
#[1] 0.2911086
#
#$R2.nagelkerke
#[1] 0.3868559
#
#$AIC.model
#[1] 487230.4
#
#$BIC.model
#[1] 487924.1
#
#$C
#[1] 0.9652423
#
#$crosstable
#       pere   pre
#pere 637674 12083
#pre   33462 92206
#
#$accuracy
#[1] 0.9412645
#
#$recall.predicted
#     pere       pre 
#0.9814038 0.7337270 
#
#$precision.predicted
#     pere       pre 
#0.9501413 0.8841393 
#
#$lambda.prediction
#[1] 0.6375768
#
#$tau.classification
#[1] 0.7837407
#
#$d.lambda.prediction
#[1] 246.9103
#
#$d.tau.classification
#[1] 421.4244
#
#$p.lambda.prediction
#[1] 0
#
#$p.tau.classification
#[1] 0
#
#attr(,"class")
#[1] "ndlStatistics"
#

dat.ndl$weightMatrix
#                                    pere          pre
#PerfectiveTypenatural        0.243320426  0.018851858
#PerfectiveTypenotapplicable  0.274290022 -0.012117737
#PerfectiveTypespecialized    0.024580706  0.237591578
#PrefixStackingnotstacked     0.438123271 -0.044864844
#PrefixStackingstacked        0.104067883  0.289190544
#SemanticGroupbridge          0.080884152 -0.024704376
#SemanticGroupdiv            -0.098682022  0.154861798
#SemanticGroupintrch          0.191655239 -0.135475463
#SemanticGroupmix             0.159638338 -0.103458562
#SemanticGroupovc-dur         0.104022625 -0.047842850
#SemanticGroupoverdo          0.135057385 -0.078877610
#SemanticGroupredo            0.218870986 -0.162691210
#SemanticGroupseria           0.175455563 -0.119275788
#SemanticGroupsuper          -0.332667571  0.388847346
#SemanticGroupthorough        0.189313657 -0.133133882
#SemanticGrouptransf          0.217845517 -0.161665742
#SemanticGrouptransf.met     -0.284965846  0.341145621
#SemanticGroupturn            0.188831901 -0.132652126
#SemanticGroupvery           -0.403068770  0.459248545
#ShiftAspectimp-imp          -0.153115270  0.310418641
#ShiftAspectimp-pf            0.270182035 -0.112878664
#ShiftAspectno-imp            0.013344554  0.143958817
#ShiftAspectno-pf             0.222012946 -0.064709576
#ShiftAspectpf-pf             0.189766889 -0.032463518
#ShiftTransintr-intr          0.082980617  0.048105526
#ShiftTransintr-tr            0.121317523  0.009768619
#ShiftTransno-intr            0.134760281 -0.003674139
#ShiftTransno-tr              0.104667544  0.026418598
#ShiftTranstr-intr            0.002442947  0.128643195
#ShiftTranstr-tr              0.096022242  0.035063901



wm=dat.ndl$weightMatrix

# visualization of differences between the weights
dotplot(sort(abs(wm[,2]-wm[,1])), xlab="discrimination value")

# variable importance

diffs = abs(wm[,2]-wm[,1])
names(diffs)=substr(names(diffs), 1,4)
diffs
#      PART       PART       REDU       REDU       VERB       VERB       VERB 
#0.24128522 0.31798987 0.04861583 0.12532048 0.69953384 1.07584023 0.43785799 
#      VERB 
#0.01515305 
diffs.dfr = data.frame(Factor=names(diffs), Diffs=as.numeric(diffs))
summedDiffs=tapply(diffs.dfr$Diffs, diffs.dfr$Factor, sum)
summedDiffs     
#     PART      REDU      VERB 
#0.5592751 0.1739363 2.2283851 
names(summedDiffs)=c("Participle", "Reduction", "Verb")
dotplot(sort(summedDiffs), xlab="summed discrimination")

# the next cross-validation takes heaps of time
dat.ndl.cv = ndlCrossvalidate(Prefix~PrefixStacking+ShiftAspect+SemanticGroup+
    ShiftTrans+PerfectiveType, frequency=dat$Frequency, data=dat)

summary(dat.ndl.cv)
#Number of folds:  10
#N(total):  775425
#N(train):  697882
#N(test):   77543
#
#                    Mean    Minimum Maximum
#loglikelihood.null   -34360  -34650  -33940
#loglikelihood.model  -28660  -31120  -27060
#deviance.null         68710   67880   69290
#deviance.model        57310   54130   62250
#R2.likelihood         0.166 0.09734  0.2162
#R2.nagelkerke        0.2321  0.1408  0.2971
#AIC.model             57430   54240   62360
#BIC.model             57980   54760   62900
#C                    0.8602   0.817  0.9182
#accuracy             0.8408  0.8366  0.8452
#lambda.prediction   0.01773       0 0.02651
#tau.classification   0.4139  0.4023  0.4206




dat.ndl.cv1 = ndlCrossvalidate(Prefix~PrefixStacking+ShiftAspect+SemanticGroup+ShiftTrans+PerfectiveType,
data=dat, method="awk")
summary(dat.ndl.cv1)
#
#Cross-validation summary statistics
#
#Call:
#ndlCrossvalidate(formula = Prefix ~ PrefixStacking + ShiftAspect + 
#    SemanticGroup + ShiftTrans + PerfectiveType, data = dat, 
#    method = "awk")
#
#Formula:
#Prefix ~ PrefixStacking + ShiftAspect + SemanticGroup + ShiftTrans + 
#    PerfectiveType
#
#Number of folds:  10
#N(total):  1834
#N(train):  1650
#N(test):   184
#
#                    Mean    Minimum Maximum
#loglikelihood.null   -40.84  -49.52  -32.91
#loglikelihood.model  -35.08  -42.65  -30.85
#deviance.null         81.69   65.81   99.03
#deviance.model        70.15   61.71    85.3
#R2.likelihood         0.132 0.02357   0.262
#R2.nagelkerke        0.1589 0.02792  0.3131
#AIC.model             190.2   181.7   205.3
#BIC.model             383.1   374.6   398.2
#C                    0.8833  0.8089  0.9386
#accuracy             0.9489  0.9239  0.9728
#lambda.prediction    0.1331       0   0.375
#tau.classification   0.5393  0.4588  0.6733


# run this on server
dat.ndl.cv2 = ndlCrossvalidate(Prefix~PrefixStacking+ShiftAspect+SemanticGroup+ShiftTrans+PerfectiveType, frequency=dat$Frequency,
data=dat, method="awk")

dat.ndl.cv3 = ndlCrossvalidate(Prefix~PrefixStacking+ShiftAspect+SemanticGroup+
    ShiftTrans+PerfectiveType + Frequency,
data=dat, method="awk")
summary(dat.ndl.cv3)
#C                    0.8852  0.7042  0.9711
#accuracy             0.9505  0.9239  0.9837




####

dat$ndlAct = dat.ndl$activationMatrix[,2] > 0.5


dat.ndl2 = ndlClassify(ndlAct~PrefixStacking+ShiftAspect+SemanticGroup+
    ShiftTrans+PerfectiveType, frequency=dat$Frequency, data=dat)
cor.test(dat.ndl2$activationMatrix[,2], dat.ndl$activationMatrix[,2])
#t = 121.0654, df = 1832, p-value < 2.2e-16
#0.942812
cor.test(dat.ndl2$weightMatrix[,2], dat.ndl$weightMatrix[,2])
#t = 12.425, df = 28, p-value = 6.526e-13
#     cor 
# 0.920041



probability = dat.ndl2$activationMatrix[,2]
somers2(probability, prefixes01)
#           C          Dxy            n      Missing 
#   0.8245134    0.6490267 1834.0000000    0.0000000 


######## VARIABLE IMPORTANCE #####################################

wm=dat.ndl$weightMatrix

diffs = abs(wm[,2]-wm[,1])
names(diffs)=substr(names(diffs), 1,8)
diffs
#      PART       PART       REDU       REDU       VERB       VERB       VERB 
#0.24128522 0.31798987 0.04861583 0.12532048 0.69953384 1.07584023 0.43785799 
#      VERB 
#0.01515305 
diffs.dfr = data.frame(Factor=names(diffs), Diffs=as.numeric(diffs))
summedDiffs=tapply(diffs.dfr$Diffs, diffs.dfr$Factor, sum)
summedDiffs     
# Perfecti  PrefixSt  Semantic  ShiftAsp  ShiftTra 
# 0.7238872 0.6681108 5.2248405 1.4861618 0.5502659 

names(summedDiffs)=c("PerfectiveType", "PrefixStacking", "SemanticGroup", "ShiftAspect", "ShiftTrans")

dotplot(sort(summedDiffs), xlab="summed discrimination")

rev(sort(summedDiffs))
 SemanticGroup        5.2248405 
 ShiftAspect          1.4861618
 PerfectiveType       0.7238872
 PrefixStacking       0.6681108   
 ShiftTrans           0.5502659


# check performance when frequency is made into a factor
