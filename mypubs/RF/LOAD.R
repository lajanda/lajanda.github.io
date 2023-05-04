########################################################################
# logistic regression
########################################################################


dat = read.csv("datLOAD.csv",T)
library(rms)
dat.dd = datadist(dat)
options(datadist="dat.dd")
dat.lrm = lrm(CONSTRUCTION~VERB+REDUCED+PARTICIPLE+VERB*PARTICIPLE, data=dat)
dat.lrm
#
#                      Model Likelihood     Discrimination    Rank Discrim.    
#                         Ratio Test            Indexes          Indexes       
#Obs          1920    LR chi2    1738.47    R2       0.796    C       0.964    
# goal         871    d.f.             8    g        4.643    Dxy     0.928    
# theme       1049    Pr(> chi2) <0.0001    gr     103.877    gamma   0.945    
#max |deriv| 2e-08                          gp       0.459    tau-a   0.460    
#                                           Brier    0.076                     
#
#                            Coef    S.E.   Wald Z Pr(>|Z|)
#Intercept                   -0.9465 0.2023 -4.68  <0.0001 
#VERB=po                      6.7143 1.0220  6.57  <0.0001 
#VERB=za                      1.0920 0.2451  4.45  <0.0001 
#VERB=_zero                   2.3336 0.2446  9.54  <0.0001 
#REDUCED=yes                 -0.8891 0.1748 -5.09  <0.0001 
#PARTICIPLE=yes              -4.1862 1.0220 -4.10  <0.0001 
#VERB=po * PARTICIPLE=yes     3.8953 1.5978  2.44  0.0148  
#VERB=za * PARTICIPLE=yes     1.4087 1.0774  1.31  0.1910  
#VERB=_zero * PARTICIPLE=yes -1.7717 1.4415 -1.23  0.2190  

##########################
##### table showing accuracy of lrm model inserted by LAURA
#####################
probabilityTheme = plogis(predict(dat.lrm))
tab = table(dat$CONSTRUCTION=="theme", probabilityTheme >= 0.5)
tab
#			FALSE TRUE
#  FALSE   728  143
#  TRUE     77  972

sum(diag(tab))/sum(tab)
#[1] 0.8854167
##############################

dat.glm1 = glm(CONSTRUCTION~VERB+REDUCED+PARTICIPLE,
    data=dat, family="binomial")
dat.glm2 = glm(CONSTRUCTION~VERB+REDUCED+PARTICIPLE+VERB*PARTICIPLE, 
    data=dat, family="binomial")
anova(dat.glm1, dat.glm2, test="Chisq")
#  Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
#1      1914     928.19                          
#2      1911     906.69  3   21.501 8.284e-05 ***

dat.glm0 = glm(CONSTRUCTION~1,
    data=dat, family="binomial")
dat.glm1 = glm(CONSTRUCTION~VERB,
    data=dat, family="binomial")
dat.glm2 = glm(CONSTRUCTION~VERB+PARTICIPLE,
    data=dat, family="binomial")
dat.glm3 = glm(CONSTRUCTION~VERB*PARTICIPLE,
    data=dat, family="binomial")
dat.glm4 = glm(CONSTRUCTION~VERB*PARTICIPLE+REDUCED,
    data=dat, family="binomial")
anova(dat.glm0, dat.glm1, dat.glm2, dat.glm3, dat.glm4, test="Chisq")
#  Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
#1      1919    2645.16                          
#2      1916    1305.31  3  1339.85 < 2.2e-16 ***
#3      1915     950.73  1   354.58 < 2.2e-16 ***
#4      1912     933.48  3    17.25  0.000628 ***
#5      1911     906.69  1    26.80 2.257e-07 ***

AIC(dat.glm0)
AIC(dat.glm1)
AIC(dat.glm2)
AIC(dat.glm3)
AIC(dat.glm4)

AIC(dat.glm0)
#[1] 2647.159
AIC(dat.glm1)
#[1] 1313.312
AIC(dat.glm2)
#[1] 960.7343
AIC(dat.glm3)
#[1] 949.4849
AIC(dat.glm4)
#[1] 924.6854

summary(dat.glm4)
#
#Call:
#glm(formula = CONSTRUCTION ~ VERB * PARTICIPLE + REDUCED, family = "binomial", 
#    data = dat)
#
#Deviance Residuals: 
#    Min       1Q   Median       3Q      Max  
#-3.1261  -0.2414   0.0790   0.0914   3.2058  

#Coefficients:
#                       Estimate Std. Error z value Pr(>|z|)    
#(Intercept)             -0.9465     0.2023  -4.679 2.88e-06 ***
#VERBpo                   6.7143     1.0220   6.570 5.03e-11 ***
#VERBza                   1.0920     0.2451   4.455 8.40e-06 ***
#VERBzero                 2.3336     0.2446   9.539  < 2e-16 ***
#PARTICIPLEyes           -4.1862     1.0220  -4.096 4.20e-05 ***
#REDUCEDyes              -0.8891     0.1748  -5.085 3.67e-07 ***
#VERBpo:PARTICIPLEyes     3.8953     1.5978   2.438   0.0148 *  
#VERBza:PARTICIPLEyes     1.4087     1.0774   1.308   0.1910    
#VERBzero:PARTICIPLEyes  -1.7717     1.4415  -1.229   0.2190    
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

#(Dispersion parameter for binomial family taken to be 1)

#    Null deviance: 2645.16  on 1919  degrees of freedom
#Residual deviance:  906.69  on 1911  degrees of freedom
#AIC: 924.69

#Number of Fisher Scoring iterations: 8



###############################
##trees & forests############

library(party)
library(lattice)
dat = read.csv("datLOAD.csv",T)
head(dat)
#  CONSTRUCTION VERB REDUCED PARTICIPLE
#1        theme zero      no         no
#2        theme zero      no         no
#3        theme zero      no         no
#4        theme zero      no         no
#5        theme zero      no         no
#6        theme zero      no         no
dat.ctree = ctree(CONSTRUCTION ~ VERB + REDUCED + PARTICIPLE, data=dat)
plot(dat.ctree)
dat.ctree
#
#
#1) VERB == {na, za, zero}; criterion = 1, statistic = 1058.718
#  2) PARTICIPLE == {yes}; criterion = 1, statistic = 372.91
#    3) VERB == {na, zero}; criterion = 0.973, statistic = 9.429
#      4)*  weights = 328 
#    3) VERB == {za}
#      5)*  weights = 248 
#  2) PARTICIPLE == {no}
#    6) VERB == {na, za}; criterion = 1, statistic = 102.05
#      7) VERB == {na}; criterion = 1, statistic = 18.133
#        8)*  weights = 147 
#      7) VERB == {za}
#        9)*  weights = 208 
#    6) VERB == {zero}
#      10) REDUCED == {no}; criterion = 1, statistic = 14.429
#        11)*  weights = 169 
#      10) REDUCED == {yes}
#        12)*  weights = 117 
#1) VERB == {po}
#  13) REDUCED == {no}; criterion = 1, statistic = 27.644
#    14)*  weights = 634 
#  13) REDUCED == {yes}
#    15)*  weights = 69 

##############
###HERE ARE THE ACTUAL NUMBERS THAT THE PLOTS AT THE TERMINAL NODES OF THE CTREE INDICATE:
#################
locations=where(dat.ctree)
table(dat$CONSTRUCTION, locations)
#       locations
#          4   5   8   9  11  12  14  15
#  goal  326 237 113 114  32  46   0   3
#  theme   2  11  34  94 137  71 634  66




dat.cforest = cforest(CONSTRUCTION ~ VERB + REDUCED + PARTICIPLE, data=dat)

dat.cforest.varimp = varimp(dat.cforest)
dotplot(sort(dat.cforest.varimp))

dat.trp=treeresponse(dat.cforest)
head(dat.trp)
#$`1`
#     CONSTRUCTION.goal CONSTRUCTION.theme
#[1,]         0.1892296          0.8107704
#
#$`2`
#     CONSTRUCTION.goal CONSTRUCTION.theme
#[1,]         0.1892296          0.8107704
#
#$`3`
#     CONSTRUCTION.goal CONSTRUCTION.theme
#[1,]         0.1892296          0.8107704
#
#$`4`
#     CONSTRUCTION.goal CONSTRUCTION.theme
#[1,]         0.1892296          0.8107704
#
#$`5`
#     CONSTRUCTION.goal CONSTRUCTION.theme
#[1,]         0.1892296          0.8107704
#
#$`6`
#     CONSTRUCTION.goal CONSTRUCTION.theme
#[1,]         0.1892296          0.8107704
#



constructions = dat$CONSTRUCTION
constructions01 = as.numeric(dat$CONSTRUCTION)-1

x = treeresponse(dat.cforest)
probabilityTheme = sapply(x, FUN=function(v)return(v[2]))
somers2(probabilityTheme, constructions01)

#           C          Dxy            n      Missing 
#   0.9638467    0.9276934 1920.0000000    0.0000000 
tab = table(dat$CONSTRUCTION, probabilityTheme >= 0.5)
tab       
#        FALSE TRUE
#  goal    728  143
#  theme    77  972
sum(diag(tab))/sum(tab)
# [1] 0.8854167


############################## ndl ####################################


# for ndl, we need all columns to be character vectors, no factors!

dat = read.csv("datLOAD.csv",T, stringsAsFactors=FALSE)

library(ndl)

dat.ndl = ndlClassify(CONSTRUCTION~VERB+REDUCED+PARTICIPLE, data=dat)
ndlStatistics(dat.ndl)$C
ndlStatistics(dat.ndl)$crosstable
ndlStatistics(dat.ndl)$accuracy

ndlStatistics(dat.ndl)$C
# [1] 0.9560989
ndlStatistics(dat.ndl)$crosstable
#       goal theme
# goal   790    81
# theme  141   908
ndlStatistics(dat.ndl)$accuracy
# [1] 0.884375

dat.ndl$weightMatrix
#                     goal       theme
#PARTICIPLEno   0.07935739  0.32064261
#PARTICIPLEyes  0.35899493  0.04100507
#REDUCEDno      0.17569209  0.22430791
#REDUCEDyes     0.26266024  0.13733976
#VERBna         0.44976692 -0.24976692
#VERBpo        -0.43792012  0.63792012
#VERBza         0.31892899 -0.11892899
#VERBzero       0.10757652  0.09242348

head(dat.ndl$activationMatrix)

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

x = ndlCrossvalidate(CONSTRUCTION~VERB+REDUCED+PARTICIPLE, data=dat)
summary(x)
#Cross-validation summary statistics
#
#Call:
#ndlCrossvalidate(formula = CONSTRUCTION ~ VERB + REDUCED + PARTICIPLE, 
#    data = dat)
#
#Formula:
#CONSTRUCTION ~ VERB + REDUCED + PARTICIPLE
#
#Number of folds:  10
#N(total):  1920
#N(train):  1728
#N(test):   192
#
#                    Mean    Minimum Maximum
#loglikelihood.null  -132.1    -133  -130.1 
#loglikelihood.model -67.11   -71.9  -59.72 
#deviance.null        264.2   260.1     266 
#deviance.model       134.2   119.4   143.8 
#R2.likelihood       0.4921  0.4582   0.549 
#R2.nagelkerke       0.6577  0.6265  0.7097 
#AIC.model            166.2   151.4   175.8 
#BIC.model            218.3   203.6   227.9 
#C                    0.955  0.9294  0.9785 
#accuracy            0.8844  0.8438  0.9479 
#lambda.prediction   0.7448  0.6667  0.8864 
#tau.classification  0.7664  0.6863  0.8951 
#




