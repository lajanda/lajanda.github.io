############
##
#############

dat=read.csv("datAUX.csv")


table(dat$Epoch, dat$RelClause)
        
#         NoRelCl RelCl
#  early       30    30
#  late       379   113
#  middle      75    75

chisq.test(table(dat$Epoch, dat$RelClause))  
# p-value = 1.236e-11

table(dat$Epoch, dat$Subject)
#        
#         NoSubj Subj
#  early      21   39
#  late      213  279
#  middle     40  110

chisq.test(table(dat$Epoch, dat$Subject))  
# p-value = 0.001007

table(dat$Epoch, dat$TempLoc)
#       
#         FALSE TRUE
#  early     39   21
#  late     222  270
#  middle    79   71
chisq.test(table(dat$Epoch, dat$TempLoc))  
# p-value = 0.007336

######################
######LOGISTIC REGRESSION
##################       
library(rms)
dat.dd = datadist(dat)
options(datadist='dat.dd')
dat.lrm = lrm(Aux ~ Subject+TempLoc + SpatAdv*Reflexive + Subject*TempLoc + 
RelClause + Epoch, data=dat)
dat.lrm
#
#Logistic Regression Model
#
#lrm(formula = Aux ~ Subject + TempLoc + SpatAdv * Reflexive + 
#    Subject * TempLoc + RelClause + Epoch, data = dat)
#
#                      Model Likelihood     Discrimination    Rank Discrim.    
#                         Ratio Test            Indexes          Indexes       
#Obs           702    LR chi2     160.60    R2       0.273    C       0.754    
# Aux          362    d.f.             9    g        1.266    Dxy     0.508    
# NoAux        340    Pr(> chi2) <0.0001    gr       3.545    gamma   0.529    
#max |deriv| 5e-14                          gp       0.261    tau-a   0.254    
#                                           Brier    0.199                     
#
#                                      Coef    S.E.   Wald Z Pr(>|Z|)
#Intercept                             -0.5305 0.3422 -1.55  0.1211  
#Subject=Subj                           0.3930 0.2520  1.56  0.1188  
#TempLoc                                1.6567 0.2965  5.59  <0.0001 
#SpatAdv=SpatAdv                        0.4125 0.2136  1.93  0.0535  
#Reflexive=Reflexive                    0.5650 0.3436  1.64  0.1001  
#RelClause=RelCl                       -0.7116 0.1942 -3.66  0.0002  
#Epoch=late                             0.3020 0.3048  0.99  0.3218  
#Epoch=middle                          -1.1366 0.3548 -3.20  0.0014  
#SpatAdv=SpatAdv * Reflexive=Reflexive -2.5188 0.7121 -3.54  0.0004  
#Subject=Subj * TempLoc                -1.6940 0.3597 -4.71  <0.0001 

tab=table(dat$Aux, predict(dat.lrm, type="fitted")>0.5)
tab
sum(diag(tab))/sum(tab)

############################
#TREE & FOREST
############################
library(party)
library(lattice)
dat.ctree=ctree(Aux ~ Voice + Number + Subject + Reflexive + TempLoc + Aspect + SpatAdv + Epoch + RelClause, data=dat)
plot(dat.ctree)
dat.ctree
#1) Epoch == {early, middle}; criterion = 1, statistic = 68.688
#  2) RelClause == {RelCl}; criterion = 0.995, statistic = 12.008
#    3)*  weights = 105 
#  2) RelClause == {NoRelCl}
#    4) Number == {Pl}; criterion = 0.99, statistic = 10.629
#      5)*  weights = 28 
#    4) Number == {Sg}
#      6)*  weights = 77 
#1) Epoch == {late}
#  7) TempLoc <= 0; criterion = 0.998, statistic = 13.528
#    8) Voice == {Passive}; criterion = 0.993, statistic = 11.398
#      9)*  weights = 35 
#    8) Voice == {Active}
#      10)*  weights = 187 
#  7) TempLoc > 0
#    11) Subject == {Subj}; criterion = 1, statistic = 23.315
#      12)*  weights = 131 
#    11) Subject == {NoSubj}
#      13) Voice == {Active}; criterion = 1, statistic = 16.619
#       14)*  weights = 78 
#      13) Voice == {Passive}
#        15)*  weights = 61 


dat.cforest=cforest(Aux ~ Voice + Number + Subject + Reflexive + TempLoc + Aspect + SpatAdv + Epoch + RelClause, data=dat)
dat.cforest.varimp = varimp(dat.cforest)
dotplot(sort(dat.cforest.varimp))
###################
#####Here we test the C and accuracy of the tree & forest:
#############
aux=dat$Aux
aux01=as.numeric(dat$Aux)-1
x = treeresponse(dat.cforest)
probabilityNoAux = sapply(x, FUN=function(v)return(v[2]))
library(rms)
somers2(probabilityNoAux, aux01)
#  0.8071701   0.6143403 702.0000000   0.0000000 
tab = table(dat$Aux, probabilityNoAux >= 0.5)
tab
       
#        FALSE TRUE
#  Aux     247  115
#  NoAux    80  260
sum(diag(tab))/sum(tab)
#[1] 0.7222222

##############################
######NDL: HARALD SHOULD RUN THIS AGAIN ON THIS CORRECT DATASET
##############################

dat=read.csv("datAUX.csv",stringsAsFactors=FALSE)
rownames(dat)=1:nrow(dat)
write.table(dat, file="datAuxCleaned.csv", sep=",", quote=FALSE, row.names=FALSE)


dat=read.csv("datAuxCleaned.csv",stringsAsFactors=FALSE)
dat.ndl  = ndlClassify(Aux~.,data=dat)
summary(dat.ndl)
#
#Call:
#ndlClassify(formula = Aux ~ ., data = dat)
#
#Formula:
#Aux ~ X + Voice + Number + Subject + Reflexive + TempLoc + Aspect + 
#    SpatAdv + Epoch + RelClause
#<environment: 0x8f40588>
#
#Weights:
#                            Aux     NoAux
#AspectImpf             0.046835  0.056614
#AspectPf               0.082255  0.021193
#Epochearly             0.004298  0.064667
#Epochlate             -0.078189  0.147154
#Epochmiddle            0.202980 -0.134014
#NumberPl               0.109269 -0.005821
#NumberSg               0.019820  0.083628
#ReflexiveNonreflexive  0.043617  0.059831
#ReflexiveReflexive     0.085473  0.017976
#RelClauseNoRelCl      -0.020070  0.123519
#... [ omitted 11 rows ] ...
#
#Null deviance:              972.5  on  1404  degrees of freedom
#Residual (model) deviance:  865.4  on  1362  degrees of freedom
#
#R2.likelihood:  0.1101            
#AIC:            949.4             
#BIC:            1141             

ndlStatistics(dat.ndl)$C
#[1] 0.7346644
ndlStatistics(dat.ndl)$accuracy
#[1] 0.6452991


dat.ndlcv = ndlCrossvalidate(Aux~., data=dat, method="awk")
summary(dat.ndlcv)
#
#Cross-validation summary statistics
#
#Call:
#ndlCrossvalidate(formula = Aux ~ ., data = dat, method = "awk")
#
#Formula:
#Aux ~ .
#
#Number of folds:  10
#N(total):  702
#N(train):  631
#N(test):   71
#
#                    Mean     Minimum  Maximum 
#loglikelihood.null    -48.99   -49.21   -48.64
#loglikelihood.model   -44.49   -50.92   -38.55
#deviance.null          97.97    97.28    98.41
#deviance.model         88.98    77.09    101.8
#R2.likelihood         0.0918 -0.03473   0.2112
#R2.nagelkerke         0.1537 -0.06576   0.3375
#AIC.model                173    161.1    185.8
#BIC.model                268    256.1    280.9
#C                     0.7123   0.6238   0.7849
#accuracy              0.6169    0.507   0.6901
#lambda.prediction     0.1744 -0.09375   0.3125
#tau.classification    0.2288 0.004407   0.3742

