dat=read.csv("datNU.csv", T)
head(dat)
#    NU   Form     Prefix    Period     Genre    Rootfinal SemClass   SJA
#1 NoNu   part   Prefixed 1950-1999 massmedia velarplosive InchIntr NoSja
#2 NoNu finite   Prefixed 1800-1849   fiction velarplosive InchIntr NoSja
#3 NoNu finite Unprefixed 1800-1849   fiction velarplosive InchIntr NoSja
#4 NoNu finite   Prefixed 1900-1949 massmedia velarplosive InchIntr NoSja
#5 NoNu mascsg   Prefixed 1850-1899 massmedia velarplosive InchIntr NoSja
#6 NoNu   part   Prefixed 1900-1949   fiction velarplosive InchIntr NoSja

library(rms)

dat.glm=glm(NU~Form+Prefix + Genre + Rootfinal + SemClass + SJA + 
    Period, data=dat, family="binomial")
summary(dat.glm)
#    Min       1Q   Median       3Q      Max  
#-3.2545  -0.2291  -0.1159  -0.0452   3.9404  
#
#Coefficients:
#                         Estimate Std. Error z value Pr(>|z|)    
#(Intercept)              -5.24889    0.34592 -15.174  < 2e-16 ***
#Formgerund                8.35787    0.15084  55.408  < 2e-16 ***
#Formmascsg                2.23666    0.11826  18.913  < 2e-16 ***
#Formpart                  3.97885    0.11979  33.216  < 2e-16 ***
#PrefixUnprefixed          3.08211    0.11329  27.206  < 2e-16 ***
#Genrefiction              1.03740    0.32131   3.229 0.001244 ** 
#Genremassmedia            1.21879    0.32302   3.773 0.000161 ***
#Genremix                  1.06627    0.45953   2.320 0.020321 *  
#Genrenonfiction           1.29996    0.32963   3.944 8.02e-05 ***
#Genreprivat               0.86595    0.39150   2.212 0.026975 *  
#Rootfinaldentalplosive  -10.16946  169.96162  -0.060 0.952288    
#Rootfinallabialplosive   -1.49063    0.11852 -12.577  < 2e-16 ***
#Rootfinalnone            -1.24325    0.30325  -4.100 4.13e-05 ***
#Rootfinalvelarfricative  -1.09527    0.10716 -10.221  < 2e-16 ***
#Rootfinalvelarplosive    -0.95212    0.09194 -10.356  < 2e-16 ***
#SemClassStatIntrans      -0.44538    0.10242  -4.348 1.37e-05 ***
#SemClassTransitive        2.06763    0.09479  21.814  < 2e-16 ***
#SJASja                   -0.55186    0.12157  -4.540 5.64e-06 ***
#Period1850-1899          -0.90956    0.13458  -6.758 1.39e-11 ***
#Period1900-1949          -1.59966    0.12665 -12.630  < 2e-16 ***
#Period1950-1999          -1.97013    0.12724 -15.483  < 2e-16 ***
#Period2000-              -1.90432    0.13109 -14.527  < 2e-16 ***

table(dat$NU)
#
# NoNu    nu 
#31790  2289 
2289/nrow(dat)
#[1] 0.06716746

dat.glm2=glm(NU~Form*Prefix + Genre + Rootfinal + SemClass + SJA + 
    Period, data=dat, family="binomial")
anova(dat.glm, dat.glm2, test="Chisq")
#1     34057     7797.5                          
#2     34055     7497.7  2   299.81 < 2.2e-16 ***

probs=fitted(dat.glm2)
nu01 = as.numeric(dat$NU)-1
somers2(probs, nu01)
#           C          Dxy            n      Missing 
#9.524395e-01 9.048790e-01 3.407900e+04 0.000000e+00 

dat.glm3=glm(NU~Form*Prefix + Genre + Rootfinal + SemClass + SJA + 
    Period + Period*SemClass*Genre, data=dat, family="binomial")
anova(dat.glm2, dat.glm3, test="Chisq")
#  Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
#1     34055     7497.7                          
#2     33981     7252.9 74   244.82 < 2.2e-16 ***
probs=fitted(dat.glm3)
somers2(probs, nu01)
#           C          Dxy            n      Missing 
#9.546143e-01 9.092285e-01 3.407900e+04 0.000000e+00 

summary(dat.glm3)

#Call:
#glm(formula = NU ~ Form * Prefix + Genre + Rootfinal + SemClass + 
#    SJA + Period + Period * SemClass * Genre, family = "binomial", 
#    data = dat)

#Deviance Residuals: 
#    Min       1Q   Median       3Q      Max  
#-3.3463  -0.2237  -0.1110  -0.0542   3.8504  

#Coefficients: (5 not defined because of singularities)
#                                                      Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                                           -7.02317    1.00595  -6.982 2.92e-12 ***
#Formgerund                                             8.03982    0.15323  52.469  < 2e-16 ***
#Formmascsg                                             1.86264    0.12826  14.522  < 2e-16 ***
#Formpart                                               3.42406    0.12621  27.129  < 2e-16 ***
#PrefixUnprefixed                                       1.37322    0.28978   4.739 2.15e-06 ***
#Genrefiction                                           3.38754    1.01846   3.326 0.000881 ***
#Genremassmedia                                         2.33430    1.15921   2.014 0.044042 *  
#Genremix                                               2.72966    1.34745   2.026 0.042785 *  
#Genrenonfiction                                        1.76614    1.14207   1.546 0.122000    
#Genreprivat                                            1.40366    1.18227   1.187 0.235125    
#Rootfinaldentalplosive                               -11.00132  284.12724  -0.039 0.969114    
#Rootfinallabialplosive                                -1.77747    0.13277 -13.388  < 2e-16 ***
#Rootfinalnone                                         -1.59195    0.34983  -4.551 5.35e-06 ***
#Rootfinalvelarfricative                               -1.55420    0.12944 -12.007  < 2e-16 ***
#Rootfinalvelarplosive                                 -1.13334    0.09356 -12.113  < 2e-16 ***
#SemClassStatIntrans                                  -10.62570 1645.66103  -0.006 0.994848    
#SemClassTransitive                                     2.55167    1.35617   1.882 0.059901 .  
#SJASja                                                -0.56690    0.12296  -4.611 4.02e-06 ***
#Period1850-1899                                        1.26838    1.85809   0.683 0.494843    
#Period1900-1949                                        1.93804    1.33255   1.454 0.145840    
#Period1950-1999                                        1.05102    1.16720   0.900 0.367876    
#Period2000-                                            0.86806    1.70199   0.510 0.610034    
#Formgerund:PrefixUnprefixed                                 NA         NA      NA       NA    
#Formmascsg:PrefixUnprefixed                            1.23596    0.32933   3.753 0.000175 ***
#Formpart:PrefixUnprefixed                              5.56044    0.48701  11.417  < 2e-16 ***
#SemClassStatIntrans:Period1850-1899                   -0.27075 2909.64191   0.000 0.999926    
#SemClassTransitive:Period1850-1899                    -1.23539    2.49084  -0.496 0.619913    
#SemClassStatIntrans:Period1900-1949                   15.59670  670.68080   0.023 0.981447    
#SemClassTransitive:Period1900-1949                    -0.87842    1.93082  -0.455 0.649147    
#SemClassStatIntrans:Period1950-1999                   -2.37883 1782.93288  -0.001 0.998935    
#SemClassTransitive:Period1950-1999                    -0.16183    1.65189  -0.098 0.921961    
#SemClassStatIntrans:Period2000-                       -3.20932 2909.64223  -0.001 0.999120    
#SemClassTransitive:Period2000-                         0.91779    2.10375   0.436 0.662647    
#Genrefiction:Period1850-1899                          -2.56304    1.87634  -1.366 0.171946    
#Genremassmedia:Period1850-1899                        -0.77456    1.96047  -0.395 0.692775    
#Genremix:Period1850-1899                             -14.25081  571.73442  -0.025 0.980114    
#Genrenonfiction:Period1850-1899                       -1.35196    1.98154  -0.682 0.495065    
#Genreprivat:Period1850-1899                            0.41854    2.04029   0.205 0.837467    
#Genrefiction:Period1900-1949                          -3.51745    1.35263  -2.600 0.009310 ** 
#Genremassmedia:Period1900-1949                        -2.36264    1.46912  -1.608 0.107792    
#Genremix:Period1900-1949                              -2.09081    1.71954  -1.216 0.224019    
#Genrenonfiction:Period1900-1949                       -1.09491    1.45754  -0.751 0.452532    
#Genreprivat:Period1900-1949                           -0.82065    1.65118  -0.497 0.619182    
#Genrefiction:Period1950-1999                          -2.95309    1.18925  -2.483 0.013023 *  
#Genremassmedia:Period1950-1999                        -1.58236    1.31587  -1.203 0.229162    
#Genremix:Period1950-1999                              -3.74874    1.94549  -1.927 0.053994 .  
#Genrenonfiction:Period1950-1999                       -0.84613    1.32943  -0.636 0.524474    
#Genreprivat:Period1950-1999                          -13.35648  423.00913  -0.032 0.974811    
#Genrefiction:Period2000-                              -2.47735    1.72026  -1.440 0.149840    
#Genremassmedia:Period2000-                            -1.02642    1.80431  -0.569 0.569442    
#Genremix:Period2000-                                        NA         NA      NA       NA    
#Genrenonfiction:Period2000-                           -0.67382    1.81423  -0.371 0.710334    
#Genreprivat:Period2000-                               -2.21845    2.08116  -1.066 0.286438    
#Genrefiction:SemClassStatIntrans                      10.44253 1645.66111   0.006 0.994937    
#Genremassmedia:SemClassStatIntrans                    -3.03394 1745.22323  -0.002 0.998613    
#Genremix:SemClassStatIntrans                          11.23310 1645.66176   0.007 0.994554    
#Genrenonfiction:SemClassStatIntrans                   -2.21842 1765.15515  -0.001 0.998997    
#Genreprivat:SemClassStatIntrans                       -6.36289 1777.07829  -0.004 0.997143    
#Genrefiction:SemClassTransitive                        0.26064    1.39136   0.187 0.851404    
#Genremassmedia:SemClassTransitive                      1.20108    1.52128   0.790 0.429807    
#Genremix:SemClassTransitive                           -0.15330    2.06816  -0.074 0.940911    
#Genrenonfiction:SemClassTransitive                     2.75826    1.48944   1.852 0.064043 .  
#Genreprivat:SemClassTransitive                         1.08721    1.59806   0.680 0.496295    
#Genrefiction:SemClassStatIntrans:Period1850-1899      -0.05916 2909.64199   0.000 0.999984    
#Genremassmedia:SemClassStatIntrans:Period1850-1899    13.76412 2967.08953   0.005 0.996299    
#Genremix:SemClassStatIntrans:Period1850-1899          14.08301 2965.28177   0.005 0.996211    
#Genrenonfiction:SemClassStatIntrans:Period1850-1899   11.92100 2978.85716   0.004 0.996807    
#Genreprivat:SemClassStatIntrans:Period1850-1899       15.45419 2985.93803   0.005 0.995870    
#Genrefiction:SemClassTransitive:Period1850-1899        1.19512    2.52422   0.473 0.635886    
#Genremassmedia:SemClassTransitive:Period1850-1899     -0.12289    2.60253  -0.047 0.962339    
#Genremix:SemClassTransitive:Period1850-1899           12.00845  571.74619   0.021 0.983243    
#Genrenonfiction:SemClassTransitive:Period1850-1899    -0.04531    2.60991  -0.017 0.986148    
#Genreprivat:SemClassTransitive:Period1850-1899        -0.06247    2.72732  -0.023 0.981727    
#Genrefiction:SemClassStatIntrans:Period1900-1949     -16.05655  670.68108  -0.024 0.980900    
#Genremassmedia:SemClassStatIntrans:Period1900-1949    -2.93886  887.36516  -0.003 0.997357    
#Genremix:SemClassStatIntrans:Period1900-1949         -29.58884  921.16302  -0.032 0.974375    
#Genrenonfiction:SemClassStatIntrans:Period1900-1949   -4.52045  925.95144  -0.005 0.996105    
#Genreprivat:SemClassStatIntrans:Period1900-1949             NA         NA      NA       NA    
#Genrefiction:SemClassTransitive:Period1900-1949       -0.60814    1.97681  -0.308 0.758358    
#Genremassmedia:SemClassTransitive:Period1900-1949     -0.73056    2.06870  -0.353 0.723977    
#Genremix:SemClassTransitive:Period1900-1949            0.05526    2.79496   0.020 0.984225    
#Genrenonfiction:SemClassTransitive:Period1900-1949    -3.22282    2.05213  -1.570 0.116305    
#Genreprivat:SemClassTransitive:Period1900-1949         0.10181    2.32686   0.044 0.965101    
#Genrefiction:SemClassStatIntrans:Period1950-1999       1.84687 1782.93298   0.001 0.999174    
#Genremassmedia:SemClassStatIntrans:Period1950-1999    14.79839 1875.22098   0.008 0.993704    
#Genremix:SemClassStatIntrans:Period1950-1999           2.83296 1782.93541   0.002 0.998732    
#Genrenonfiction:SemClassStatIntrans:Period1950-1999   14.40324 1893.78536   0.008 0.993932    
#Genreprivat:SemClassStatIntrans:Period1950-1999       20.29690 2140.99206   0.009 0.992436    
#Genrefiction:SemClassTransitive:Period1950-1999       -0.48057    1.69523  -0.283 0.776804    
#Genremassmedia:SemClassTransitive:Period1950-1999     -2.19771    1.80950  -1.215 0.224542    
#Genremix:SemClassTransitive:Period1950-1999           -0.46631    3.21119  -0.145 0.884541    
#Genrenonfiction:SemClassTransitive:Period1950-1999    -4.09604    1.83876  -2.228 0.025907 *  
#Genreprivat:SemClassTransitive:Period1950-1999        -2.05632 1112.43058  -0.002 0.998525    
#Genrefiction:SemClassStatIntrans:Period2000-           2.02757 2909.64230   0.001 0.999444    
#Genremassmedia:SemClassStatIntrans:Period2000-        14.66957 2967.08982   0.005 0.996055    
#Genremix:SemClassStatIntrans:Period2000-                    NA         NA      NA       NA    
#Genrenonfiction:SemClassStatIntrans:Period2000-        2.55991 3008.77788   0.001 0.999321    
#Genreprivat:SemClassStatIntrans:Period2000-            9.07190 3012.09059   0.003 0.997597    
#Genrefiction:SemClassTransitive:Period2000-           -1.89445    2.15187  -0.880 0.378656    
#Genremassmedia:SemClassTransitive:Period2000-         -3.67466    2.22260  -1.653 0.098267 .  
#Genremix:SemClassTransitive:Period2000-                     NA         NA      NA       NA    
#Genrenonfiction:SemClassTransitive:Period2000-        -5.65891    2.24472  -2.521 0.011703 *  
#Genreprivat:SemClassTransitive:Period2000-             0.03341    2.61698   0.013 0.989814    
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

#(Dispersion parameter for binomial family taken to be 1)

#    Null deviance: 16783.9  on 34078  degrees of freedom
#Residual deviance:  7252.9  on 33981  degrees of freedom
#AIC: 7448.9

#Number of Fisher Scoring iterations: 15


#########
###Accuracy for glm3:
########

probabilityNU = plogis(predict(dat.glm3))
tab = table(dat$NU=="nu", probabilityNU >= 0.5)
tab
       
#        FALSE  TRUE
#  FALSE 31553   237
#  TRUE    920  1369
sum(diag(tab))/sum(tab)
#[1] 0.9660495

#####################

dat.dd=datadist(dat)
options(datadist="dat.dd")

dat.lrm=lrm(NU~Prefix + Genre + Rootfinal + SemClass + SJA + Period, 
    data=dat)
table(dat$Form)
#
#finite gerund mascsg   part 
# 17411   1377   8318   6973 
table(dat$Form, dat$Prefix)
#        
#         Prefixed Unprefixed
#  finite    15428       1983
#  gerund     1377          0
#  mascsg     7891        427
#  part       6826        147

# so the structural zero is creating an immense problem, on the logit scale, -inf and +inf

dat2 = dat[dat$Form != "gerund",]
dat2$Form= dat2$Form[drop=TRUE]
dat2.dd=datadist(dat2)
options(datadist="dat2.dd")
dat2.lrm=lrm(NU~Form+Prefix + Genre + Rootfinal + SemClass + SJA + Period, 
    data=dat2)

# this works



#################################
#TREE & FOREST
#################################

library(party)

dat.ctree=ctree(NU~Form+Prefix + Genre + Rootfinal + SemClass + SJA + Period, 
    dat)

pdf("nu.ctree.pdf", he=10,wi=40)
plot(dat.ctree)
dev.off()



nu01 = as.numeric(dat$NU)-1
x = treeresponse(dat.ctree)
probability = sapply(x, FUN=function(v)return(v[2]))
somers2(probability, nu01)
#           C          Dxy            n      Missing 
#9.641528e-01 9.283057e-01 3.407900e+04 0.000000e+00 

tab=table(dat$NU, probability >= 0.5)
tab
#      
#       FALSE  TRUE
#  NoNu 31673   117
#  nu    1002  1287
sum(diag(tab))/sum(tab)
# 0.9671645

table(dat$NU)
# NoNu    nu 
#31790  2289

prop.test(c(31790, sum(diag(tab))), rep(nrow(dat),2))
#
#        2-sample test for equality of proportions with continuity correction
#
#data:  c(31790, sum(diag(tab))) out of rep(nrow(dat), 2) 
#X-squared = 422.0914, df = 1, p-value < 2.2e-16
#alternative hypothesis: two.sided 
#95 percent confidence interval:
# -0.03762362 -0.03104037 
#sample estimates:
#   prop 1    prop 2 
#0.9328325 0.9671645 



nu01 = as.numeric(dat$NU)-1
probs=fitted(dat.glm)
somers2(probs, nu01)
#           C          Dxy            n      Missing 
#9.497128e-01 8.994256e-01 3.407900e+04 0.000000e+00 


dat$TreeResponse = predict(dat.ctree)
dat.ctree=ctree(TreeResponse~Form+Prefix + Genre + 
    Rootfinal + SemClass + SJA + Period, dat)
plot(dat.ctree)


# an abbreviated tree using a maximal depth of four
dat.ctree=ctree(NU~Form+Prefix + Genre + 
    Rootfinal + SemClass + SJA + Period, dat, 
    controls=ctree_control(maxdepth=4))
plot(dat.ctree)

dat.ctree=ctree(NU~Form+Prefix + Genre + 
    Rootfinal + SemClass + SJA + Period, dat, 
    controls=ctree_control(mincriterion=0.9999999))
plot(dat.ctree)



#######################################################################

##########################################################################
# ndl
##########################################################################

library(ndl)
dat=read.csv("datNU.csv", T, stringsAsFactors=FALSE)
rownames(dat)=1:nrow(dat)
write.table(dat, file="datNUcleaned.csv", row.names=FALSE, quote=FALSE)

# AUGUST 1 UPDATE
dat=read.csv("datNUcleaned.csv", T, stringsAsFactors=FALSE)

dat.ndl = ndlClassify(NU~Form+Prefix + Genre + Rootfinal + SemClass + SJA + 
    Period, data=dat)
ndlStatistics(dat.ndl)$C
#[1] 0.9496399
ndlStatistics(dat.ndl)$crosstable
ndlStatistics(dat.ndl)$crosstable
#      NoNu   nu
#NoNu 31614  176
#nu    1088 1201

ndlStatistics(dat.ndl)$accuracy
#[1] 0.9629097


dat.ndl = ndlClassify(NU~Form*Prefix + Genre + Rootfinal + SemClass + SJA + 
    Period, data=dat)

ndlStatistics(dat.ndl)$C
#[1] 0.9525346

dat.ndl.cv1 = ndlCrossvalidate(NU~Form+Prefix + Genre + 
    Rootfinal + SemClass + SJA + Period, data=dat)
summary(dat.ndl.cv1)
#Number of folds:  10
#N(total):  34079
#N(train):  30671
#N(test):   3408
#
#                    Mean    Minimum Maximum
#loglikelihood.null    -839  -906.3  -804.9 
#loglikelihood.model   -548    -580    -523 
#deviance.null         1678    1610    1813 
#deviance.model        1096    1046    1160 
#R2.likelihood       0.3462  0.3069  0.3939 
#R2.nagelkerke       0.4029  0.3606  0.4583 
#AIC.model             1208    1158    1272 
#BIC.model             1552    1501    1615 
#C                   0.9489  0.9396  0.9559 
#accuracy            0.9629  0.9604  0.9657 
#lambda.prediction   0.4474  0.4133  0.4748 
#tau.classification  0.7038  0.6859   0.718 



##########################################################################
# what does the model do given its choice behavior
##########################################################################




