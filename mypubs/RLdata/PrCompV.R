> dat=read.csv("PrCompV.csv", T)
#This shows the first six lines of the database
> head(dat)
#  Sentence.number Language Latitude
#1               1       RU    55.45
#2               2       RU    55.45
#3               3       RU    55.45
#4               4       RU    55.45
#5               5       RU    55.45
#6               6       RU    55.45
#  Longitude Time.parameters Prep
#1     37.37           short    v
#2     37.37           short    v
#3     37.37           short    v
#4     37.37       unbounded    v
#5     37.37           short    v
#6     37.37       unbounded    v
#        Case Number    CasePrep
#1 accusative     sg vacc
#2 accusative     pl vacc
#3 accusative     pl vacc
#4 accusative     sg vacc
#5 accusative     sg vacc
#6 accusative     sg vacc
#Create a table:
> x=table(dat$Language, dat$CasePrep)
#Run a principle components analysis
> x.pr=prcomp(x, scale=T)
#Plot the results of the PrComp analysis
> pdf("biplot.pdf", height=6,width=6)
> biplot(x.pr, xlim=c(-0.65, 0.58), col=c("black", "darkgray"))
> dev.off()
null device 
          1 
# get the loadings of the languages on the first PC
> pc1vals=x.pr$x[,1]
# make a data frame (dfr) that has languages, PC1 and longitude
> dfr = unique(dat[,c("Language","Longitude")])
> dfr$PC1 = pc1vals[as.character(dfr$Language)]
#run a linear regression predicting PC1 (language) from geography
> dfr.lm = lm(PC1~Longitude,data=dfr)
> summary(dfr.lm)

#Call:
#lm(formula = PC1 ~ Longitude, data = dfr)
#
#Residuals:
#       1      272      543      814 
#-0.09082  0.81427  1.24600 -0.88022 
#    1085 
#-1.08922 
#
#
#Coefficients:
#            Estimate Std. Error t value
#(Intercept)  6.75871    1.79660   3.762
#Longitude   -0.27259    0.06926  -3.936
#            Pr(>|t|)  
#(Intercept)   0.0328 *
#Longitude     0.0292 *
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
#
#Residual standard error: 1.181 on 3 degrees of freedom
#Multiple R-squared: 0.8378,	Adjusted R-squared: 0.7837 
#F-statistic: 15.49 on 1 and 3 DF,  p-value: 0.02922 
#
# a standard coorelation test
> cor.test(dfr$Longitude, dfr$PC1)

#	Pearson's product-moment correlation

#data:  dfr$Longitude and dfr$PC1 
#t = -3.9359, df = 3, p-value =0.02922
#alternative hypothesis: true correlation is not equal to 0 95 percent #confidence interval:
# -0.9944826 -0.1715968 
#sample estimates:
#       cor 
#-0.9152932 

#Plot the results of the regression 
> pdf("regression_new.pdf, he=6, wi=6")
> xlimit=c(12,41)
> plot(dfr$Longitude, dfr$PC1, type="n", xlim=xlimit, xlab="longitude", ylab="PC1")
> text(dfr$Longitude, dfr$PC1, dfr$Language)
> abline(dfr.lm$coef)
> dev.off()
null device 
          1 
> summary(dat.pc)
#Importance of components:
#                          PC1    PC2    PC3     PC4       PC5
#Standard deviation     2.5395 1.7288 1.2608 0.98629 4.651e-16
#Proportion of Variance 0.5374 0.2491 0.1325 0.08106 0.000e+00
#Cumulative Proportion  0.5374 0.7865 0.9189 1.00000 1.000e+00
> 