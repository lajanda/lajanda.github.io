v = read.csv('verbs.csv')

library('languageR')
library('cluster')

source('translit.r')

'%ni%' <- Negate('%in%')

v$afflem = interaction(v$prefix, v$suffix, v$lemma, drop=TRUE)
#for lemma set
v$segment = interaction(v$prefix,v$harmonised_stem,v$suffix,v$lemma,drop=TRUE)

#excluded byti, ne byti, byvati, skazati, graze-pasti, verbs without stem tag
verbs =  subset(v, lemma_id != 58773 & lemma_id != 59322 & lemma_id != 58890 & lemma_id != 65845 & lemma_id != 70085 & stem !='')

#This makes a table with the lemmas and counts for each subparadigm
a = table(verbs$lemma_id, verbs$tensemood)

#This creates a dataframe with only lemmas that have 20 or more attestations
twenty = data.frame(a[rowSums(a)>=20,])

#This is a vector of those lemmas
verbset = rownames(twenty)

#This creates a dataframe that has only the verbs with 20 or more attestations
tw = subset(verbs, lemma_id %in% verbset)

#This counts up how many datapoints there are in the tw dataframe
print(nrow(tw))

#This creates a table like the table a above, but with the added information on prefixes and suffixes for each lemma
lat = table(tw$afflem, tw$tensemood)

#This removes empty rows
lat2 = lat[rowSums(lat)!=0,]

#This gives the values as percentages
lat3 = prop.table(lat2,1)

#This makes the table into a dataframe
tw2 = data.frame(lat3)

#This makes new columns for prefix/suffix information and transliterates for plots
tw2$presuff = gsub('(.*\\..*)\\..*', '\\1',rownames(tw2), perl=TRUE)
tw2$presuff = translitocs(tw2$presuff)

tw2$prefix = gsub('(.*)\\..*\\..*', '\\1',rownames(tw2), perl=TRUE)
tw2$prefix = translitocs(tw2$prefix)

tw2$suffix = gsub('.*\\.(.*)\\..*', '\\1',rownames(tw2), perl=TRUE)
tw2$suffix = translitocs(tw2$suffix)

tw2$lemma = gsub('.*\\..*\\.(.*)', '\\1',rownames(tw2), perl=TRUE)

tw2$lemma = translitocs(tw2$lemma)

tw2$pre = tw2$prefix != ''

tw2$pspattern = as.factor(interaction(tw2$pre, tw2$suffix, drop=TRUE))

#This sorts the data so that you have metadata on one side and counts only for the subparadigms on the other
set = tw2[,c(13,12,11,10,14,15,1:4,8,7,9)]

#This separates the metadata from the other data
crmeta = set[,1:6]
crdata = set[,c(7:13)]

#This performs the correspondence analysis
verbs.ca = corres.fnc(crdata)

#This makes plots from the correspondence analysis, overlaying various metadata for the plots
cairo_pdf('lemmaplot.pdf')
plot(verbs.ca, rlabels=crmeta$lemma,rcex=0.6, ccol='black')
dev.off()

cairo_pdf('patternplot1.pdf')
plot(verbs.ca, rlabels=crmeta$presuff,rcex=0.5, ccol='black')
dev.off()

cairo_pdf('patternplot2.pdf')
plot(verbs.ca, rlabels=crmeta$pspattern,rcex=0.5, ccol='black')
dev.off()

#This utilizes coordinates from plots for subsets
coord = attr(verbs.ca,"data")$origOut$rproj
left = subset(coord, coord[,1] < 0)
right = subset(coord, coord[,1] > 0)
middle = subset(coord, coord[,1] > -0.5 & coord[,1] < 0.5)
xleft = subset(coord, coord[,1] < -0.5)
xright = subset(coord, coord[,1] > 0.5)

lft = rownames(left)
rght = rownames(right)
mdl = rownames(middle)

#These are lemma lists
lefties = gsub('.*\\..*\\.(.*)', '\\1',rownames(left), perl=TRUE)
righties = gsub('.*\\..*\\.(.*)', '\\1',rownames(right), perl=TRUE)
middles = gsub('.*\\..*\\.(.*)', '\\1',rownames(middle), perl=TRUE)
xlefties = gsub('.*\\..*\\.(.*)', '\\1',rownames(xleft), perl=TRUE)
xrighties = gsub('.*\\..*\\.(.*)', '\\1',rownames(xright), perl=TRUE)

#These are the quadrants
dl = subset(coord, coord[,1] < 0 & coord[,2] < 0)
dr = subset(coord, coord[,1] > 0 & coord[,2] < 0)
ul = subset(coord, coord[,1] < 0 & coord[,2] > 0)
ur = subset(coord, coord[,1] > 0 & coord[,2] > 0)

#These are subsets of the database for each group of verbs
lefts = subset(verbs, lemma %in% lefties)
rights = subset(verbs, lemma %in% righties)
mids = subset(verbs, lemma %in% middles)

#These are overall grammatical profiles for lefties and righties according to percetages
lev = prop.table(table(lefts$tensemood))*100

pra = prop.table(table(rights$tensemood))*100

prg = range(0,50)

cairo_pdf('rightprofile.pdf')
par(mar = c(7, 4, 4, 2) + 0.1)
barplot(pra[c(1:4,8,7,9)], ylim=prg, cex.names = 0.7, las = 2)
dev.off()

cairo_pdf('leftprofile.pdf')
par(mar = c(7, 4, 4, 2) + 0.1)
barplot(lev[c(1:4,8,7,9)], ylim=prg, cex.names = 0.7, las = 2)
dev.off()

levpra = t(data.frame(left=lev[c(1:4,8,7,9)], right=pra[c(1:4,8,7,9)]))

cairo_pdf('levpra.pdf')
barplot(levpra, legend=rownames(levpra),beside=T,cex.names=0.5)
dev.off()




#This is input data for the diana analysis
mat = set[,c(1,7:13)]
rownames(mat) = mat$lemma
mat2 = mat[,2:8]
verbs.dist = dist(mat2)

cairo_pdf('diana.pdf')
 pltree(diana(verbs.dist),cex=0.7)
dev.off()

#This is a dataframe with the name of the lemma and its cluster membership according to the diana analysis
x = data.frame(lemma = gsub('.*\\..*\\.(.*)', '\\1',rownames(set), perl=TRUE), cluster = cutree(diana(verbs.dist),2))
print("Verb lemma membership in two main clusters")
print(x[order(x$cluster),])

#These are subsets according to the clusters
one = subset(x, cluster == 1)
two = subset(x, cluster == 2)

ones = one$lemma
twos = two$lemma

jedin = subset(verbs, lemma %in% ones)
dva = subset(verbs, lemma %in% twos)

jed = prop.table(table(jedin$tensemood))*100

dv = prop.table(table(dva$tensemood))*100

cairo_pdf('oneprofile.pdf')
barplot(jed[c(1:4,8,7,9)], cex.names = 0.5)
dev.off()

cairo_pdf('twoprofile.pdf')
barplot(dv[c(1:4,8,7,9)], cex.names = 0.5)
dev.off()

print("Lemmata present in cluster 1 but not on the right side of the correspondence analysis plot")
print(setdiff(one$lemma,righties))

print("Lemmata present in cluster 2 but not on the left side of the correspondence analysis plot")
print(setdiff(two$lemma,lefties))

#counts of lefty and righty grammatical profiles
counts.table=matrix(c(table(lefts$tensemood)[c(1:4,8,7,9)], table(rights$tensemood)[c(1:4,8,7,9)]), byrow=TRUE, ncol=7)

#this prints the table of counts with the verbs on the left of the correspondence analysis in the first row and the verbs on right of the correspondence analysis in the second row:
print(counts.table)

#this gives us the N which we need for the denominator in the effect size measure:
verbsum = sum(counts.table)
print("This is the sum of observations")
print(verbsum)

#this gives us the chi-square calculation:
test = chisq.test(counts.table)
print(test)

#this extracts the chi-square-value alone:
value = test$statistic

#this computes the effect size:
cramer = sqrt(test$statistic/verbsum)
print('This is the effect size')
print(cramer)

#sets up a data set of unique lemmata with affix, stem and pattern information
sg = table(verbs$segment, verbs$tensemood)
seg = data.frame(sg[rowSums(sg)!=0,])
seg$prefix = gsub('(.*)\\..*\\..*\\..*', '\\1',rownames(seg), perl=TRUE)
seg$suffix = gsub('.*\\..*\\.(.*)\\..*', '\\1',rownames(seg), perl=TRUE)
seg$stem = gsub('.*\\.(.*)\\..*\\..*', '\\1',rownames(seg), perl=TRUE)
seg$lemma = gsub('.*\\..*\\..*\\.(.*)', '\\1',rownames(seg), perl=TRUE)
seg$presuff = as.factor(interaction(seg$prefix,seg$suffix,drop=TRUE))
seg$pre = seg$prefix != ''
seg$pspattern = as.factor(interaction(seg$pre, seg$suffix, drop=TRUE))
lemmaset = seg[,10:16]


#percentages of lefty and righty grammatical profiles
ltm = prop.table(table(lefts$lemma, lefts$tensemood),1)*100
rtm = prop.table(table(rights$lemma, rights$tensemood),1)*100

pdf('aorist.pdf')
boxplot(ltm[,1],rtm[,1],names=c('left','right'))
dev.off()

#This prints out the following objects: a matrix with 1=bottom whisker, 2=lower quartile, 3 = median, 4= higher quartile, 5 = top whisker; number of verbs on left and right; conf?; names and percentages for outliers; group= whether outliers are on left or right
print("Boxplot output for the aorist")
aorist = (boxplot(ltm[,1],rtm[,1],names=c('left','right'),plot=FALSE))
print(aorist)
aoristout = names(aorist$out)

#This gives the full data for outliers
print ("Distribution patterns of aorist outliers")
print(subset(seg, lemma %in% aoristout)[,c(1:9)])



pdf('imperative.pdf')
boxplot(ltm[,2],rtm[,2],names=c('left','right'))
dev.off()

print("Boxplot output for the imperative")
imper = (boxplot(ltm[,2],rtm[,2],names=c('left','right'),plot=FALSE))
print(imper)
imperout = names(imper$out)

print ("Distribution patterns of imperative outliers")
print(subset(seg, lemma %in% imperout)[,c(1:9)])


pdf('imperfect.pdf')
boxplot(ltm[,3],rtm[,3],names=c('left','right'))
dev.off()

print("Boxplot output for the imperfect")
ipf = boxplot(ltm[,3],rtm[,3],names=c('left','right'),plot=FALSE)
print(ipf)
ipfout = names(ipf$out)

print ("Distribution patterns of imperfect outliers")
print(subset(seg, lemma %in% ipfout)[,c(1:9)])


pdf('infinitivesupine.pdf')
boxplot(ltm[,4],rtm[,4],names=c('left','right'))
dev.off()

print("Boxplot output for the infinitive and supine")
infsup = boxplot(ltm[,4],rtm[,4],names=c('left','right'),plot=FALSE)
print(infsup)
infsupout = names(infsup$out)

print ("Distribution patterns of infinitive/supine outliers")
print(subset(seg, lemma %in% infsupout)[,c(1:9)])



pdf('pastparticiple.pdf')
boxplot(ltm[,7],rtm[,7],names=c('left','right'))
dev.off()

print("Boxplot output for the past participle")
ppcl = boxplot(ltm[,7],rtm[,7],names=c('left','right'),plot=FALSE)
print(ppcl)
ppclout = names(ppcl$out)

print ("Distribution patterns of past participle outliers")
print(subset(seg, lemma %in% ppclout)[,c(1:9)])


pdf('present.pdf')
boxplot(ltm[,8],rtm[,8],names=c('left','right'))
dev.off()

print("Boxplot output for the present")
present = boxplot(ltm[,8],rtm[,8],names=c('left','right'),plot=FALSE)
print(present)
presentout = names(present$out)

print ("Distribution patterns of present outliers")
print(subset(seg, lemma %in% presentout)[,c(1:9)])


pdf('presentparticiple.pdf')
boxplot(ltm[,9],rtm[,9],names=c('left','right'))
dev.off()

print("Boxplot output for the present participle")
prepcl = boxplot(ltm[,9],rtm[,9],names=c('left','right'),plot=FALSE)
print(prepcl)
prepclout = names(prepcl$out)

print ("Distribution patterns of present participle outliers")
print(subset(seg, lemma %in% prepclout)[,c(1:9)])



print("Verbs between the 0.5 and -0.5 on the factor 1 axis")
print(middles)

print("Verbs left of -0.5")
print(xlefties)

print("Verbs right of 0.5")
print(xrighties)

llemma = subset(lemmaset, lemma %in% lefties)
print("Affix patterns of the lefties")
print(summary(llemma$presuff, maxsum=21))
print("Per cent")
print(prop.table(summary(llemma$presuff, maxsum=21))*100)

print("How many lefties are prefixed?")
print(table(llemma$pre))


rlemma = subset(lemmaset, lemma %in% righties)
print("Affix patterns of the righties")
print(summary(rlemma$presuff, maxsum=42))
print("Per cent")
print(prop.table(summary(rlemma$presuff, maxsum=42))*100)

print("How many righties are prefixed?")
print(table(rlemma$pre))


mlemma = subset(lemmaset, lemma %in% middles)
print("Affix patterns of the middles")
print(summary(mlemma$presuff, maxsum=29))
print("Per cent")
print(prop.table(summary(mlemma$presuff, maxsum=29))*100)

print("How many middles are prefixed?")
print(table(mlemma$pre))

#picks out all verbs that don't occur in the aorist
noaor = row.names(subset(set, aorist == 0))

#subset of these with coordinates
noaorc = subset(coord, rownames(coord) %in% noaor)
print(noaorc[order(noaorc[,1]),1])

#lefties to the left of -0.4
farleft = subset(coord, coord[,1] < -0.4)

#verbs to the left of -0,4 that do occur in the aorist
farleftaor = setdiff(rownames(farleft),noaor)

#coordinates of farleftaor

farleftaorc = subset(coord, rownames(coord) %in% farleftaor)
print(farleftaorc[order(farleftaorc[,1]),1])

#checking Greek originals of righty present occurrences, using a data set of Greek correspondents to righty present tense occurrences
gre = read.csv('righty_greek_present.csv')
print("Greek correspondents of righty present occurrences, per cent")
print(prop.table(table(gre$gr_tensemood)))












