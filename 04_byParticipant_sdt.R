
rm(list=ls())

require(pROC)

set.seed(1828) # to replicate results

load('sdtdat.rda')

# scores as proportions; by participant, t/a, seen, and grammaticality
sdtdat$Probs <- with(sdtdat, ave(x=Correct, ID, PosttTA, OldNew, IsCorrect,
    FUN=function(x){sum(x)/length(x)}))

# unique by-participant data points
subjdat = unique(sdtdat[,-c(3,4,8)])

# remove rare TA Seen/Unseen combinations
subjdat = droplevels(subjdat[(subjdat$OldNew=='old' & !subjdat$PosttTA=='FutImp') |
    (subjdat$OldNew=='new' & !subjdat$PosttTA %in% c('PastImp','FutImp')),])

###############################
### ROC tests of difference ###
###############################

### 1 ### PastImp / Seen
roc.LexOldPastImp = roc(IsCorrect ~ Probs, data=subjdat,
    levels=c('correct','incorrect'), direction='>',
    subset=(GROUP=='LEX' & PosttTA=='PastImp' & OldNew=='old'))
roc.GramOldPastImp = roc(IsCorrect ~ Probs, data=subjdat,
    levels=c('correct','incorrect'), direction='>',
    subset=(GROUP=='GRAM' & PosttTA=='PastImp' & OldNew=='old'))
roc.test(roc.LexOldPastImp, roc.GramOldPastImp, method='bootstrap', boot.n=10000)
# D = 1.2015, boot.n = 10000, boot.stratified = 1, p-value = 0.2295
# alternative hypothesis: true difference in AUC is not equal to 0
# sample estimates:
# AUC of roc1 AUC of roc2 
#      0.7000      0.6125 

### 2 ### PastPerf / Seen
roc.LexOldPastPerf = roc(IsCorrect ~ Probs, data=subjdat,
    levels=c('correct','incorrect'), direction='>',
    subset=(GROUP=='LEX' & PosttTA=='PastPerf' & OldNew=='old'))
roc.GramOldPastPerf = roc(IsCorrect ~ Probs, data=subjdat,
    levels=c('correct','incorrect'), direction='>',
    subset=(GROUP=='GRAM' & PosttTA=='PastPerf' & OldNew=='old'))
roc.test(roc.LexOldPastPerf, roc.GramOldPastPerf, method='bootstrap', boot.n=10000)
# D = 1.7749, boot.n = 10000, boot.stratified = 1, p-value = 0.07592
# alternative hypothesis: true difference in AUC is not equal to 0
# sample estimates:
# AUC of roc1 AUC of roc2 
#   0.8581250   0.7309375 

### 3 ### PresImp / Seen
roc.LexOldPresImp = roc(IsCorrect ~ Probs, data=subjdat,
    levels=c('correct','incorrect'), direction='>',
    subset=(GROUP=='LEX' & PosttTA=='PresImp' & OldNew=='old'))
roc.GramOldPresImp = roc(IsCorrect ~ Probs, data=subjdat,
    levels=c('correct','incorrect'), direction='>',
    subset=(GROUP=='GRAM' & PosttTA=='PresImp' & OldNew=='old'))
roc.test(roc.LexOldPresImp, roc.GramOldPresImp, method='bootstrap', boot.n=10000)
# D = 0.963, boot.n = 10000, boot.stratified = 1, p-value = 0.3355
# alternative hypothesis: true difference in AUC is not equal to 0
# sample estimates:
# AUC of roc1 AUC of roc2 
#   0.8690625   0.8059375 

### 4 ### FutPerf / Seen
roc.LexOldFutPerf = roc(IsCorrect ~ Probs, data=subjdat,
    levels=c('correct','incorrect'), direction='>',
    subset=(GROUP=='LEX' & PosttTA=='FutPerf' & OldNew=='old'))
roc.GramOldFutPerf = roc(IsCorrect ~ Probs, data=subjdat,
    levels=c('correct','incorrect'), direction='>',
    subset=(GROUP=='GRAM' & PosttTA=='FutPerf' & OldNew=='old'))
roc.test(roc.LexOldFutPerf, roc.GramOldFutPerf, method='bootstrap', boot.n=10000)
# D = -0.15099, boot.n = 10000, boot.stratified = 1, p-value = 0.88
# alternative hypothesis: true difference in AUC is not equal to 0
# sample estimates:
# AUC of roc1 AUC of roc2 
#    0.746250    0.756875 

### 5 ### PastPerf / Unseen
roc.LexNewPastPerf = roc(IsCorrect ~ Probs, data=subjdat,
    levels=c('correct','incorrect'), direction='>',
    subset=(GROUP=='LEX' & PosttTA=='PastPerf' & OldNew=='new'))
roc.GramNewPastPerf = roc(IsCorrect ~ Probs, data=subjdat,
    levels=c('correct','incorrect'), direction='>',
    subset=(GROUP=='GRAM' & PosttTA=='PastPerf' & OldNew=='new'))
roc.test(roc.LexNewPastPerf, roc.GramNewPastPerf, method='bootstrap', boot.n=10000)
# D = 1.2774, boot.n = 10000, boot.stratified = 1, p-value = 0.2015
# alternative hypothesis: true difference in AUC is not equal to 0
# sample estimates:
# AUC of roc1 AUC of roc2 
#    0.785625    0.682500 

### 6 ### PresImp / Unseen
roc.LexNewPresImp = roc(IsCorrect ~ Probs, data=subjdat,
    levels=c('correct','incorrect'), direction='>',
    subset=(GROUP=='LEX' & PosttTA=='PresImp' & OldNew=='new'))
roc.GramNewPresImp = roc(IsCorrect ~ Probs, data=subjdat,
    levels=c('correct','incorrect'), direction='>',
    subset=(GROUP=='GRAM' & PosttTA=='PresImp' & OldNew=='new'))
roc.test(roc.LexNewPresImp, roc.GramNewPresImp, method='bootstrap', boot.n=10000)
# D = -0.4752, boot.n = 10000, boot.stratified = 1, p-value = 0.6346
# alternative hypothesis: true difference in AUC is not equal to 0
# sample estimates:
# AUC of roc1 AUC of roc2 
#     0.74375     0.78125 

### 7 ### FutPerf / Unseen
roc.LexNewFutPerf = roc(IsCorrect ~ Probs, data=subjdat,
    levels=c('correct','incorrect'), direction='>',
    subset=(GROUP=='LEX' & PosttTA=='FutPerf' & OldNew=='new'))
roc.GramNewFutPerf = roc(IsCorrect ~ Probs, data=subjdat,
    levels=c('correct','incorrect'), direction='>',
    subset=(GROUP=='GRAM' & PosttTA=='FutPerf' & OldNew=='new'))
roc.test(roc.LexNewFutPerf, roc.GramNewFutPerf, method='bootstrap', boot.n=10000)
# D = 0.51551, boot.n = 10000, boot.stratified = 1, p-value = 0.6062
# alternative hypothesis: true difference in AUC is not equal to 0
# sample estimates:
# AUC of roc1 AUC of roc2 
#    0.590625    0.551250 

# -----

# Uncomment, should you wish to generate the by-participant data plots
# using the pROC package. Note, however, that the pROC plots present
# Specificity and Sensitivity, not False Alarms and Hits.

# par(mfrow=c(1,4))
# plot(roc.LexOldPresImp, col='blue', main='PresImp')
# lines(roc.GramOldPresImp, col='red', lty='dotted')
# plot(roc.LexOldPastPerf, col='blue', main='PastPerf')
# lines(roc.GramOldPastPerf, col='red', lty='dotted')
# plot(roc.LexOldPastImp, col='blue', main='PastImp')
# lines(roc.GramOldPastImp, col='red', lty='dotted')
# plot(roc.LexOldFutPerf, col='blue', main='FutPerf')
# lines(roc.GramOldFutPerf, col='red', lty='dotted')

# -

# par(mfrow=c(1,4))
# plot(roc.LexNewPresImp, col='blue', main='PresImp')
# lines(roc.GramNewPresImp, col='red', lty='dotted')
# plot(roc.LexNewPastPerf, col='blue', main='PastPerf')
# lines(roc.GramNewPastPerf, col='red', lty='dotted')
# plot(roc.LexNewFutPerf, col='blue', main='PastImp')
# lines(roc.GramNewFutPerf, col='red', lty='dotted')


