
rm(list=ls())

load('daysdat.rda')

# score as counts; by participant, day, and grammaticality
daysdat$Sums <- with(daysdat, ave(x=Response, Group, isSentenceCorrect, InterventionDay,
    FUN=function(x){sum(x)}))
daysdat$Counts <- with(daysdat, ave(x=Response, Group, isSentenceCorrect, InterventionDay,
    FUN=function(x){length(x)}))

# unique scores
summary.daysdat = unique(daysdat[,-c(1,5)])
summary.daysdat = summary.daysdat[order(summary.daysdat[,2], summary.daysdat[,3]),]
rownames(summary.daysdat) = NULL
summary.daysdat
#    Group isSentenceCorrect InterventionDay Sums Counts
# 1   GRAM             gramm               1  389    440
# 2    LEX             gramm               1  395    440
# 3   GRAM             gramm               2  362    400
# 4    LEX             gramm               2  370    400
# 5   GRAM             gramm               3  389    440
# 6    LEX             gramm               3  401    440
# 7   GRAM           ungramm               1  348    400
# 8    LEX           ungramm               1  365    400
# 9   GRAM           ungramm               2  369    440
# 10   LEX           ungramm               2  366    440
# 11  GRAM           ungramm               3  268    400
# 12   LEX           ungramm               3  276    400

# Note: Use the above Sums and Counts values to run the Bayes AB test in JASP!

