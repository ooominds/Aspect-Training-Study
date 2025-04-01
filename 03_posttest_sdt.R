
# The R file provided does not contain the primary function for calculating
# various parametric and nonparametric SDT-based measures referenced
# Stanislaw & Todorov (1999). This code remains unpublished.

########################################################################

rm(list=ls())

source('auxiliary_functions.R')

load('sdtdat.rda')

# setting the crosstabulation condition
variables = c('Correct', 'PosttTA', 'GROUP', 'OldNew', 'IsCorrect')
baseFormula = as.formula(paste('~', paste(variables, collapse=' + '), sep=''))

### 1 ### Getting counts: Training (lex/gram) x Items (old/new) x TA

lexOldSignal = as.data.frame.matrix(xtabs(baseFormula, data=sdtdat)
    [,,GROUP='LEX',OldNew='old',IsCorrect='correct'])
lexOldNoise = as.data.frame.matrix(xtabs(baseFormula, data=sdtdat)
    [,,GROUP='LEX',OldNew='old',IsCorrect='incorrect'])
lexNewSignal = as.data.frame.matrix(xtabs(baseFormula, data=sdtdat)
    [,,GROUP='LEX',OldNew='new',IsCorrect='correct'])
lexNewNoise = as.data.frame.matrix(xtabs(baseFormula, data=sdtdat)
    [,,GROUP='LEX',OldNew='new',IsCorrect='incorrect'])

gramOldSignal = as.data.frame.matrix(xtabs(baseFormula, data=sdtdat)
    [,,GROUP='GRAM',OldNew='old',IsCorrect='correct'])
gramOldNoise = as.data.frame.matrix(xtabs(baseFormula, data=sdtdat)
    [,,GROUP='GRAM',OldNew='old',IsCorrect='incorrect'])
gramNewSignal = as.data.frame.matrix(xtabs(baseFormula, data=sdtdat)
    [,,GROUP='GRAM',OldNew='new',IsCorrect='correct'])
gramNewNoise = as.data.frame.matrix(xtabs(baseFormula, data=sdtdat)
    [,,GROUP='GRAM',OldNew='new',IsCorrect='incorrect'])

### 2 ### Signal Detection values for each combination

  # A #   Lexical group on old items

# Removing FutImp as we do not have FalseAlarms data
lexOldSignal[,c(1)] = NULL
lexOldNoise[,c(1)] = NULL

# Total of Signal and Noise trials
lexOldSignalSums = colSums(lexOldSignal)
lexOldNoiseSums = colSums(lexOldNoise)

# Correcting for extreme counts
lexOldSignalCorrected = correct_extremes(lexOldSignal, lexOldSignalSums)
lexOldNoiseCorrected = correct_extremes(lexOldNoise, lexOldNoiseSums)

# Hits and FalseAlarms
lexOldHits = unlist(sweep(lexOldSignalCorrected['1',], 2, lexOldSignalSums, '/'))
lexOldFalseAlarms = unlist(sweep(lexOldNoiseCorrected['0',], 2, lexOldNoiseSums, '/'))

  # B #   Lexical group on new items

# Removing FutImp and PastImp as we do not have Hits and FalseAlarms data
lexNewSignal[,c(1,3)] = NULL
lexNewNoise[,c(1,3)] = NULL

# Total of Signal and Noise trials
lexNewSignalSums = colSums(lexNewSignal)
lexNewNoiseSums = colSums(lexNewNoise)

# Correcting for extreme counts
lexNewSignalCorrected = correct_extremes(lexNewSignal, lexNewSignalSums)
lexNewNoiseCorrected = correct_extremes(lexNewNoise, lexNewNoiseSums)

# Hits and FalseAlarms
lexNewHits = unlist(sweep(lexNewSignalCorrected['1',], 2, lexNewSignalSums, '/'))
lexNewFalseAlarms = unlist(sweep(lexNewNoiseCorrected['0',], 2, lexNewNoiseSums, '/'))

  # C #   Grammatical group on old items

# Removing FutImp as we do not have FalseAlarms data
gramOldSignal[,c(1)] = NULL
gramOldNoise[,c(1)] = NULL

# Total of Signal and Noise trials
gramOldSignalSums = colSums(gramOldSignal)
gramOldNoiseSums = colSums(gramOldNoise)

# Correcting for extreme counts
gramOldSignalCorrected = correct_extremes(gramOldSignal, gramOldSignalSums)
gramOldNoiseCorrected = correct_extremes(gramOldNoise, gramOldNoiseSums)

# Hits and FalseAlarms
gramOldHits = unlist(sweep(gramOldSignalCorrected['1',], 2, gramOldSignalSums, '/'))
gramOldFalseAlarms = unlist(sweep(gramOldNoiseCorrected['0',], 2, gramOldNoiseSums, '/'))

  # D #   Grammatical group on new items

# Removing FutImp and PastImp as we do not have Hits and FalseAlarms data
gramNewSignal[,c(1,3)] = NULL
gramNewNoise[,c(1,3)] = NULL

# Total of Signal and Noise trials
gramNewSignalSums = colSums(gramNewSignal)
gramNewNoiseSums = colSums(gramNewNoise)

# Correcting for extreme counts
gramNewSignalCorrected = correct_extremes(gramNewSignal, gramNewSignalSums)
gramNewNoiseCorrected = correct_extremes(gramNewNoise, gramNewNoiseSums)

# Hits and FalseAlarms
gramNewHits = unlist(sweep(gramNewSignalCorrected['1',], 2, gramNewSignalSums, '/'))
gramNewFalseAlarms = unlist(sweep(gramNewNoiseCorrected['0',], 2, gramNewNoiseSums, '/'))

### Plots ###

require(ggplot2)
require(grid)

oldpaidat = data.frame(
    Training = c(rep('Lex', 3), rep('Gram', 3)),
    hit = c(c(0, lexOldHits[2], 1), c(0, gramOldHits[2], 1)),
    fal = c(c(0, lexOldFalseAlarms[2], 1), c(0, gramOldFalseAlarms[2], 1))
)
p1 <- ggplot(data=oldpaidat, aes(x=fal, y=hit, group=Training)) +
    coord_cartesian(xlim=c(0.0,1.0), ylim=c(0.0,1.0)) +
    geom_line(aes(colour=Training, linetype=Training), linewidth=1.2) +
    geom_segment(aes(x=0, y=0, xend=1, yend=1), linetype='dotted', linewidth=1.0) +
    scale_color_manual(name='Training:', values=c('#b2182b','#1b7837'),
        breaks=c('Lex','Gram'),
        labels=c('Lexical','Grammatical')) +
    scale_linetype_manual(name='Training:', values=c(1,1),
        breaks=c('Lex','Gram'),
        labels=c('Lexical','Grammatical')) +
    scale_x_continuous('False-alarm rate') +
    scale_y_continuous('Hit rate') +
    ggtitle('Past Imperfective') +
    theme(plot.title=element_text(hjust=0.5))
# -
oldpapdat = data.frame(
    Training = c(rep('Lex', 3), rep('Gram', 3)),
    hit = c(c(0, lexOldHits[3], 1), c(0, gramOldHits[3], 1)),
    fal = c(c(0, lexOldFalseAlarms[3], 1), c(0, gramOldFalseAlarms[3], 1))
)
p2 <- ggplot(data=oldpapdat, aes(x=fal, y=hit, group=Training)) +
    coord_cartesian(xlim=c(0.0,1.0), ylim=c(0.0,1.0)) +
    geom_line(aes(colour=Training, linetype=Training), linewidth=1.2) +
    geom_segment(aes(x=0, y=0, xend=1, yend=1), linetype='dotted', linewidth=1.0) +
    scale_color_manual(name='Training:', values=c('#b2182b','#1b7837'),
        breaks=c('Lex','Gram'),
        labels=c('Lexical','Grammatical')) +
    scale_linetype_manual(name='Training:', values=c(1,1),
        breaks=c('Lex','Gram'),
        labels=c('Lexical','Grammatical')) +
    scale_x_continuous('False-alarm rate') +
    scale_y_continuous('Hit rate') +
    ggtitle('Past Perfective') +
    theme(legend.position='none',
        plot.title=element_text(hjust=0.5))
# -
oldpridat = data.frame(
    Training = c(rep('Lex', 3), rep('Gram', 3)),
    hit = c(c(0, lexOldHits[4], 1), c(0, gramOldHits[4], 1)),
    fal = c(c(0, lexOldFalseAlarms[4], 1), c(0, gramOldFalseAlarms[4], 1))
)
p3 <- ggplot(data=oldpridat, aes(x=fal, y=hit, group=Training)) +
    coord_cartesian(xlim=c(0.0,1.0), ylim=c(0.0,1.0)) +
    geom_line(aes(colour=Training, linetype=Training), linewidth=1.2) +
    geom_segment(aes(x=0, y=0, xend=1, yend=1), linetype='dotted', linewidth=1.0) +
    scale_color_manual(name='Training:', values=c('#b2182b','#1b7837'),
        breaks=c('Lex','Gram'),
        labels=c('Lexical','Grammatical')) +
    scale_linetype_manual(name='Training:', values=c(1,1),
        breaks=c('Lex','Gram'),
        labels=c('Lexical','Grammatical')) +
    scale_x_continuous('False-alarm rate') +
    scale_y_continuous('Hit rate') +
    ggtitle('Present Imperfective') +
    theme(legend.position='none',
        plot.title=element_text(hjust=0.5))
# -
oldfupdat = data.frame(
    Training = c(rep('Lex', 3), rep('Gram', 3)),
    hit = c(c(0, lexOldHits[1], 1), c(0, gramOldHits[1], 1)),
    fal = c(c(0, lexOldFalseAlarms[1], 1), c(0, gramOldFalseAlarms[1], 1))
)
p4 <- ggplot(data=oldfupdat, aes(x=fal, y=hit, group=Training)) +
    coord_cartesian(xlim=c(0.0,1.0), ylim=c(0.0,1.0)) +
    geom_line(aes(colour=Training, linetype=Training), linewidth=1.2) +
    geom_segment(aes(x=0, y=0, xend=1, yend=1), linetype='dotted', linewidth=1.0) +
    scale_color_manual(name='Training:', values=c('#b2182b','#1b7837'),
        breaks=c('Lex','Gram'),
        labels=c('Lexical','Grammatical')) +
    scale_linetype_manual(name='Training:', values=c(1,1),
        breaks=c('Lex','Gram'),
        labels=c('Lexical','Grammatical')) +
    scale_x_continuous('False-alarm rate') +
    scale_y_continuous('Hit rate') +
    ggtitle('Future Perfective') +
    theme(legend.position='none',
        plot.title=element_text(hjust=0.5))

pushViewport(viewport(layout=grid.layout(2, 4,
    heights=unit(c(1,5), 'null'),
    widths=unit(c(3,3,3,4.3), 'null'))))
grid.text(expression(bold('Seen Items')),
    vp=viewport(layout.pos.row=1, layout.pos.col=1:4, gp=gpar(fontsize=18)))
print(p3, vp=viewport(layout.pos.row=2, layout.pos.col=1))
print(p2, vp=viewport(layout.pos.row=2, layout.pos.col=2))
print(p4, vp=viewport(layout.pos.row=2, layout.pos.col=3))
print(p1, vp=viewport(layout.pos.row=2, layout.pos.col=4))

# -----

newpapdat = data.frame(
    Training = c(rep('Lex', 3), rep('Gram', 3)),
    hit = c(c(0, lexNewHits[2], 1), c(0, gramNewHits[2], 1)),
    fal = c(c(0, lexNewFalseAlarms[2], 1), c(0, gramNewFalseAlarms[2], 1))
)
p5 <- ggplot(data=newpapdat, aes(x=fal, y=hit, group=Training)) +
    coord_cartesian(xlim=c(0.0,1.0), ylim=c(0.0,1.0)) +
    geom_line(aes(colour=Training, linetype=Training), linewidth=1.2) +
    geom_segment(aes(x=0, y=0, xend=1, yend=1), linetype='dotted', linewidth=1.0) +
    scale_color_manual(name='Training:', values=c('#b2182b','#1b7837'),
        breaks=c('Lex','Gram'),
        labels=c('Lexical','Grammatical')) +
    scale_linetype_manual(name='Training:', values=c(1,1),
        breaks=c('Lex','Gram'),
        labels=c('Lexical','Grammatical')) +
    scale_x_continuous('False-alarm rate') +
    scale_y_continuous('Hit rate') +
    ggtitle('Past Perfective') +
    theme(legend.position='none',
        plot.title=element_text(hjust=0.5))
# -
newpridat = data.frame(
    Training = c(rep('Lex', 3), rep('Gram', 3)),
    hit = c(c(0, lexNewHits[3], 1), c(0, gramNewHits[3], 1)),
    fal = c(c(0, lexNewFalseAlarms[3], 1), c(0, gramNewFalseAlarms[3], 1))
)
p6 <- ggplot(data=newpridat, aes(x=fal, y=hit, group=Training)) +
    coord_cartesian(xlim=c(0.0,1.0), ylim=c(0.0,1.0)) +
    geom_line(aes(colour=Training, linetype=Training), linewidth=1.2) +
    geom_segment(aes(x=0, y=0, xend=1, yend=1), linetype='dotted', linewidth=1.0) +
    scale_color_manual(name='Training:', values=c('#b2182b','#1b7837'),
        breaks=c('Lex','Gram'),
        labels=c('Lexical','Grammatical')) +
    scale_linetype_manual(name='Training:', values=c(1,1),
        breaks=c('Lex','Gram'),
        labels=c('Lexical','Grammatical')) +
    scale_x_continuous('False-alarm rate') +
    scale_y_continuous('Hit rate') +
    ggtitle('Present Imperfective') +
    theme(legend.position='none',
        plot.title=element_text(hjust=0.5))
# -
newfupdat = data.frame(
    Training = c(rep('Lex', 3), rep('Gram', 3)),
    hit = c(c(0, lexNewHits[1], 1), c(0, gramNewHits[1], 1)),
    fal = c(c(0, lexNewFalseAlarms[1], 1), c(0, gramNewFalseAlarms[1], 1))
)
p7 <- ggplot(data=newfupdat, aes(x=fal, y=hit, group=Training)) +
    coord_cartesian(xlim=c(0.0,1.0), ylim=c(0.0,1.0)) +
    geom_line(aes(colour=Training, linetype=Training), linewidth=1.2) +
    geom_segment(aes(x=0, y=0, xend=1, yend=1), linetype='dotted', linewidth=1.0) +
    scale_color_manual(name='Training:', values=c('#b2182b','#1b7837'),
        breaks=c('Lex','Gram'),
        labels=c('Lexical','Grammatical')) +
    scale_linetype_manual(name='Training:', values=c(1,1),
        breaks=c('Lex','Gram'),
        labels=c('Lexical','Grammatical')) +
    scale_x_continuous('False-alarm rate') +
    scale_y_continuous('Hit rate') +
    ggtitle('Future Perfective') +
    theme(plot.title=element_text(hjust=0.5))

pushViewport(viewport(layout=grid.layout(2, 3,
    heights=unit(c(1,5), 'null'),
    widths=unit(c(3,3,4.3), 'null'))))
grid.text(expression(bold('Unseen Items')),
    vp=viewport(layout.pos.row=1, layout.pos.col=1:3, gp=gpar(fontsize=18)))
print(p6, vp=viewport(layout.pos.row=2, layout.pos.col=1))
print(p5, vp=viewport(layout.pos.row=2, layout.pos.col=2))
print(p7, vp=viewport(layout.pos.row=2, layout.pos.col=3))

