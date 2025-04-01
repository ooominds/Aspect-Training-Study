
rm(list=ls())

load('postdatL.rda')

############################
### Log-linear modelling ###
############################

tabdat = xtabs(~ Correct + IsCorrect + OldNew + GROUP, data=postdatL)

ll.indep = loglin(tabdat, list(c(1), c(2,3,4)), param=TRUE, fit=TRUE)
ll.indep$lrt # [1] 832.9085
1 - pchisq(ll.indep$lrt, ll.indep$df) # [1] 0

ll.32 = loglin(tabdat, list(c(1,3,2), c(2,3,4)), param=TRUE, fit=TRUE)
ll.32$lrt # [1] 13.12575
1 - pchisq(ll.32$lrt, ll.32$df) # [1] 0.01067743

ll.32and4 = loglin(tabdat, list(c(1,2,3), c(1,4), c(2,3,4)), param=TRUE, fit=TRUE)
ll.32and4$lrt # [1] 2.853522
1 - pchisq(ll.32and4$lrt, ll.32and4$df) # [1] 0.4147649
# does this model improve fit significantly?
1 - pchisq(abs(ll.32and4$lrt-ll.32$lrt),
    abs(ll.32and4$df-ll.32$df))
# [1] 0.001350477 --- YES

### Plots ###

require(ggplot2)
require(grid)

freqdat = data.frame(cbind(data.frame(tabdat),
    'loglinFreq'=data.frame(ll.32and4$fit)[,5]))
freqdat$SameCorr = interaction(freqdat$OldNew, freqdat$IsCorrect)
freqdat$SameCorr = factor(freqdat$SameCorr,
    levels=c('old.correct','old.incorrect','new.correct','new.incorrect'))

lex = droplevels(freqdat[freqdat$GROUP=='LEX',])
gramm = droplevels(freqdat[freqdat$GROUP=='GRAM',])

p1 <- ggplot(data=lex, aes(x=SameCorr, y=loglinFreq, fill=factor(Correct))) +
    geom_bar(position=position_dodge(.9), width=0.5, stat='identity') +
    scale_x_discrete(name='Item', guide=guide_axis(n.dodge=2),
        breaks=c('old.correct','old.incorrect','new.correct','new.incorrect'),
        labels=c('seen.gramm','seen.ungramm','unseen.gramm','unseen.ungramm')) +
    scale_y_continuous('Predicted Frequency', limits=c(0, 650)) +
    scale_fill_manual(values=c('#b2182b','#1b7837'),
        name='Participants\'\nanswers:',
        breaks=c(0,1),
        labels=c('incorrect','correct')) +
    ggtitle('Lexical training') +
    theme(legend.position='none',
        plot.title=element_text(hjust=0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.text=element_text(size=10))
p2 <- ggplot(data=gramm, aes(x=SameCorr, y=loglinFreq, fill=factor(Correct))) +
    geom_bar(position=position_dodge(.9), width=0.5, stat='identity') +
    scale_x_discrete(name='Item', guide=guide_axis(n.dodge=2),
        breaks=c('old.correct','old.incorrect','new.correct','new.incorrect'),
        labels=c('seen.gramm','seen.ungramm','unseen.gramm','unseen.ungramm')) +
    scale_y_continuous('Predicted Frequency', limits=c(0, 650)) +
    scale_fill_manual(values=c('#b2182b','#1b7837'),
        name='Participants\'\nanswers:',
        breaks=c(0,1),
        labels=c('incorrect','correct')) +
    ggtitle('Grammatical training') +
    theme(plot.title=element_text(hjust=0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.text=element_text(size=10))

pushViewport(viewport(
    layout=grid.layout(1, 2, heights=unit(c(5), 'null'),
        widths=unit(c(5,6.18), 'null'))))
print(p1, vp=viewport(layout.pos.row=1, layout.pos.col=1))
print(p2, vp=viewport(layout.pos.row=1, layout.pos.col=2))

################################################################################

#######################################
### Logistic mixed effect modelling ###
#######################################

rm(list=ls())

require(brms)
require(posterior)

load('postdatL.rda')

# releveling and sum-coding
postdatL$GROUP = relevel(postdatL$GROUP, ref='LEX')
contrasts(postdatL$GROUP) = 'contr.sum'
contrasts(postdatL$OldNew) = 'contr.sum'
contrasts(postdatL$IsCorrect) = 'contr.sum'

# scaling the order
postdatL$Order.z = as.numeric(scale(postdatL$Order))

# # Note: running this model can take a lot of time!
# summary.brmMain <- summary(brmMain <- brm(Correct ~
#     Order.z +
#     GROUP + OldNew + IsCorrect +
#     s(SentenceID, bs='re') +
#     s(Order.z, ID, bs='fs', m=1),
#     family='bernoulli',
#     data=postdatL,
#     chains=4, iter=4000, cores=4,
#     save_pars=save_pars(all=TRUE),
#     control=list(adapt_delta=0.98,
#         max_treedepth=10)))
# save(brmMain, file='brmMain.rda')
# save(summary.brmMain, file='summary.brmMain.rda')

load('summary.brmMain.rda')
summary.brmMain
# Smooth Terms:
#                    Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sds(sSentenceID_1)     0.90      0.10     0.73     1.11 1.00     2528     4234
# sds(sOrder.zID_1)      0.29      0.20     0.01     0.72 1.00     2354     3481
# sds(sOrder.zID_2)      3.81      0.51     2.86     4.88 1.00     3669     5626
#
# Population-Level Effects:
#            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept      0.48      0.13     0.23     0.74 1.00     2182     3625
# Order.z        0.06      0.12    -0.17     0.30 1.00     2154     3633
# GROUP1        -0.12      0.06    -0.23    -0.01 1.00     5083     5905
# OldNew1        0.74      0.12     0.51     0.99 1.00     1757     2660
# IsCorrect1     0.65      0.12     0.42     0.89 1.00     1953     3332

### Plots ###

require(ggplot2)
require(grid)

load('brmMain.rda')

condition = make_conditions(brmMain, 'GROUP')
combinedPlot = plot(conditional_effects(brmMain,
    effects=c('OldNew:IsCorrect'), conditions=condition),
    ask=FALSE, plot=FALSE)

comboPlot = combinedPlot[[1]] +
    scale_x_discrete(name='Seen or unseen items',
        breaks=c('old','new'),
        labels=c('seen','unseen')) +
    scale_y_continuous('Correct answers', limits=c(0.30, 1.00)) +
    scale_fill_manual(name='Item\ngrammaticality:',
        breaks=c('correct','incorrect'),
        labels=c('grammatical','ungrammatical'),
        values=c('#b2182b','#1b7837')) +
    scale_color_manual(name='Item\ngrammaticality:',
        breaks=c('correct','incorrect'),
        labels=c('grammatical','ungrammatical'),
        values=c('#b2182b','#1b7837')) +
    facet_wrap(.~GROUP,
        labeller=as_labeller(c(LEX='Lexical training',GRAM='Grammatical training'))) +
    guides(fill=guide_legend(title='Item\ngrammaticality:'),
        color=guide_legend(title='Item\ngrammaticality:')) +
    theme(plot.title=element_text(size=14,hjust=0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.text=element_text(size=10))

plot(comboPlot)

