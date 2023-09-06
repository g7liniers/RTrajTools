source("src/loading/load_all-traj.R")

##### distribution of trajectory univariate features

########### Kolmogorov-Smirnov
## In ks tests, alternative hypothesis is that the two samples come from different distributions
## A low p value (<.01) supports the alternative hypothesis
## after the computation, pvalue = 0 means pvalue < 2.2e-16
###########
source("src/preanalysis/KS-test.R")

# ANT

#### Global
bol <- btraj.ANT
ks.pvalues.ANT <- ks_btraj_all_pval(bol)

#### Low
bol <- filter_traj(btraj.ANT, mix_values = 1:5 / 10)
ks.pvalues.ANT.low <- ks_btraj_all_pval(bol)

#### High
bol <- filter_traj(btraj.ANT, mix_values = 6:10 / 10)
ks.pvalues.ANT.high <- ks_btraj_all_pval(bol)



# GL

#### Global
bol <- btraj.GL
ks.pvalues.GL <- ks_btraj_all_pval(bol)

#### Low
bol <- filter_traj(btraj.GL, mix_values = 1:5 / 10)
ks.pvalues.GL.low <- ks_btraj_all_pval(bol)

#### High
bol <- filter_traj(btraj.GL, mix_values = 6:10 / 10)
ks.pvalues.GL.high <- ks_btraj_all_pval(bol)

# Visualization

ks.pvalues.df <-
  rbind(
    ks.pvalues.ANT,
    ks.pvalues.ANT.low,
    ks.pvalues.ANT.high,
    ks.pvalues.GL,
    ks.pvalues.GL.low,
    ks.pvalues.GL.high
  )
#

########### Wilcoxon
## In the wilcoxon tests, alternative hypothesis is that the two samples have different central tendencies
## A low p value (<.01) supports the alternative hypothesis
## after the computation, pvalue = 0 means pvalue < 2.2e-16
########

source("src/preanalysis/wilcoxon-test.R")

# ANT

#### Global
bol <- btraj.ANT
wilcoxon.pvalues.ANT <- wilcoxon_btraj_all_pval(bol)

#### Low
bol <- filter_traj(btraj.ANT, mix_values = 1:5 / 10)
wilcoxon.pvalues.ANT.low <- wilcoxon_btraj_all_pval(bol)

#### High
bol <- filter_traj(btraj.ANT, mix_values = 6:10 / 10)
wilcoxon.pvalues.ANT.high <- wilcoxon_btraj_all_pval(bol)



# GL

#### Global
bol <- btraj.GL
wilcoxon.pvalues.GL <- wilcoxon_btraj_all_pval(bol)

#### Low
bol <- filter_traj(btraj.GL, mix_values = 1:5 / 10)
wilcoxon.pvalues.GL.low <- wilcoxon_btraj_all_pval(bol)

#### High
bol <- filter_traj(btraj.GL, mix_values = 6:10 / 10)
wilcoxon.pvalues.GL.high <- wilcoxon_btraj_all_pval(bol)

# Visualization

wilcoxon.pvalues.df <-
  rbind(
    wilcoxon.pvalues.ANT,
    wilcoxon.pvalues.ANT.low,
    wilcoxon.pvalues.ANT.high,
    wilcoxon.pvalues.GL,
    wilcoxon.pvalues.GL.low,
    wilcoxon.pvalues.GL.high
  )