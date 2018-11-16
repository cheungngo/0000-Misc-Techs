# Tutorial on meta-analysis in R
# R useR! Conference 2013
# Stephanie Kovalchik


###

# Summarizing effects

###


# Brief intro


# Introduction to modeling approaches
# Fixed: same mean ES, zero between-study var
# Random: different mean ES, between-study var
# Mixed: Study-level regression for ES


# FE vs RE
# FE is a description of K studies
# RE regards K studies as a sample of a larger universe of studies
# The RE can be used to infer what would likely happen if a new study were performed
# FE model cannot
# Common practice: to report both


# Using rma() in metafor package
# rma stand for random effects meta-analysis


?rma
# Usage:
# rma(yi, vi, sei, weights, ai, bi, ci, di, n1i, n2i, x1i, x2i, t1i, t2i,
# m1i, m2i, sd1i, sd2i, xi, mi, ri, ti, sdi, r2i, ni, mods,
# measure="GEN", intercept=TRUE, data, slab, subset,
# add=1/2, to="only0", drop00=FALSE, vtype="LS",
# method="REML", weighted=TRUE, test="z",
# level=95, digits=4, btt, tau2, verbose=FALSE, control, ...)

# Note that it is sort of a wrapper for escalc
# So we can even calculate the effect size here if we forgot

# methods for rma:
# method="DL" = DerSimonian-Laird estimator
# method="HE" = Hedges estimator
# method="HS" = Hunter-Schmidt estimator
# method="SJ" = Sidik-Jonkman estimator
# method="ML" = maximum-likelihood estimator
# method="REML" = restricted maximum-likelihood estimator
# method="EB" = empirical Bayes estimator
# method="PM" = Paule-Mandel estimator
# method="GENQ" = generalized Q-statistic estimator

# defaulted would be 'REML'
# And of course 'FE' represents fixed model

# It cited that Viechtbauer's simulation study suggests 
# REML has the most recommendable properties


###


# Fitting the fixed effect model


# Using the dat.bcg as an example for dichotomous data

?dat.bcg
data("dat.bcg")
es.bcg = escalc(ai=tpos,bi=tneg,ci=cpos,di=cneg, 
                data=dat.bcg, measure='OR',
                append=F)
res_bcg = rma(yi,vi, data = es.bcg, method = 'FE')


# Exploring the rma Class

names(res_bcg)
#  [1] "b"         "beta"      "se"        "zval"      "pval"      "ci.lb"    
#  [7] "ci.ub"     "vb"        "tau2"      "se.tau2"   "tau2.fix"  "k"        
# [13] "k.f"       "k.eff"     "p"         "p.eff"     "parms"     "m"        
# [19] "QE"        "QEp"       "QM"        "QMp"       "I2"        "H2"       
# [25] "R2"        "int.only"  "int.incl"  "allvipos"  "coef.na"   "yi"       
# [31] "vi"        "X"         "weights"   "yi.f"      "vi.f"      "X.f"      
# [37] "weights.f" "M"         "ai.f"      "bi.f"      "ci.f"      "di.f"     
# [43] "x1i.f"     "x2i.f"     "t1i.f"     "t2i.f"     "ni"        "ni.f"     
# [49] "ids"       "not.na"    "subset"    "slab"      "slab.null" "measure"  
# [55] "method"    "weighted"  "test"      "dfs"       "s2w"       "btt"      
# [61] "intercept" "digits"    "level"     "control"   "verbose"   "add"      
# [67] "to"        "drop00"    "fit.stats" "version"   "model"     "call"


# Frequently used Elements

res_bcg$b # intrcpt -0.4361391  # estimated coefficients of the model
res_bcg$ci.lb # [1] -0.5189778  # lower bound of the confidence intervals for the coefficients.
res_bcg$ci.ub # [1] -0.3533003  # upper bound of the confidence intervals for the coefficients
res_bcg$vb # intrcpt 0.001786369
res_bcg$fit.stats # a list with the log-likelihood, deviance, AIC, BIC, and AICc values under the unrestricted and restricted likelihood
res_bcg$yi # vector of study effect sizes
res_bcg$vi # vector of effect size variances


# Methods for rma

coef(res_bcg) # just giving $b
#    intrcpt 
# -0.4361391 

# confint() # for random effect models


summary(res_bcg)
# Fixed-Effects Model (k = 13)
#    logLik  deviance       AIC       BIC      AICc  
#  -76.0290  163.1649  154.0580  154.6229  154.4216  
# Test for Heterogeneity: 
#  Q(df = 12) = 163.1649, p-val < .0001
# Model Results:
# estimate      se      zval    pval    ci.lb    ci.ub     
#  -0.4361  0.0423  -10.3190  <.0001  -0.5190  -0.3533  ***


###


# Fitting the Random Effects Model


# Exemplified by 'amlodipine'

data("amlodipine")
amlodipine
SMD_amlo = escalc(measure = 'SMD', data = amlodipine, append = F,
                  n1i = n.amlo, m1i = mean.amlo, sd1i = sqrt(var.amlo),
                  n2i = n.plac, m2i = mean.plac, sd2i = sqrt(var.plac))
res_amlo = rma(yi=yi, vi=vi, data = SMD_amlo, method = 'REML', measure = 'SMD')
# actually the method = and the measure = can be omitted

summary(res_amlo)
#   logLik  deviance       AIC       BIC      AICc  
#  -1.6293    3.2586    7.2586    7.1504   10.2586  
# tau^2 (estimated amount of total heterogeneity): 0.0076 (SE = 0.0330)
# tau (square root of estimated tau^2 value):      0.0873
# I^2 (total heterogeneity / total variability):   11.75%
# H^2 (total variability / sampling variability):  1.13
# Test for Heterogeneity: 
#   Q(df = 7) = 9.7728, p-val = 0.2018
# Model Results:
#   estimate      se    zval    pval   ci.lb   ci.ub     
#     0.4228  0.0898  4.7092  <.0001  0.2468  0.5987  ***


# assessing the tau

res_amlo$tau2 # [1] 0.007617537
res_amlo$se.tau2 # [1] 0.03301923
res_amlo$tau2 + c(-1,1)*res_amlo$se.tau2*qnorm(1-0.025)
# [1] -0.05709896  0.07233404


###