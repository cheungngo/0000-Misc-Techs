# Tutorial on meta-analysis in R
# R useR! Conference 2013
# Stephanie Kovalchik


###

# Evaluating Heterogeneity

###


# Q's test


# Q = Sum(Wi(Yi-theta)^2)
# The weighted devations about the summary effect size
# Larger --> greater between study heterogeneity


# When tau2 = 0, Q ~ Chi2(K-1)


# Using the bcg data again

data("dat.bcg")
es.bcg = escalc(ai=tpos,bi=tneg,ci=cpos,di=cneg, 
                data=dat.bcg, measure='OR',
                append=F)
res_bcg = rma(yi,vi, data = es.bcg, method = 'DL')

res_bcg$QE
# [1] 163.1649

# Note that Q is actually a chisq approx, so it would only be valid if the sample size is large


###


# Higgins' I^2


# I^2 = (Q-df)/Q x 100%
# Percentage of 'unexplained' variance
# Since the 'explained' variance would by Q = K-1 assuming tau2 = 0


# 0-30: Low
# 30-60: Mod
# 50-90: Substantial
# 75-100: Considerable


res_bcg$I2 # [1] 92.64548
# OR
with(res_bcg, (QE-(k-1))/QE) # [1] 0.9264548

# Note that method = 'DL' is quite important for this to work


# Proof of the formula of I2

x = 1:100
k = 10:50
ind = sapply(k, function(k) {
  y = pchisq(x,k-1)
  ind1 = which((y-0.5) > 0)
  ind2 = which.min(y[ind1]-0.5)
  ind[k] = ind1[ind2]
})
ind
# [1]  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35
# [28] 36 37 38 39 40 41 42 43 44 45 46 47 48 49

ind - (k-1)
#  [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
# [41] 0


# So for all k
# we assume p = 0.5 for heterogeneity, i.e. not biased by tau2
# => and we got the results for x (i.e. Q) equals to k-1 for every simulations
# Here is the prove for the calculation of I2


###


# H^2

# H^2 = Q/df
# H > 1 suggests unexplained homogeneity

sqrt(res_bcg$H2)
# [1] 3.687421

# OR

sqrt(res_bcg$QE / (res_bcg$k - 1))
# [1] 3.687421

# OR

sqrt(with(res_bcg, 1 / (1-(I2/100))))
# [1] 3.687421
# For 1/H2 = 1- I2/100


###


# Confidence intervals for heterogeneity


confint(res_bcg)
#        estimate   ci.lb   ci.ub
# tau^2    0.3663  0.1302  1.1812
# tau      0.6053  0.3608  1.0868
# I^2(%)  92.6455 81.7376 97.5971
# H^2     13.5971  5.4757 41.6164


###


