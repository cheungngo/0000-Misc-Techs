# Tutorial on meta-analysis in R
# R useR! Conference 2013
# Stephanie Kovalchik


###

# Calculating the effect sizes

###


# Common and necessary packages

install.packages(c('meta','metafor','rmeta'))
library(meta)
library(metafor)
library(rmeta)


###


# two sample datasets
# dat.bcg and amlodipine

data(package='metafor')
# Data sets in package 'metafor':
# dat.bangertdrowns2004    Studies on the Effectiveness of Writing-to-Learn
#                          Interventions
# dat.bcg                  Studies on the Effectiveness of the BCG Vaccine
#                          Against Tuberculosis
# dat.begg1989             Studies on Bone-Marrow Transplantation versus
#                          Chemotherapy for the Treatment of Leukemia
# ...
data(dat.bcg)
str(dat.bcg)
# 'data.frame':	13 obs. of  9 variables:
# ...

data(package='meta')
# Data sets in package 'meta':
# Fleiss93                 Aspirin after Myocardial Infarction
# Fleiss93_CR              Import RevMan 5 data files (.csv)
# Fleiss93cont             Mental Health Treatment
# Olkin95                  Thrombolytic Therapy after Acute Myocardial
#                          Infarction
# amlodipine               Amlodipine for Work Capacity
# ...
data("amlodipine")
str(amlodipine)
# 'data.frame':	8 obs. of  7 variables:
#  $ study    : Factor w/ 8 levels "Protocol 154",..: 1 2 3 4 5 6 7 8
# ...


###


# Effect sizes

# Conventional meta-analytic models assume normality of ESs
# Because of the CLT, this will hold given large enough samples
# To normalize ESs, log transform is common


# Using the dat.bcg as an example

str(dat.bcg)
?dat.bcg
# ...
# Results from 13 studies examining the effectiveness 
# of the Bacillus Calmette-Guerin (BCG) vaccine against tuberculosis.
# ...
# tpos	numeric	 number of TB positive cases in the treated (vaccinated) group
# tneg	numeric	 number of TB negative cases in the treated (vaccinated) group
# cpos	numeric	 number of TB positive cases in the control (non-vaccinated) group
# cneg	numeric	 number of TB negative cases in the control (non-vaccinated) group
# ...

# We can calculate the OR, but to normalize that, we need to give it a log
# LOR = log((tpos/tneg)/(cpos/cneg)) 
# Var = 1/tpos + 1/tneg + 1/cpos + 1/cneg

# But that would be so clumpsy
# And escalc() in metafor could do the work

es.bcg = escalc(ai=tpos,bi=tneg,ci=cpos,di=cneg, data=dat.bcg, measure='OR')
names(es.bcg)
#  [1] "trial"  "author" "year"   "tpos"   "tneg"   "cpos"   "cneg"   "ablat"  "alloc" 
# [10] "yi"     "vi" 
# Note the addition of 'yi' and 'vi'


###


# Exploring escalc()

?escalc
# Making it simple, we can summarize that there are few kinds of input leading to
# Several groups of effect sizes


###


# 1. Measures for Dichotomous Variables


# group 1	ai	bi	n1i
# group 2	ci	di	n2i


# The options for the measure argument are then:
# "RR" for the log risk ratio.
# "OR" for the log odds ratio.
# "RD" for the risk difference.
# "AS" for the arcsine square root transformed risk difference (Rücker et al., 2009).
# "PETO" for the log odds ratio estimated with Peto's method (Yusuf et al., 1985).


# Note that the log is taken of the risk ratio and the odds ratio


# Calculating OR, RR and RD using the dat.bcg
RR.bcg = escalc(ai=tpos,bi=tneg,ci=cpos,di=cneg, data=dat.bcg, measure='RR', append = F)
OR.bcg = escalc(ai=tpos,bi=tneg,ci=cpos,di=cneg, data=dat.bcg, measure='OR', append = F)
RD.bcg = escalc(ai=tpos,bi=tneg,ci=cpos,di=cneg, data=dat.bcg, measure='RD', append = F)


###


# 2. SMD based on dichotomous data


# Assuming that the dichotomous outcome is actually a dichotomized version 
# of the responses on an underlying quantitative scale, 
# it is also possible to estimate the standardized mean difference 
# based on 2x2 table data


# sample dataset:
data("dat.gibson2002")
?dat.gibson2002
# ...
# Results from 15 trials examining the effectiveness of self-management education 
# and regular medical review for adults with asthma.
# ...
# n1i	numeric	 number of participants in the intervention group
# m1i	numeric	 mean number of days off work/school in the intervention group
# sd1i	numeric	 standard deviation of the number of days off work/school in the intervention group
# n2i	numeric	 number of participants in the control/comparison group
# m2i	numeric	 mean number of days off work/school in the control/comparison group
# sd2i	numeric	 standard deviation of the number of days off work/school in the control/comparison group
# ai	numeric	 number of participants who had one or more days off work/school in the intervention group
# bi	numeric	 number of participants who no days off work/school in the intervention group
# ci	numeric	 number of participants who had one or more days off work/school in the control/comparison group
# di	numeric	 number of participants who no days off work/school in the control/comparison group


dat.gibson2002
# As what we have observed, there has really be n/m/s in some group
# While in others they are represented by a/b/c/d
# That's why we need the transformation


# Type of measures used:
# "PBIT" for the probit transformed risk difference as an estimate of the standardized mean difference.
# "OR2DN" for the transformed odds ratio as an estimate of the standardized mean difference (normal distributions).
# "OR2DL" for the transformed odds ratio as an estimate of the standardized mean difference (logistic distributions).


# The probit transformation assumes that the responses on the underlying quantitative scale are normally distributed. 
# There are two versions of the odds ratio transformation, 
# the first also assuming normal distributions within the two groups, 
# while the second assumes that the responses follow logistic distributions.


# Try transforming the data
OR2DN_gibson = escalc(measure = 'OR2DN', data = dat.gibson2002,
                      ai=ai,bi=bi,ci=ci,di=di,
                      append = F)
OR2DN_gibson
# So they would only calculate the entries with a/b/c/d


###


# 3. Measures for Quantitative Variables


# Sample measures
# group 1	m1i	sd1i	n1i
# group 2	m2i	sd2i	n2i


# The options for the measure argument are then:
# "MD" for the raw mean difference.
# "SMD" for the standardized mean difference.
# "SMDH" for the standardized mean difference with heteroscedastic population variances in the two groups (Bonett, 2008, 2009).
# "ROM" for the log transformed ratio of means (Hedges et al., 1999; Lajeunesse, 2011).


###


# 4. Calculating SMD for the rest of the gibson2002 data


dat.gibson2002
SMD_gibson = escalc(measure = 'SMD', data = dat.gibson2002,
                    n1i=n1i,m1i=m1i,sd1i=sd1i,n2i=n2i,m2i=m2i,sd2i=sd2i,
                    append = F)
# We can even combine the OR2DN_gibson and the SMD_gibson
ind = which(is.na(SMD_gibson[,1]))
SMD_gibson[ind,] = OR2DN_gibson[ind,]
SMD_gibson
# So we got all the SMD for the meta-analysis


# ROM (log transformed ratio of means) exemplified by dat.curtis1998


data("dat.curtis1998")
?dat.curtis1998
# ...
# Results from studies examining the effects of elevated CO2 levels on woody plant mass.
# ...
# m1i	numeric	 mean plant mass under elevated CO2 level (treatment group)
# sd1i	numeric	 standard deviation of plant mass underelevated CO2 level (treatment group)
# n1i	numeric	 number of observations under elevated CO2 level (treatment group)
# m2i	numeric	 mean plant mass under ambient CO2 level (control group)
# sd2i	numeric	 standard deviation of plant mass under ambient CO2 level (control group)
# n2i	numeric	 number of observations under ambient CO2 level (control group)


# Some explanations from the original paper that why ROM is used:
# The response ratio, the ratio of some measured quantity
# in experimental and control groups, is commonly
# used as a measure of experimental effect because it
# quantifies the proportionate change that results from
# an experimental manipulation. Examples of such ratios
# include relative competition intensity, relative yield,
# and relative crowding coefficient. 


dat.curtis1998
ROM_curtis = escalc(data=dat.curtis1998, measure = 'ROM',
                    n1i=n1i,m1i=m1i,sd1i=sd1i,n2i=n2i,m2i=m2i,sd2i=sd2i,
                    append = F)
ROM_curtis


###


# 5. Outcome Measures for Variable Association, for Two Quantitative Variables


# Sample dataset:
data("dat.mcdaniel1994")
?dat.mcdaniel1994
# ..
# Results from 160 studies on the correlation 
# between employment interview assessments and job performance
# ..
# ni	numeric	 sample size of the study
# ri	numeric	 observed correlation
# ..


# possible 'measure' arguments:
# "COR" for the raw correlation coefficient.
# "UCOR" for the raw correlation coefficient corrected for its slight negative bias (based on equation 2.3 in Olkin & Pratt, 1958).
# "ZCOR" for Fisher's r-to-z transformed correlation coefficient (Fisher, 1921).

# About the 'ZCOR'
# From https://blogs.sas.com/content/iml/2017/09/20/fishers-transformation-correlation.html
# Pearson's correlation measures the linear association between two variables. 
# Because the correlation is bounded between [-1, 1], 
# the sampling distribution for highly correlated variables is highly skewed. 
# Even for bivariate normal data, the skewness makes it challenging to estimate 
# confidence intervals for the correlation, to run one-sample hypothesis tests 
# ("Is the correlation equal to 0.5?"), and to run two-sample hypothesis tests 
# ("Do these two samples have the same correlation?").

# In 1921, R. A. Fisher studied the correlation of bivariate normal data and 
# discovered a wonderful transformation (shown to the right) that 
# converts the skewed distribution of the sample correlation (r) 
# into a distribution that is approximately normal. 
# Furthermore, whereas the variance of the sampling distribution of r 
# depends on the correlation, the variance of the transformed distribution 
# is independent of the correlation.


dat.mcdaniel1994
ZCOR_mcdanie = escalc(data = dat.mcdaniel1994, measure = 'ZCOR',
                      ri=ri, ni=ni,
                      append = F)
ZCOR_mcdanie


###


# 6. Measures for Event Counts


# esults are sometimes reported in terms of event counts 
# (i.e., the number of events, such as strokes or myocardial infarctions) 
# over a certain period of time comparing two treatment groups


# common input:
# group 1	x1i	t1i
# group 2	x2i	t2i

# where x1i and x2i denote the total number of events in the first and the second group, respectively, 
# and t1i and t2i the corresponding total person-times at risk


# Sample data: dat.hart1999
data("dat.hart1999")
?dat.hart1999
# ..
# Results from 6 clinical trials examining the effectiveness of adjusted-dose warfarin 
# for preventing strokes in patients with atrial fibrillation
# ..
# x1i	numeric	 number of strokes in the warfarin group
# n1i	numeric	 number of patients in the warfarin group
# t1i	numeric	 total person-time (in years) in the warfarin group
# x2i	numeric	 number of strokes in the placebo/control group
# n2i	numeric	 number of patients in the placebo/control group
# t2i	numeric	 total person-time (in years) in the placebo/control group
# ..


# Options for measures:
# "IRR" for the log incidence rate ratio.
# "IRD" for the incidence rate difference.
# "IRSD" for the square root transformed incidence rate difference


# Calculating the IRR

IRR_hart = escalc(data=dat.hart1999, measure = 'IRR',
                  x1i=x1i,t1i=t1i,x2i=x2i,t2i=t2i,
                  append = F)
IRR_hart


###


# For other options for measure, please kindly refer to the ?escalc


###