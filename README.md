
<!-- README.md is generated from README.Rmd. Please edit that file -->


# DAF (Differential Algorithmic Functioning)

As algorithmic decision making is increasingly deployed in every walk of life, many researchers have raised concerns about fairness-related bias from such algorithms. But there is little research on harnessing psychometric methods to uncover potential discriminatory bias inside decision making algorithms. The main goal of this paper is to propose a new framework for algorithmic fairness based on *differential item functioning* (DIF), which has been commonly used to measure item fairness in psychometrics. Our fairness notion, which we call *differential algorithmic functioning* (DAF), is defined based on three pieces of information: a decision variable, a "fair" variable, and a protected variable such as race or gender. Under the DAF framework, an algorithm can exhibit uniform DAF, nonuniform DAF, or neither (i.e., non-DAF). For detecting DAF, we provide modifications of well-established DIF methods: Mantel-Haenszel test, logistic regression, and residual-based DAF. We demonstrate our framework through a real dataset concerning decision-making algorithms for grade retention in K-12 education in the United States. See Suk
and Han (2022)
\<[doi: 10.31234/osf.io/4cpdn](https://psyarxiv.com/4cpdn)\>
for details.


## Example

- Mantel-Haenszel test

``` r
# basic function in R
dafMH <- mantelhaen.test(x = dat$decision, # decision
                         y = dat$female,   # group membership
                         z = fairatt_str)  # strata of the fair attribute  
```


- Logistic regression

``` r
# basic function in R
dafLogistic <- glm(decision ~ priorscore, data=dat, family=binomial)
dafLogistic <- glm(decision ~ priorscore * female, data=dat, family=binomial)

anova(df1_difLogistic_0, df1_difLogistic)
summary(dafLogistic)
```

- Residual-based DAF

``` r
source(".../R/residualDAF.R") # read R code for residual-based DAF.

dafResidual <- residualDAF(decision = dat$decision, # decision
                           group = dat$female, # group membership
                           fair.att = dat$priorscore # fair attribute)
dafResidual$resi_summary
dafResidual$stat_summary
```
