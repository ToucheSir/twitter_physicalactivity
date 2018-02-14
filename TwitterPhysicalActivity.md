Twitter and Physical Activity
================

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you execute code within the notebook, the results appear beneath the code.

Try executing this chunk by clicking the *Run* button within the chunk or by placing your cursor inside it and pressing *Ctrl+Shift+Enter*.

``` r
options(repos = c(CRAN = "https://cran.rstudio.com"))
install.packages("betareg")
```

    ## Installing package into '/home/brianc/R/x86_64-pc-linux-gnu-library/3.4'
    ## (as 'lib' is unspecified)

``` r
install.packages("ggplot2")
```

    ## Installing package into '/home/brianc/R/x86_64-pc-linux-gnu-library/3.4'
    ## (as 'lib' is unspecified)

``` r
install.packages("fitdistrplus")
```

    ## Installing package into '/home/brianc/R/x86_64-pc-linux-gnu-library/3.4'
    ## (as 'lib' is unspecified)

``` r
install.packages("logspline")
```

    ## Installing package into '/home/brianc/R/x86_64-pc-linux-gnu-library/3.4'
    ## (as 'lib' is unspecified)

``` r
install.packages("Hmisc")
```

    ## Installing package into '/home/brianc/R/x86_64-pc-linux-gnu-library/3.4'
    ## (as 'lib' is unspecified)

``` r
library(foreign)
library(betareg)
library(ggplot2)
library(fitdistrplus)
```

    ## Loading required package: MASS

    ## Loading required package: survival

``` r
library(logspline)
library(Hmisc)
```

    ## Loading required package: lattice

    ## Loading required package: Formula

    ## 
    ## Attaching package: 'Hmisc'

    ## The following objects are masked from 'package:base':
    ## 
    ##     format.pval, units

``` r
library(knitr)
```

The compiled dataset contains a subset of US counties (names and FIPS codes). The associated data for each county is as follows: \* All\_Tweets: the total collected number of tweets from that county \* PA\_Tweets: the number of tweets identified as physical activity-related \* Inactivity\_Cases: TODO \* Inactivity\_Percent: TODO \* Inactivity\_Percent\_AgeAdjusted: TODO \* Gini\_Index: TODO \* Gini\_Index\_MoE: the margin of error of the Gini index estimate

``` r
all_data <- read.csv("all_data.csv")
all_data[0:5,]
```

    ##   FIPS PA_Tweets All_Tweets Inactivity_Cases Inactivity_Percent
    ## 1 1001       104       6063            11342               28.6
    ## 2 1003       511      27019            32856               22.3
    ## 3 1005         5        802             6578               31.8
    ## 4 1007         1        591             5846               33.9
    ## 5 1009        23       1681            11954               28.0
    ##   Inactivity_Percent_AgeAdjusted      FIPS_Long                    Name
    ## 1                           28.0 0500000US01001 Autauga County, Alabama
    ## 2                           21.2 0500000US01003 Baldwin County, Alabama
    ## 3                           31.0 0500000US01005 Barbour County, Alabama
    ## 4                           33.3 0500000US01007    Bibb County, Alabama
    ## 5                           27.1 0500000US01009  Blount County, Alabama
    ##   Gini_Index Gini_Index_MoE
    ## 1     0.4031         0.0149
    ## 2     0.4455         0.0095
    ## 3     0.4658         0.0146
    ## 4     0.4500         0.0468
    ## 5     0.4144         0.0137

Not all counties in the dataset have tweet or physical activity data available, so we must filter those out. We must also normalize the percentages (expressed here as a number /100) to values we can use for a beta distribution (i.e. 0-1)

``` r
# Remove rows with missing values
cleaned_data <- na.omit(all_data)
# Normalize percentages to the interval [0,1]
cleaned_data$Inactivity_Percent <- cleaned_data$Inactivity_Percent / 100
cleaned_data$Inactivity_Percent_AgeAdjusted <- cleaned_data$Inactivity_Percent_AgeAdjusted / 100

cleaned_data$PA_Tweets_Log <- log1p(cleaned_data$PA_Tweets)
vars <- c(2:6, 9:11)
summary(cleaned_data[vars])
```

    ##    PA_Tweets         All_Tweets      Inactivity_Cases  Inactivity_Percent
    ##  Min.   :    0.0   Min.   :      0   Min.   :     16   Min.   :0.0810    
    ##  1st Qu.:    5.0   1st Qu.:    427   1st Qu.:   2214   1st Qu.:0.2260    
    ##  Median :   20.0   Median :   1450   Median :   5285   Median :0.2580    
    ##  Mean   :  271.2   Mean   :  15386   Mean   :  16600   Mean   :0.2595    
    ##  3rd Qu.:   99.0   3rd Qu.:   5942   3rd Qu.:  12773   3rd Qu.:0.2940    
    ##  Max.   :66486.0   Max.   :2408076   Max.   :1310363   Max.   :0.4140    
    ##  Inactivity_Percent_AgeAdjusted   Gini_Index     Gini_Index_MoE   
    ##  Min.   :0.084                  Min.   :0.3296   Min.   :0.00160  
    ##  1st Qu.:0.214                  1st Qu.:0.4147   1st Qu.:0.01360  
    ##  Median :0.245                  Median :0.4356   Median :0.02000  
    ##  Mean   :0.247                  Mean   :0.4378   Mean   :0.02332  
    ##  3rd Qu.:0.280                  3rd Qu.:0.4592   3rd Qu.:0.02900  
    ##  Max.   :0.398                  Max.   :0.5985   Max.   :0.16180  
    ##  PA_Tweets_Log   
    ##  Min.   : 0.000  
    ##  1st Qu.: 1.792  
    ##  Median : 3.045  
    ##  Mean   : 3.288  
    ##  3rd Qu.: 4.605  
    ##  Max.   :11.105

The count data is heavily right-skewed with notable outliers. This is partly attributable to the disparity in population between counties (e.g. LA, the most populous county, has a similar relative magnitude of tweets)

``` r
par(mfrow=c(ceiling(length(vars) / 2), 2))
for (v in vars) {
  boxplot(cleaned_data[v], main = colnames(cleaned_data)[v], horizontal = TRUE)
}
```

![](TwitterPhysicalActivity_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
par(mfrow=c(2, 2))
hist(cleaned_data$Inactivity_Percent)
descdist(cleaned_data$Inactivity_Percent)
```

    ## summary statistics
    ## ------
    ## min:  0.081   max:  0.414 
    ## median:  0.258 
    ## mean:  0.2595285 
    ## estimated sd:  0.05202181 
    ## estimated skewness:  -0.03363435 
    ## estimated kurtosis:  2.907298

``` r
hist(cleaned_data$Inactivity_Percent_AgeAdjusted)
descdist(cleaned_data$Inactivity_Percent_AgeAdjusted)
```

![](TwitterPhysicalActivity_files/figure-markdown_github/unnamed-chunk-5-1.png)

    ## summary statistics
    ## ------
    ## min:  0.084   max:  0.398 
    ## median:  0.245 
    ## mean:  0.2469793 
    ## estimated sd:  0.04960239 
    ## estimated skewness:  0.05559656 
    ## estimated kurtosis:  2.844852

``` r
fit.beta1 <- fitdist(cleaned_data$Inactivity_Percent, "beta")
plot(fit.beta1)
```

![](TwitterPhysicalActivity_files/figure-markdown_github/unnamed-chunk-5-2.png)

``` r
fit.beta2 <- fitdist(cleaned_data$Inactivity_Percent_AgeAdjusted, "beta")
plot(fit.beta2)
```

![](TwitterPhysicalActivity_files/figure-markdown_github/unnamed-chunk-5-3.png) Correlation of variables:

``` r
displayCorr <- function(results) {
  knitr::kable(list(results$r, results$P), "markdown")
}
#cor(cleaned_data[vars], method = "pearson")
displayCorr(rcorr(as.matrix(cleaned_data[vars]), type = "pearson"))
```

<table>
<colgroup>
<col width="18%" />
<col width="7%" />
<col width="7%" />
<col width="10%" />
<col width="11%" />
<col width="18%" />
<col width="7%" />
<col width="9%" />
<col width="8%" />
</colgroup>
<thead>
<tr class="header">
<th align="left"></th>
<th align="right">PA_Tweets</th>
<th align="right">All_Tweets</th>
<th align="right">Inactivity_Cases</th>
<th align="right">Inactivity_Percent</th>
<th align="right">Inactivity_Percent_AgeAdjusted</th>
<th align="right">Gini_Index</th>
<th align="right">Gini_Index_MoE</th>
<th align="right">PA_Tweets_Log</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">PA_Tweets</td>
<td align="right">1.0000000</td>
<td align="right">0.9600327</td>
<td align="right">0.8652400</td>
<td align="right">-0.1727724</td>
<td align="right">-0.1496675</td>
<td align="right">0.1476223</td>
<td align="right">-0.1769564</td>
<td align="right">0.3816327</td>
</tr>
<tr class="even">
<td align="left">All_Tweets</td>
<td align="right">0.9600327</td>
<td align="right">1.0000000</td>
<td align="right">0.9018978</td>
<td align="right">-0.1906339</td>
<td align="right">-0.1614875</td>
<td align="right">0.1820915</td>
<td align="right">-0.2192222</td>
<td align="right">0.4418422</td>
</tr>
<tr class="odd">
<td align="left">Inactivity_Cases</td>
<td align="right">0.8652400</td>
<td align="right">0.9018978</td>
<td align="right">1.0000000</td>
<td align="right">-0.1765177</td>
<td align="right">-0.1398289</td>
<td align="right">0.1632351</td>
<td align="right">-0.3116606</td>
<td align="right">0.5424680</td>
</tr>
<tr class="even">
<td align="left">Inactivity_Percent</td>
<td align="right">-0.1727724</td>
<td align="right">-0.1906339</td>
<td align="right">-0.1765177</td>
<td align="right">1.0000000</td>
<td align="right">0.9878729</td>
<td align="right">0.1357708</td>
<td align="right">0.1694772</td>
<td align="right">-0.4573069</td>
</tr>
<tr class="odd">
<td align="left">Inactivity_Percent_AgeAdjusted</td>
<td align="right">-0.1496675</td>
<td align="right">-0.1614875</td>
<td align="right">-0.1398289</td>
<td align="right">0.9878729</td>
<td align="right">1.0000000</td>
<td align="right">0.1583250</td>
<td align="right">0.1249542</td>
<td align="right">-0.4001130</td>
</tr>
<tr class="even">
<td align="left">Gini_Index</td>
<td align="right">0.1476223</td>
<td align="right">0.1820915</td>
<td align="right">0.1632351</td>
<td align="right">0.1357708</td>
<td align="right">0.1583250</td>
<td align="right">1.0000000</td>
<td align="right">0.2439544</td>
<td align="right">0.0995427</td>
</tr>
<tr class="odd">
<td align="left">Gini_Index_MoE</td>
<td align="right">-0.1769564</td>
<td align="right">-0.2192222</td>
<td align="right">-0.3116606</td>
<td align="right">0.1694772</td>
<td align="right">0.1249542</td>
<td align="right">0.2439544</td>
<td align="right">1.0000000</td>
<td align="right">-0.5459746</td>
</tr>
<tr class="even">
<td align="left">PA_Tweets_Log</td>
<td align="right">0.3816327</td>
<td align="right">0.4418422</td>
<td align="right">0.5424680</td>
<td align="right">-0.4573069</td>
<td align="right">-0.4001130</td>
<td align="right">0.0995427</td>
<td align="right">-0.5459746</td>
<td align="right">1.0000000</td>
</tr>
</tbody>
</table>

<table style="width:100%;">
<colgroup>
<col width="19%" />
<col width="6%" />
<col width="7%" />
<col width="10%" />
<col width="11%" />
<col width="19%" />
<col width="7%" />
<col width="9%" />
<col width="8%" />
</colgroup>
<thead>
<tr class="header">
<th align="left"></th>
<th align="right">PA_Tweets</th>
<th align="right">All_Tweets</th>
<th align="right">Inactivity_Cases</th>
<th align="right">Inactivity_Percent</th>
<th align="right">Inactivity_Percent_AgeAdjusted</th>
<th align="right">Gini_Index</th>
<th align="right">Gini_Index_MoE</th>
<th align="right">PA_Tweets_Log</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">PA_Tweets</td>
<td align="right">NA</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
</tr>
<tr class="even">
<td align="left">All_Tweets</td>
<td align="right">0</td>
<td align="right">NA</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
</tr>
<tr class="odd">
<td align="left">Inactivity_Cases</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">NA</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
</tr>
<tr class="even">
<td align="left">Inactivity_Percent</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">NA</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
</tr>
<tr class="odd">
<td align="left">Inactivity_Percent_AgeAdjusted</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">NA</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
</tr>
<tr class="even">
<td align="left">Gini_Index</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">NA</td>
<td align="right">0</td>
<td align="right">0</td>
</tr>
<tr class="odd">
<td align="left">Gini_Index_MoE</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">NA</td>
<td align="right">0</td>
</tr>
<tr class="even">
<td align="left">PA_Tweets_Log</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">NA</td>
</tr>
</tbody>
</table>

Comparing models:

``` r
m1 <- betareg(Inactivity_Percent ~ PA_Tweets, data = cleaned_data)
summary(m1)
```

    ## 
    ## Call:
    ## betareg(formula = Inactivity_Percent ~ PA_Tweets, data = cleaned_data)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.5350 -0.5938  0.0277  0.6751  3.4453 
    ## 
    ## Coefficients (mean model with logit link):
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -1.031e+00  4.889e-03 -210.95   <2e-16 ***
    ## PA_Tweets   -7.643e-05  5.204e-06  -14.69   <2e-16 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)   71.938      1.805   39.84   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  4887 on 3 Df
    ## Pseudo R-squared: 0.03259
    ## Number of iterations: 15 (BFGS) + 2 (Fisher scoring)

``` r
m2 <- update(m1, . ~ . + Gini_Index)
summary(m2)
```

    ## 
    ## Call:
    ## betareg(formula = Inactivity_Percent ~ PA_Tweets + Gini_Index, data = cleaned_data)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9656 -0.5876  0.0485  0.6815  3.5875 
    ## 
    ## Coefficients (mean model with logit link):
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -1.625e+00  5.978e-02 -27.173   <2e-16 ***
    ## PA_Tweets   -8.903e-05  5.309e-06 -16.769   <2e-16 ***
    ## Gini_Index   1.361e+00  1.364e-01   9.978   <2e-16 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)   74.279      1.865   39.84   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  4936 on 4 Df
    ## Pseudo R-squared: 0.0476
    ## Number of iterations: 11 (BFGS) + 7 (Fisher scoring)

``` r
m3 <- update(m2, . ~ . + PA_Tweets:Gini_Index)

m4 <- betareg(Inactivity_Percent ~ PA_Tweets_Log, data = cleaned_data)
summary(m4)
```

    ## 
    ## Call:
    ## betareg(formula = Inactivity_Percent ~ PA_Tweets_Log, data = cleaned_data)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.1461 -0.6891 -0.0016  0.7185  2.9574 
    ## 
    ## Coefficients (mean model with logit link):
    ##                Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)   -0.841770   0.008245 -102.09   <2e-16 ***
    ## PA_Tweets_Log -0.064110   0.002212  -28.98   <2e-16 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)   86.937      2.184   39.81   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  5188 on 3 Df
    ## Pseudo R-squared: 0.2208
    ## Number of iterations: 8 (BFGS) + 2 (Fisher scoring)

``` r
m5 <- update(m4, . ~ . + Gini_Index)
summary(m5)
```

    ## 
    ## Call:
    ## betareg(formula = Inactivity_Percent ~ PA_Tweets_Log + Gini_Index, 
    ##     data = cleaned_data)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.5259 -0.6582  0.0301  0.7102  3.0821 
    ## 
    ## Coefficients (mean model with logit link):
    ##                Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)   -1.413036   0.053645  -26.34   <2e-16 ***
    ## PA_Tweets_Log -0.066174   0.002175  -30.43   <2e-16 ***
    ## Gini_Index     1.319441   0.122310   10.79   <2e-16 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)   90.213      2.267    39.8   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  5246 on 4 Df
    ## Pseudo R-squared: 0.2484
    ## Number of iterations: 12 (BFGS) + 3 (Fisher scoring)

``` r
m6 <- update(m5, . ~ . + PA_Tweets:Gini_Index)
summary(m6)
```

    ## 
    ## Call:
    ## betareg(formula = Inactivity_Percent ~ PA_Tweets_Log + Gini_Index + 
    ##     Gini_Index:PA_Tweets, data = cleaned_data)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.5430 -0.6615  0.0317  0.7092  3.0780 
    ## 
    ## Coefficients (mean model with logit link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)          -1.427e+00  5.436e-02 -26.257   <2e-16 ***
    ## PA_Tweets_Log        -6.480e-02  2.336e-03 -27.743   <2e-16 ***
    ## Gini_Index            1.345e+00  1.233e-01  10.910   <2e-16 ***
    ## Gini_Index:PA_Tweets -1.096e-05  6.711e-06  -1.633    0.102    
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)   90.289      2.269    39.8   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  5247 on 5 Df
    ## Pseudo R-squared: 0.2488
    ## Number of iterations: 12 (BFGS) + 2 (Fisher scoring)

``` r
# AIC(m.beta1, m.beta2, m.beta3)
AIC(m1, m2, m3, m4, m5, m6, k = log(nrow(cleaned_data)))
```

    ##    df        AIC
    ## m1  3  -9750.206
    ## m2  4  -9840.329
    ## m3  5  -9888.875
    ## m4  3 -10351.925
    ## m5  4 -10459.922
    ## m6  5 -10454.439

``` r
trunc_data <- cleaned_data[cleaned_data$All_Tweets > median(cleaned_data$All_Tweets),]
displayCorr(rcorr(as.matrix(trunc_data[vars]), type = "pearson"))
```

<table>
<colgroup>
<col width="18%" />
<col width="7%" />
<col width="7%" />
<col width="10%" />
<col width="11%" />
<col width="18%" />
<col width="7%" />
<col width="9%" />
<col width="8%" />
</colgroup>
<thead>
<tr class="header">
<th align="left"></th>
<th align="right">PA_Tweets</th>
<th align="right">All_Tweets</th>
<th align="right">Inactivity_Cases</th>
<th align="right">Inactivity_Percent</th>
<th align="right">Inactivity_Percent_AgeAdjusted</th>
<th align="right">Gini_Index</th>
<th align="right">Gini_Index_MoE</th>
<th align="right">PA_Tweets_Log</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">PA_Tweets</td>
<td align="right">1.0000000</td>
<td align="right">0.9595165</td>
<td align="right">0.8674039</td>
<td align="right">-0.1763574</td>
<td align="right">-0.1566089</td>
<td align="right">0.2087093</td>
<td align="right">-0.2286187</td>
<td align="right">0.4723191</td>
</tr>
<tr class="even">
<td align="left">All_Tweets</td>
<td align="right">0.9595165</td>
<td align="right">1.0000000</td>
<td align="right">0.9007430</td>
<td align="right">-0.1880533</td>
<td align="right">-0.1627543</td>
<td align="right">0.2588863</td>
<td align="right">-0.2840633</td>
<td align="right">0.5376199</td>
</tr>
<tr class="odd">
<td align="left">Inactivity_Cases</td>
<td align="right">0.8674039</td>
<td align="right">0.9007430</td>
<td align="right">1.0000000</td>
<td align="right">-0.1434239</td>
<td align="right">-0.1157220</td>
<td align="right">0.2234304</td>
<td align="right">-0.3795684</td>
<td align="right">0.6050935</td>
</tr>
<tr class="even">
<td align="left">Inactivity_Percent</td>
<td align="right">-0.1763574</td>
<td align="right">-0.1880533</td>
<td align="right">-0.1434239</td>
<td align="right">1.0000000</td>
<td align="right">0.9907621</td>
<td align="right">0.0495294</td>
<td align="right">0.1647755</td>
<td align="right">-0.4976922</td>
</tr>
<tr class="odd">
<td align="left">Inactivity_Percent_AgeAdjusted</td>
<td align="right">-0.1566089</td>
<td align="right">-0.1627543</td>
<td align="right">-0.1157220</td>
<td align="right">0.9907621</td>
<td align="right">1.0000000</td>
<td align="right">0.0698201</td>
<td align="right">0.1361131</td>
<td align="right">-0.4628461</td>
</tr>
<tr class="even">
<td align="left">Gini_Index</td>
<td align="right">0.2087093</td>
<td align="right">0.2588863</td>
<td align="right">0.2234304</td>
<td align="right">0.0495294</td>
<td align="right">0.0698201</td>
<td align="right">1.0000000</td>
<td align="right">0.1421941</td>
<td align="right">0.2185893</td>
</tr>
<tr class="odd">
<td align="left">Gini_Index_MoE</td>
<td align="right">-0.2286187</td>
<td align="right">-0.2840633</td>
<td align="right">-0.3795684</td>
<td align="right">0.1647755</td>
<td align="right">0.1361131</td>
<td align="right">0.1421941</td>
<td align="right">1.0000000</td>
<td align="right">-0.5130924</td>
</tr>
<tr class="even">
<td align="left">PA_Tweets_Log</td>
<td align="right">0.4723191</td>
<td align="right">0.5376199</td>
<td align="right">0.6050935</td>
<td align="right">-0.4976922</td>
<td align="right">-0.4628461</td>
<td align="right">0.2185893</td>
<td align="right">-0.5130924</td>
<td align="right">1.0000000</td>
</tr>
</tbody>
</table>

<table style="width:100%;">
<colgroup>
<col width="19%" />
<col width="6%" />
<col width="7%" />
<col width="10%" />
<col width="11%" />
<col width="19%" />
<col width="7%" />
<col width="9%" />
<col width="8%" />
</colgroup>
<thead>
<tr class="header">
<th align="left"></th>
<th align="right">PA_Tweets</th>
<th align="right">All_Tweets</th>
<th align="right">Inactivity_Cases</th>
<th align="right">Inactivity_Percent</th>
<th align="right">Inactivity_Percent_AgeAdjusted</th>
<th align="right">Gini_Index</th>
<th align="right">Gini_Index_MoE</th>
<th align="right">PA_Tweets_Log</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">PA_Tweets</td>
<td align="right">NA</td>
<td align="right">0</td>
<td align="right">0.0e+00</td>
<td align="right">0.0000000</td>
<td align="right">0.0000000</td>
<td align="right">0.0000000</td>
<td align="right">0e+00</td>
<td align="right">0</td>
</tr>
<tr class="even">
<td align="left">All_Tweets</td>
<td align="right">0</td>
<td align="right">NA</td>
<td align="right">0.0e+00</td>
<td align="right">0.0000000</td>
<td align="right">0.0000000</td>
<td align="right">0.0000000</td>
<td align="right">0e+00</td>
<td align="right">0</td>
</tr>
<tr class="odd">
<td align="left">Inactivity_Cases</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">NA</td>
<td align="right">0.0000000</td>
<td align="right">0.0000043</td>
<td align="right">0.0000000</td>
<td align="right">0e+00</td>
<td align="right">0</td>
</tr>
<tr class="even">
<td align="left">Inactivity_Percent</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0.0e+00</td>
<td align="right">NA</td>
<td align="right">0.0000000</td>
<td align="right">0.0497444</td>
<td align="right">0e+00</td>
<td align="right">0</td>
</tr>
<tr class="odd">
<td align="left">Inactivity_Percent_AgeAdjusted</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">4.3e-06</td>
<td align="right">0.0000000</td>
<td align="right">NA</td>
<td align="right">0.0056458</td>
<td align="right">1e-07</td>
<td align="right">0</td>
</tr>
<tr class="even">
<td align="left">Gini_Index</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0.0e+00</td>
<td align="right">0.0497444</td>
<td align="right">0.0056458</td>
<td align="right">NA</td>
<td align="right">0e+00</td>
<td align="right">0</td>
</tr>
<tr class="odd">
<td align="left">Gini_Index_MoE</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0.0e+00</td>
<td align="right">0.0000000</td>
<td align="right">0.0000001</td>
<td align="right">0.0000000</td>
<td align="right">NA</td>
<td align="right">0</td>
</tr>
<tr class="even">
<td align="left">PA_Tweets_Log</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0.0e+00</td>
<td align="right">0.0000000</td>
<td align="right">0.0000000</td>
<td align="right">0.0000000</td>
<td align="right">0e+00</td>
<td align="right">NA</td>
</tr>
</tbody>
</table>

``` r
par(mfrow=c(2, 1))
scatter.smooth(cleaned_data$PA_Tweets, cleaned_data$Inactivity_Percent)
scatter.smooth(cleaned_data$PA_Tweets_Log, cleaned_data$Inactivity_Percent)
```

![](TwitterPhysicalActivity_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
predict(m4, data.frame(PA_Tweets_Log = log(23)))
```

    ##         1 
    ## 0.2606127
