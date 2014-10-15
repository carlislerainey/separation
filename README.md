# README

This is the beginnings of an R packages that implements the ideas in my
paper on dealing with separation in logit models. You can find pdf of the working draft of that paper [here](http://www.carlislerainey.com/papers/separation.pdf) and the GitHub repository for the paper [here](https://github.com/carlislerainey/priors-for-separation).

`separation` requires the latest version of `compactr`, and both of these packages are available on GitHub.


```r
devtools::install_github("carlislerainey/compactr")
devtools::install_github("carlislerainey/separation")
```

The idea is simple. In order to draw meaningful inferences when facing separation, you must choose an informative prior. This can be tricky with logistic regression models, because the coefficients are not on a scale that many researchers are comfortable with. To facilitate choose a reasonable, informative prior, researchers might focus on the partial prior predictive distribution, explained in the paper linked to above. The partial prior distribution allows researchers to focus on the usual quantities of interest, such as predicted probabilities, first-differences, and risk-ratios. Researchers can use this package to what prior distributions for the coefficients imply about the partial prior preditive distribution, which is easier to make sense of.

Let's look at a simple example. Suppose we estimate a simple model explaining governors' decisions to oppose the Medicaid expansion under the Affordable Care Act. In particular, we'd like to draw a conclusion about whether a governor's partisanship matters more than the level of need in her state. We might estimate a simple model using the data from [Barrilleaux and Rainey](http://www.carlislerainey.com/papers/need.pdf), which is included in the package `separation` installed above.


```r
data(politics_and_need)
m <- glm(oppose_expansion ~ gop_governor + percent_uninsured, 
         family = binomial, data = politics_and_need)

library(texreg)  # for screenreg, which neatly prints the results
```

```
## Version:  1.33
## Date:     2014-07-21
## Author:   Philip Leifeld (University of Konstanz)
## 
## Please cite the JSS article in your publications -- see citation("texreg").
```

```r
screenreg(m)
```

```
## 
## ============================
##                    Model 1  
## ----------------------------
## (Intercept)          -20.92 
##                    (2370.03)
## gop_governor          19.46 
##                    (2370.03)
## percent_uninsured      0.10 
##                       (0.10)
## ----------------------------
## AIC                   46.33 
## BIC                   52.07 
## Log Likelihood       -20.17 
## Deviance              40.33 
## Num. obs.             50    
## ============================
## *** p < 0.001, ** p < 0.01, * p < 0.05
```

You'll notice that we have separation, because no Democratic governors opposed the expansion, the variable `gop_governor` perfectly predicts zeros. We can see this in a table.


```r
xtabs(~ oppose_expansion + gop_governor, data = politics_and_need)
```

```
##                 gop_governor
## oppose_expansion  0  1
##                0 20 14
##                1  0 16
```

This separation leads to implausibly large estimates for the coefficient for `gop_governor` and, because the perfect prediction occurs when `gop_governor` equals one, the intercept as well. Notice, perhaps most importantly, that the standard errors are unusable as well.

To get a handle on this, we need to provide some form of prior information to stablize the coefficient for `gop_governor`, which will in turn provide a reasonable estimate for the intercept. But what prior should we choose for `gop_governor`? A N(0, 2)? and N(0, 4)? What? To help make this choice, I introduce the concept of a partial prior predictive distribution that allows researchers to focus narrowly on a single region of the prior distribute--the only part that really matters. See [the paper](http://www.carlislerainey.com/papers/separation.pdf) for the details. 

We have three quantities of interest that we might choose to focus on.

1. predicted probability: The probability that a Democratic governor opposes the expansion.
2. first-difference: The change in the probability of expansion as a governor's partisanship changes from Democrat to Republican.
3. risk-ratio: How many times more likely is a Republican governor to oppose the expansion compared to a Democratic governor.

In this case, it seems reasonable to focus on the probability that a Democratic governor opposes the expansion. That also happens to be the default quantity for the functions.

Let's choose a N(0, 3) as a first guess at an "informative" prior distribution. (I know this is a reasonable choice because I experimented a little before writing this.) We first need to simulate from our chosen prior and then we can convert those simulation into simulations of predicted probabilities using the function `calc_pppd()`.


```r
# load package
library(separation)

# load data
data(politics_and_need)

# simulate from potential prior
normal_3 <- rnorm(10000, sd = 3)

# model formula
f <- oppose_expansion ~ gop_governor + percent_favorable_aca + percent_uninsured

# informative prior
pppd_inf <- calc_pppd(f, data = politics_and_need, prior_sims = normal_3,     
                   sep_var_name = "gop_governor", prior_label = "Normal(0, 3)")
```

We can summarize the percentiles of the distrubiotn with the generic `print()` function.


```r
print(pppd_inf)
```

```
## 
## Model:	oppose_expansion ~ gop_governor + percent_favorable_aca + percent_uninsured
## Prior for gop_governor:	Normal(0, 3)
## 
##    percentile	predicted probability	first-difference	risk-ratio	
##    1%		0			0.009			1.022			
##    5%		0.002			0.046			1.12			
##    25%		0.024			0.21			1.962			
##    50%		0.089			0.34			4.83			
##    75%		0.218			0.404			17.61			
##    95%		0.383			0.426			201.2			
##    99%		0.419			0.428			1551			
```

That looks reasonable. There is an even chance of the probabilty of opposition falling above and below 0.09, which seems about right. Also, there is a 25% chance that the probability falls below 0.02, which also seems like a reasonable prior belief.

To get an even better feel for this PPPD, we can use the generic `plot()` function to plot the distribution of the predicted probability.


```r
plot(pppd_inf)
```

![plot of chunk inf](inf.png) 

If we care about the distribution for the tiny probabilities, we can plot the distribution on the log scale. 


```r
plot(pppd_inf, log_scale = TRUE)
```

![plot of chunk inf-log](inf-log.png) 

Again, the purpose of the prior distribution in dealing with separation is to rule out implausible large effects. Plotting on the log scale allows us to see exactly what predicted probabilities we are ruling out. We are essentially ruling out probabilities smaller than 0.001, which seems reasonable. 

If we want, we can calculate the probability of falling below certain thresholds.


```r
mean(pppd_inf$pr < 0.01)
```

```
## [1] 0.1441
```

```r
mean(pppd_inf$pr < 0.001)
```

```
## [1] 0.02811
```

From this we can see that there is a probability of 0.15 that less than 1% of Democratic governors oppose the expansion and a probability of 0.025 that less than 0.1% of Democratic governors oppose the expansion. Again, these seem reasonable.

We might also like to compare our informative prior to a "skeptical" and "enthusiastic" prior distribution. For simplicity, let's go with half and double the standard deviation of the informative prior, respectively. We can calculate the PPPD for these distributions as before and use the `combine_pppd()` function to put them together and then the generic `plot()` function for plotting.


```r
# simulate from potential skeptical and enthusiastic priors
normal_1 <- rnorm(10000, sd = 1.5)
normal_4 <- rnorm(10000, sd = 9)

# enthusiastic and skeptical prior
pppd_enth <- calc_pppd(f, data = politics_and_need, prior_sims = normal_4, 
                   sep_var_name = "gop_governor", prior_label = "Normal(0, 9)")
pppd_skep <- calc_pppd(f, data = politics_and_need, prior_sims = normal_1, 
                   sep_var_name = "gop_governor", prior_label = "Normal(0, 1.5)")
pppds <- combine_pppd(pppd_skep, pppd_inf, pppd_enth)

# plot all pppds for comparisons
plot(pppds)
```

![plot of chunk eth_skep](eth_skep1.png) 

```r
plot(pppds, log_scale = TRUE)
```

![plot of chunk eth_skep](eth_skep2.png) 
