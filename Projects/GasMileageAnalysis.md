Project 1

### Summary

Using the mtcars data set in R, a data set of a collection of cars, this
analysis explores the relationship between a set of variables and miles
per gallon (MPG) (outcome). We seek to determine whether automatic or
manual is better for MPG, and to quantify the differences in MPG between
the two.  
\*\*\*

### Part 1: Is an automatic or manual transmission better for MPG?

First, let’s examine the data and relevel the factor variables.

``` r
data(mtcars)

mtcars$cyl<-relevel(as.factor(mtcars$cyl), ref='8')
mtcars$vs<-relevel(as.factor(mtcars$vs), ref='0')
mtcars$am<-relevel(as.factor(mtcars$am), ref='0',labels=c('Automatic','Manual'))
mtcars$gear<-relevel(as.factor(mtcars$gear), ref='3')
mtcars$carb<-relevel(as.factor(mtcars$carb), ref='2')
```

Now, model mpg and transmission using a 2-variable linear model.

``` r
lm1 <- lm(mpg~am,data=mtcars)
summary(lm1)$coef
```

    ##              Estimate Std. Error   t value     Pr(>|t|)
    ## (Intercept) 17.147368   1.124603 15.247492 1.133983e-15
    ## am1          7.244939   1.764422  4.106127 2.850207e-04

``` r
summary(lm1)$adj.r.squared
```

    ## [1] 0.3384589

From the data, we know ‘am’, aka transmission, is a binary variable with
automatic equal to 0 and manual equal to 1. Based on the output, we can
see that the average mpg for automatic is 17.147, whereas for manual it
is 24.392

Now, let’s fit a regression model including all variables.

``` r
lm2 <- lm(mpg~.,mtcars)
summary(lm2)$adj.r.squared
```

    ## [1] 0.7790215

From the output, we can see the adjusted R-squared indicates that some
variables are included that are not important. In addition, all
variables in this model are insignificant.

The conclusion above is also shown by VIF. Now we’ll use the
`step`function to determine which variables are significant.

``` r
lm3$coefficients
```

    ## (Intercept)        cyl4        cyl6          hp          wt         am1 
    ## 31.54464858  2.16367532 -0.86766917 -0.03210943 -2.49682942  1.80921138

``` r
summary(lm3)$adj.r.squared
```

    ## [1] 0.8400875

The output has demonstrated that `cyl4`,`cyl6`,`hp`,`wt`, and `am1` are
significant variables. In addition, this model has a higher adjusted
R-squared than the previous.

``` r
anova(lm1,lm2,lm3)
```

The `anova` output verifies that the 3rd model, `lm3`, is the most
adequate.

After performing diagnostic plots of the residuals, **Figure 2** shows
that the residuals appear to have mean of 0, but not constant
variance.  
**Figure 3** indicates that the residuals do not completely follow
normal distribution. Therefore, the model is not perfect.

However, from the different models tested, the data indicates that
manual transmission may be better for MPG rather than automatic.

------------------------------------------------------------------------

### Part 2: Quantify the MPG difference between automatic and manual transmissions

First, we look back at the `lm1` model to see the slope of the `am`
variable, which quantifies the difference between automatic and manual
transmissions.

``` r
summary(lm1)$coef[2]
```

    ## [1] 7.244939

We can see that without any other variables, manual transmission cars
get about 7.245 more miles per gallon than automatic transmission cars.

``` r
summary(lm3)$coef[6]
```

    ## [1] 1.809211

With all other factors held constant, manual transmission cars get about
1.809 more miles per gallon than automatic transmission cars.  
\*\*\*

Appendix
--------

``` r
boxplot(mpg~am, data=mtcars, main ='Figure 1 \nFuel Efficiency',ylab='MPG',
        xlab="transmission",names=c("Automatic","Manual"),notch=FALSE, col=(c("blue","red")))
```

<img src="GasMileageAnalysis_files/figure-markdown_github/fig1-1.png" style="display: block; margin: auto;" />

``` r
plot(lm3,which=1,main="Figure 2")
```

<img src="GasMileageAnalysis_files/figure-markdown_github/fig2-1.png" style="display: block; margin: auto;" />

``` r
plot(lm3,which=2,main="Figure 3")
```

![](GasMileageAnalysis_files/figure-markdown_github/unnamed-chunk-10-1.png)
