479HW2
================
February 25, 2020

## Deal with raw data

``` r
library(blscrapeR)
df <- get_bls_county()
df1 <- get_bls_county('Nov 2019')
colnames(df1)[8:9]=c("unemployed_nov","unemployed_rate_nov" )

library(tidyr)
library(dplyr)
WIunemployment = df %>% 
  filter(fips_state == 55)

df1=filter(df1,fips_state == 55)

library(readr)
library(ggplot2)
bridges = read_csv("https://www.fhwa.dot.gov/bridge/nbi/2018/delimited/WI18.txt")
```

## Model 1

``` r
data=bridges %>%
  group_by(COUNTY_CODE_003) %>% 
  summarise(count = n(),
            APPR_WIDTH=mean(APPR_WIDTH_MT_032),
            horizonclearance=mean(HORR_CLR_MT_047),
            spanlen=mean(MAX_SPAN_LEN_MT_048),
            struclen=mean(STRUCTURE_LEN_MT_049),
            rdwid=mean(ROADWAY_WIDTH_MT_051),
            oprating=mean(OPERATING_RATING_064),
            invraing=mean(INVENTORY_RATING_066),
            sfrating=mean(SUFFICIENCY_RATING)
            ) %>% 
  left_join(WIunemployment,by=c("COUNTY_CODE_003" = "fips_county")) %>%
  select(count:sfrating,unemployed)


m=lm(log(unemployed)~.,data = data)
m=lm(unemployed~.,data = data)
summary(m)
```

    ## 
    ## Call:
    ## lm(formula = unemployed ~ ., data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2473.8  -520.5   -81.2   516.9  5070.0 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      -6679.613   4563.318  -1.464   0.1488    
    ## count                7.671      1.172   6.546 1.81e-08 ***
    ## APPR_WIDTH        -396.461   1168.852  -0.339   0.7357    
    ## horizonclearance   981.545   1171.155   0.838   0.4055    
    ## spanlen           -206.858    122.565  -1.688   0.0969 .  
    ## struclen            36.720     35.473   1.035   0.3050    
    ## rdwid              290.078    295.673   0.981   0.3307    
    ## oprating           -65.785    108.753  -0.605   0.5476    
    ## invraing           272.985    190.610   1.432   0.1576    
    ## sfrating           -59.329     56.034  -1.059   0.2942    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1080 on 57 degrees of freedom
    ##   (5 observations deleted due to missingness)
    ## Multiple R-squared:  0.8019, Adjusted R-squared:  0.7706 
    ## F-statistic: 25.63 on 9 and 57 DF,  p-value: < 2.2e-16

## Model 2

``` r
data2=bridges %>%
  group_by(COUNTY_CODE_003) %>% 
  summarise(count = n(),
            APPR_WIDTH=mean(APPR_WIDTH_MT_032),
            horizonclearance=mean(HORR_CLR_MT_047),
            spanlen=mean(MAX_SPAN_LEN_MT_048),
            struclen=mean(STRUCTURE_LEN_MT_049),
            rdwid=mean(ROADWAY_WIDTH_MT_051),
            oprating=mean(OPERATING_RATING_064),
            invraing=mean(INVENTORY_RATING_066),
            sfrating=mean(SUFFICIENCY_RATING)
  ) %>% 
  left_join(WIunemployment,by=c("COUNTY_CODE_003" = "fips_county")) %>%
  select(count:sfrating,unemployed_rate)

m1=lm(unemployed_rate~.,data = data2)
summary(m1)
```

    ## 
    ## Call:
    ## lm(formula = unemployed_rate ~ ., data = data2)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.3727 -0.4064 -0.1089  0.3220  1.6516 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       4.5753236  3.0509317   1.500   0.1392    
    ## count            -0.0020454  0.0007834  -2.611   0.0115 *  
    ## APPR_WIDTH       -0.6513770  0.7814683  -0.834   0.4080    
    ## horizonclearance  0.0895342  0.7830080   0.114   0.9094    
    ## spanlen          -0.0042595  0.0819439  -0.052   0.9587    
    ## struclen         -0.0079703  0.0237167  -0.336   0.7381    
    ## rdwid             0.3580604  0.1976805   1.811   0.0754 .  
    ## oprating         -0.1872438  0.0727099  -2.575   0.0126 *  
    ## invraing          0.6260942  0.1274376   4.913 7.93e-06 ***
    ## sfrating         -0.0842670  0.0374628  -2.249   0.0284 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.7221 on 57 degrees of freedom
    ##   (5 observations deleted due to missingness)
    ## Multiple R-squared:  0.6223, Adjusted R-squared:  0.5626 
    ## F-statistic: 10.43 on 9 and 57 DF,  p-value: 2.518e-09

## Model 3

``` r
data3=bridges %>%
  group_by(COUNTY_CODE_003) %>% 
  summarise(count = n(),
            APPR_WIDTH=mean(APPR_WIDTH_MT_032),
            horizonclearance=mean(HORR_CLR_MT_047),
            spanlen=mean(MAX_SPAN_LEN_MT_048),
            struclen=mean(STRUCTURE_LEN_MT_049),
            rdwid=mean(ROADWAY_WIDTH_MT_051),
            oprating=mean(OPERATING_RATING_064),
            invraing=mean(INVENTORY_RATING_066),
            sfrating=mean(SUFFICIENCY_RATING)
  ) %>% 
  left_join(WIunemployment,by=c("COUNTY_CODE_003" = "fips_county")) %>%
  left_join(df1,by=c("COUNTY_CODE_003" = "fips_county")) %>%
  select(count:sfrating,unemployed,unemployed_nov)

m2=lm(unemployed~.,data = data3)
summary(m2)
```

    ## 
    ## Call:
    ## lm(formula = unemployed ~ ., data = data3)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -150.45  -55.41  -15.11   32.76  280.93 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      143.00676  403.11768   0.355 0.724108    
    ## count              0.48267    0.13100   3.684 0.000518 ***
    ## APPR_WIDTH        21.36282  101.38804   0.211 0.833883    
    ## horizonclearance  -4.02195  102.10635  -0.039 0.968720    
    ## spanlen           -7.39150   10.86516  -0.680 0.499122    
    ## struclen           3.34729    3.09749   1.081 0.284487    
    ## rdwid              7.90021   25.82362   0.306 0.760793    
    ## oprating           2.81751    9.45585   0.298 0.766833    
    ## invraing           3.41803   16.80454   0.203 0.839560    
    ## sfrating          -6.12882    4.89348  -1.252 0.215613    
    ## unemployed_nov     0.91924    0.01059  86.815  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 93.58 on 56 degrees of freedom
    ##   (5 observations deleted due to missingness)
    ## Multiple R-squared:  0.9985, Adjusted R-squared:  0.9983 
    ## F-statistic:  3827 on 10 and 56 DF,  p-value: < 2.2e-16

## Model 4

``` r
data4=bridges %>%
  group_by(COUNTY_CODE_003) %>% 
  summarise(count = n(),
            APPR_WIDTH=mean(APPR_WIDTH_MT_032),
            horizonclearance=mean(HORR_CLR_MT_047),
            spanlen=mean(MAX_SPAN_LEN_MT_048),
            struclen=mean(STRUCTURE_LEN_MT_049),
            rdwid=mean(ROADWAY_WIDTH_MT_051),
            oprating=mean(OPERATING_RATING_064),
            invraing=mean(INVENTORY_RATING_066),
            sfrating=mean(SUFFICIENCY_RATING)
  ) %>% 
  left_join(WIunemployment,by=c("COUNTY_CODE_003" = "fips_county")) %>%
  left_join(df1,by=c("COUNTY_CODE_003" = "fips_county")) %>%
  select(count:sfrating,unemployed_rate,unemployed_rate_nov)

m3=lm(unemployed_rate~.,data = data4)
summary(m3)
```

    ## 
    ## Call:
    ## lm(formula = unemployed_rate ~ ., data = data4)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.81762 -0.23241 -0.03352  0.24386  1.06975 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          2.9657430  1.9232053   1.542   0.1287    
    ## count               -0.0003410  0.0005241  -0.651   0.5179    
    ## APPR_WIDTH           0.2901510  0.5007534   0.579   0.5646    
    ## horizonclearance    -0.3883773  0.4942420  -0.786   0.4353    
    ## spanlen             -0.0125830  0.0514577  -0.245   0.8077    
    ## struclen             0.0068631  0.0149741   0.458   0.6485    
    ## rdwid               -0.1415306  0.1349910  -1.048   0.2989    
    ## oprating            -0.0260127  0.0487604  -0.533   0.5958    
    ## invraing             0.1969676  0.0920921   2.139   0.0368 *  
    ## sfrating            -0.0548528  0.0237284  -2.312   0.0245 *  
    ## unemployed_rate_nov  0.9316174  0.0989799   9.412 3.92e-13 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4534 on 56 degrees of freedom
    ##   (5 observations deleted due to missingness)
    ## Multiple R-squared:  0.8537, Adjusted R-squared:  0.8276 
    ## F-statistic: 32.68 on 10 and 56 DF,  p-value: < 2.2e-16

## Some insights

The R-squared and R-squared\_adj when modeling the \#unemployed is
larger than that when modeling the unemployed rate. I think that places
with better and a larger number of bridges should have more population
and consequently have more unemployed people. However, the unemployed
rate doesn’t have much to do with them but other variables, which are
shown in the model 2.

After adding the previous \#unemployed or unemployed rate data into the
models, we found that the R-squared and R-squared\_adj increased
obviously. Since the bridges change very slowly, the \#unemployed and
unemployed rate may be more correlated with previous data.
