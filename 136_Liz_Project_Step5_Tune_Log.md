136\_Liz\_Project\_Step5\_Log Transformation
================
Hyunkyung Kim
November 6, 2018

``` r
#install.packages("glmnet")
#install.packages("mlbench")
#install.packages("Boruta")
library(caret)
```

    ## Loading required package: lattice

    ## Loading required package: ggplot2

``` r
library(tidyverse)
```

    ## -- Attaching packages ---------------------------------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v tibble  1.4.2     v purrr   0.2.5
    ## v tidyr   0.8.1     v dplyr   0.7.7
    ## v readr   1.1.1     v stringr 1.3.1
    ## v tibble  1.4.2     v forcats 0.3.0

    ## -- Conflicts ------------------------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()
    ## x purrr::lift()   masks caret::lift()

``` r
library(psych)
```

    ## 
    ## Attaching package: 'psych'

    ## The following objects are masked from 'package:ggplot2':
    ## 
    ##     %+%, alpha

``` r
library(glmnet)
```

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'Matrix'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     expand

    ## Loading required package: foreach

    ## 
    ## Attaching package: 'foreach'

    ## The following objects are masked from 'package:purrr':
    ## 
    ##     accumulate, when

    ## Loaded glmnet 2.0-16

``` r
library(mlbench)
library(Boruta)
```

    ## Loading required package: ranger

``` r
library(MASS) # stepwise regression
```

    ## 
    ## Attaching package: 'MASS'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

``` r
library(leaps) # all subsets regression
library(randomForest)
```

    ## randomForest 4.6-14

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:ranger':
    ## 
    ##     importance

    ## The following object is masked from 'package:psych':
    ## 
    ##     outlier

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

``` r
library(caret)
library(tidyverse)
library(psych)
library(glmnet)
library(mlbench)
library(Boruta)
library(MASS) # stepwise regression
library(leaps) # all subsets regression
library(randomForest)
library(MLmetrics)
```

    ## 
    ## Attaching package: 'MLmetrics'

    ## The following object is masked from 'package:psych':
    ## 
    ##     AUC

    ## The following objects are masked from 'package:caret':
    ## 
    ##     MAE, RMSE

    ## The following object is masked from 'package:base':
    ## 
    ##     Recall

``` r
library(e1071)
library(car) # VIF 
```

    ## Loading required package: carData

    ## 
    ## Attaching package: 'car'

    ## The following object is masked from 'package:psych':
    ## 
    ##     logit

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     recode

    ## The following object is masked from 'package:purrr':
    ## 
    ##     some

### Import Clean Data

``` r
H_Clean<-read.csv( file = "C:\\Users\\Hyunkyung Kim\\Desktop\\CKME999\\136\\dataset\\all\\H_clean.csv")
H_Clean$MSSubClass<-as.factor(H_Clean$MSSubClass)
Train<-H_Clean[!is.na(H_Clean$SalePrice),]
Test<-H_Clean[is.na(H_Clean$SalePrice),]
actual<-read.csv( file = "C:\\Users\\Hyunkyung Kim\\Desktop\\CKME999\\136\\dataset\\all\\AMES_test.csv")[1461:2919,2] # Test Price
```

Utilities has extremely low variance where only 1 out 2919 observation is different. This is not really useful and I will drop this.

``` r
H_Eng<-H_Clean[,names(H_Clean)!="Utilities"]
```

Add Full bath and half baths into one BathAbvGrd and BsmtFullBath+BsmtHalfBath into BsmtBath

``` r
H_Eng$BathAbvGrd<-H_Eng$FullBath+0.5*H_Eng$HalfBath
H_Eng$BsmtBath<-H_Eng$BsmtFullBath+0.5*H_Eng$BsmtHalfBath

plot( H_Eng$BathAbvGrd, H_Eng$SalePrice)
abline(lm(H_Eng$SalePrice~H_Eng$BathAbvGrd, data=H_Eng), col='red')
```

![](136_Liz_Project_Step5_Tune_Log_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
plot(H_Eng$BsmtBath, H_Eng$SalePrice)
abline(lm(H_Eng$SalePrice~H_Eng$BsmtBath, data=H_Eng), col='red')
```

![](136_Liz_Project_Step5_Tune_Log_files/figure-markdown_github/unnamed-chunk-4-2.png)

``` r
# Remove original columns
#H_Eng<-H_Eng[,!names(H_Clean) %in% c("FullBath",'HalfBath','BathAbvGrd','BsmtBath')]
H_Eng<-subset(H_Eng, select=-c(FullBath,HalfBath,BathAbvGrd,BsmtBath))
```

Log Transformation due to Skewness
----------------------------------

As Discussed in Step1, we have a few variables including response variable that thas high skewness.

``` r
H_num<-H_Clean[,sapply(H_Clean,is.numeric)] #Numerical
H_num<-H_num[-c(1)]
H_cat<-H_Clean[,sapply(H_Clean[,-1],is.factor)] # Categorical
Check<-describe(H_num)[,c(11,12)]
Check[Check$skew>1 ,]
```

    ##                skew kurtosis
    ## LotFrontage    1.38     9.31
    ## LotArea       12.82   264.31
    ## MasVnrArea     2.61     9.31
    ## ExterCond      1.32     6.27
    ## BsmtExposure   1.12    -0.08
    ## BsmtFinSF1     1.42     6.88
    ## BsmtFinType2   3.15    10.12
    ## BsmtFinSF2     4.14    18.80
    ## TotalBsmtSF    1.16     9.10
    ## X1stFlrSF      1.47     6.94
    ## LowQualFinSF  12.08   174.51
    ## GrLivArea      1.27     4.11
    ## BsmtHalfBath   3.93    14.82
    ## KitchenAbvGr   4.30    19.73
    ## WoodDeckSF     1.84     6.72
    ## OpenPorchSF    2.53    10.91
    ## EnclosedPorch  4.00    28.31
    ## X3SsnPorch    11.37   149.05
    ## ScreenPorch    3.94    17.73
    ## PoolArea      16.89   297.91
    ## PoolQC        15.86   255.66
    ## MiscVal       21.94   562.72
    ## SalePrice      1.88     6.50

Below items all high skewness and may need transformation. - Removed ordinal items.

-            skew kurtosis

LotFrontage 1.33 9.12 LotArea 12.82 264.31 MasVnrArea 2.61 9.31 ExterCond 1.32 6.27 TotalBsmtSF 1.16 9.10 X1stFlrSF 1.47 6.94 LowQualFinSF 12.08 174.51 GrLivArea 1.27 4.11 BsmtHalfBath 3.93 14.82 KitchenAbvGr 4.30 19.73 PavedDrive -2.98 7.10 WoodDeckSF 1.84 6.72 OpenPorchSF 2.53 10.91 EnclosedPorch 4.00 28.31 X3SsnPorch 11.37 149.05 ScreenPorch 3.94 17.73 PoolArea 16.89 297.91 PoolQC 15.86 255.66 MiscVal 21.94 562.72 SalePrice 1.88 6.50

#### Subset non-normal variables.

``` r
H_SkewedVar<-H_Eng[,c(
  
'LotFrontage',
'LotArea',
'MasVnrArea',
'TotalBsmtSF',
'X1stFlrSF',
'LowQualFinSF',
'GrLivArea',
'BsmtHalfBath',
'KitchenAbvGr',
'OpenPorchSF',
'EnclosedPorch',
'X3SsnPorch',
'ScreenPorch',
'PoolArea',
'MiscVal',
'SalePrice')]

H_Lo<-H_Eng
```

Apply Log only to the skewed items

``` r
H_Lo[,c(
  
'LotFrontage',
'LotArea',
'MasVnrArea',
'BsmtFinSF1',
'BsmtFinSF2',
'TotalBsmtSF',
'X1stFlrSF',
'LowQualFinSF',
'GrLivArea',
'BsmtHalfBath',
'KitchenAbvGr',
'OpenPorchSF',
'EnclosedPorch',
'X3SsnPorch',
'ScreenPorch',
'PoolArea',
'MiscVal',
'SalePrice')]<-log(1+H_Lo[,c(
  
'LotFrontage',
'LotArea',
'MasVnrArea',
'BsmtFinSF1',
'BsmtFinSF2',
'TotalBsmtSF',
'X1stFlrSF',
'LowQualFinSF',
'GrLivArea',
'BsmtHalfBath',
'KitchenAbvGr',
'OpenPorchSF',
'EnclosedPorch',
'X3SsnPorch',
'ScreenPorch',
'PoolArea',
'MiscVal',
'SalePrice')])
```

``` r
head(H_Lo)
```

    ##   Id MSSubClass MSZoning LotFrontage  LotArea Street   Alley LotShape
    ## 1  1      2SNEW       RL    4.189655 9.042040   Pave NoAlley      Reg
    ## 2  2      1SNEW       RL    4.394449 9.169623   Pave NoAlley      Reg
    ## 3  3      2SNEW       RL    4.234107 9.328212   Pave NoAlley      IR1
    ## 4  4      2SOLD       RL    4.110874 9.164401   Pave NoAlley      IR1
    ## 5  5      2SNEW       RL    4.442651 9.565284   Pave NoAlley      IR1
    ## 6  6     1NHFin       RL    4.454347 9.555064   Pave NoAlley      IR1
    ##   LandContour LotConfig LandSlope Neighborhood Condition1 Condition2
    ## 1         Lvl    Inside         3      CollgCr       Norm       Norm
    ## 2         Lvl       FR2         3      Veenker      Feedr       Norm
    ## 3         Lvl    Inside         3      CollgCr       Norm       Norm
    ## 4         Lvl    Corner         3      Crawfor       Norm       Norm
    ## 5         Lvl       FR2         3      NoRidge       Norm       Norm
    ## 6         Lvl    Inside         3      Mitchel       Norm       Norm
    ##   BldgType HouseStyle OverallQual OverallCond YearBuilt YearRemodAdd
    ## 1        5     2Story           7           5      2003         2003
    ## 2        5     1Story           6           8      1976         1976
    ## 3        5     2Story           7           5      2001         2002
    ## 4        5     2Story           7           5      1915         1970
    ## 5        5     2Story           8           5      2000         2000
    ## 6        5     1.5Fin           5           5      1993         1995
    ##   RoofStyle RoofMatl Exterior1st Exterior2nd MasVnrType MasVnrArea
    ## 1     Gable  CompShg     VinylSd     VinylSd    BrkFace   5.283204
    ## 2     Gable  CompShg     MetalSd     MetalSd       None   0.000000
    ## 3     Gable  CompShg     VinylSd     VinylSd    BrkFace   5.093750
    ## 4     Gable  CompShg     Wd Sdng     Wd Shng       None   0.000000
    ## 5     Gable  CompShg     VinylSd     VinylSd    BrkFace   5.860786
    ## 6     Gable  CompShg     VinylSd     VinylSd       None   0.000000
    ##   ExterQual ExterCond Foundation BsmtQual BsmtCond BsmtExposure
    ## 1         4         3      PConc        4        3            1
    ## 2         3         3     CBlock        4        3            4
    ## 3         4         3      PConc        4        3            2
    ## 4         3         3     BrkTil        3        4            1
    ## 5         4         3      PConc        4        3            3
    ## 6         3         3       Wood        4        3            1
    ##   BsmtFinType1 BsmtFinSF1 BsmtFinType2 BsmtFinSF2 BsmtUnfSF TotalBsmtSF
    ## 1            6   6.561031            1          0       150    6.753438
    ## 2            5   6.886532            1          0       284    7.141245
    ## 3            6   6.188264            1          0       434    6.825460
    ## 4            5   5.379897            1          0       540    6.629363
    ## 5            6   6.486161            1          0       490    7.044033
    ## 6            6   6.597146            1          0        64    6.680855
    ##   Heating HeatingQC CentralAir Electrical X1stFlrSF X2ndFlrSF LowQualFinSF
    ## 1    GasA         5          Y      SBrkr  6.753438       854            0
    ## 2    GasA         5          Y      SBrkr  7.141245         0            0
    ## 3    GasA         5          Y      SBrkr  6.825460       866            0
    ## 4    GasA         4          Y      SBrkr  6.869014       756            0
    ## 5    GasA         5          Y      SBrkr  7.044033      1053            0
    ## 6    GasA         5          Y      SBrkr  6.680855       566            0
    ##   GrLivArea BsmtFullBath BsmtHalfBath BedroomAbvGr KitchenAbvGr
    ## 1  7.444833            1    0.0000000            3    0.6931472
    ## 2  7.141245            0    0.6931472            3    0.6931472
    ## 3  7.488294            1    0.0000000            3    0.6931472
    ## 4  7.448916            1    0.0000000            3    0.6931472
    ## 5  7.695758            1    0.0000000            4    0.6931472
    ## 6  7.217443            1    0.0000000            1    0.6931472
    ##   KitchenQual TotRmsAbvGrd Functional Fireplaces FireplaceQu GarageType
    ## 1           4            8          7          0           0     Attchd
    ## 2           3            6          7          1           3     Attchd
    ## 3           4            6          7          1           3     Attchd
    ## 4           4            7          7          1           4     Detchd
    ## 5           4            9          7          1           3     Attchd
    ## 6           3            5          7          0           0     Attchd
    ##   GarageYrBlt GarageFinish GarageCars GarageArea GarageQual GarageCond
    ## 1        2003            2          2        548          3          3
    ## 2        1976            2          2        460          3          3
    ## 3        2001            2          2        608          3          3
    ## 4        1998            1          3        642          3          3
    ## 5        2000            2          3        836          3          3
    ## 6        1993            1          2        480          3          3
    ##   PavedDrive WoodDeckSF OpenPorchSF EnclosedPorch X3SsnPorch ScreenPorch
    ## 1          2          0    4.127134      0.000000   0.000000           0
    ## 2          2        298    0.000000      0.000000   0.000000           0
    ## 3          2          0    3.761200      0.000000   0.000000           0
    ## 4          2          0    3.583519      5.609472   0.000000           0
    ## 5          2        192    4.442651      0.000000   0.000000           0
    ## 6          2         40    3.433987      0.000000   5.771441           0
    ##   PoolArea PoolQC   Fence   MiscFeature  MiscVal MoSold YrSold SaleType
    ## 1        0      0 NoFence NoMiscFeature 0.000000      2   2008       WD
    ## 2        0      0 NoFence NoMiscFeature 0.000000      5   2007       WD
    ## 3        0      0 NoFence NoMiscFeature 0.000000      9   2008       WD
    ## 4        0      0 NoFence NoMiscFeature 0.000000      2   2006       WD
    ## 5        0      0 NoFence NoMiscFeature 0.000000     12   2008       WD
    ## 6        0      0   MnPrv          Shed 6.552508     10   2009       WD
    ##   SaleCondition SalePrice
    ## 1        Normal  12.24770
    ## 2        Normal  12.10902
    ## 3        Normal  12.31717
    ## 4       Abnorml  11.84940
    ## 5        Normal  12.42922
    ## 6        Normal  11.87061

``` r
head(H_Eng)
```

    ##   Id MSSubClass MSZoning LotFrontage LotArea Street   Alley LotShape
    ## 1  1      2SNEW       RL          65    8450   Pave NoAlley      Reg
    ## 2  2      1SNEW       RL          80    9600   Pave NoAlley      Reg
    ## 3  3      2SNEW       RL          68   11250   Pave NoAlley      IR1
    ## 4  4      2SOLD       RL          60    9550   Pave NoAlley      IR1
    ## 5  5      2SNEW       RL          84   14260   Pave NoAlley      IR1
    ## 6  6     1NHFin       RL          85   14115   Pave NoAlley      IR1
    ##   LandContour LotConfig LandSlope Neighborhood Condition1 Condition2
    ## 1         Lvl    Inside         3      CollgCr       Norm       Norm
    ## 2         Lvl       FR2         3      Veenker      Feedr       Norm
    ## 3         Lvl    Inside         3      CollgCr       Norm       Norm
    ## 4         Lvl    Corner         3      Crawfor       Norm       Norm
    ## 5         Lvl       FR2         3      NoRidge       Norm       Norm
    ## 6         Lvl    Inside         3      Mitchel       Norm       Norm
    ##   BldgType HouseStyle OverallQual OverallCond YearBuilt YearRemodAdd
    ## 1        5     2Story           7           5      2003         2003
    ## 2        5     1Story           6           8      1976         1976
    ## 3        5     2Story           7           5      2001         2002
    ## 4        5     2Story           7           5      1915         1970
    ## 5        5     2Story           8           5      2000         2000
    ## 6        5     1.5Fin           5           5      1993         1995
    ##   RoofStyle RoofMatl Exterior1st Exterior2nd MasVnrType MasVnrArea
    ## 1     Gable  CompShg     VinylSd     VinylSd    BrkFace        196
    ## 2     Gable  CompShg     MetalSd     MetalSd       None          0
    ## 3     Gable  CompShg     VinylSd     VinylSd    BrkFace        162
    ## 4     Gable  CompShg     Wd Sdng     Wd Shng       None          0
    ## 5     Gable  CompShg     VinylSd     VinylSd    BrkFace        350
    ## 6     Gable  CompShg     VinylSd     VinylSd       None          0
    ##   ExterQual ExterCond Foundation BsmtQual BsmtCond BsmtExposure
    ## 1         4         3      PConc        4        3            1
    ## 2         3         3     CBlock        4        3            4
    ## 3         4         3      PConc        4        3            2
    ## 4         3         3     BrkTil        3        4            1
    ## 5         4         3      PConc        4        3            3
    ## 6         3         3       Wood        4        3            1
    ##   BsmtFinType1 BsmtFinSF1 BsmtFinType2 BsmtFinSF2 BsmtUnfSF TotalBsmtSF
    ## 1            6        706            1          0       150         856
    ## 2            5        978            1          0       284        1262
    ## 3            6        486            1          0       434         920
    ## 4            5        216            1          0       540         756
    ## 5            6        655            1          0       490        1145
    ## 6            6        732            1          0        64         796
    ##   Heating HeatingQC CentralAir Electrical X1stFlrSF X2ndFlrSF LowQualFinSF
    ## 1    GasA         5          Y      SBrkr       856       854            0
    ## 2    GasA         5          Y      SBrkr      1262         0            0
    ## 3    GasA         5          Y      SBrkr       920       866            0
    ## 4    GasA         4          Y      SBrkr       961       756            0
    ## 5    GasA         5          Y      SBrkr      1145      1053            0
    ## 6    GasA         5          Y      SBrkr       796       566            0
    ##   GrLivArea BsmtFullBath BsmtHalfBath BedroomAbvGr KitchenAbvGr
    ## 1      1710            1            0            3            1
    ## 2      1262            0            1            3            1
    ## 3      1786            1            0            3            1
    ## 4      1717            1            0            3            1
    ## 5      2198            1            0            4            1
    ## 6      1362            1            0            1            1
    ##   KitchenQual TotRmsAbvGrd Functional Fireplaces FireplaceQu GarageType
    ## 1           4            8          7          0           0     Attchd
    ## 2           3            6          7          1           3     Attchd
    ## 3           4            6          7          1           3     Attchd
    ## 4           4            7          7          1           4     Detchd
    ## 5           4            9          7          1           3     Attchd
    ## 6           3            5          7          0           0     Attchd
    ##   GarageYrBlt GarageFinish GarageCars GarageArea GarageQual GarageCond
    ## 1        2003            2          2        548          3          3
    ## 2        1976            2          2        460          3          3
    ## 3        2001            2          2        608          3          3
    ## 4        1998            1          3        642          3          3
    ## 5        2000            2          3        836          3          3
    ## 6        1993            1          2        480          3          3
    ##   PavedDrive WoodDeckSF OpenPorchSF EnclosedPorch X3SsnPorch ScreenPorch
    ## 1          2          0          61             0          0           0
    ## 2          2        298           0             0          0           0
    ## 3          2          0          42             0          0           0
    ## 4          2          0          35           272          0           0
    ## 5          2        192          84             0          0           0
    ## 6          2         40          30             0        320           0
    ##   PoolArea PoolQC   Fence   MiscFeature MiscVal MoSold YrSold SaleType
    ## 1        0      0 NoFence NoMiscFeature       0      2   2008       WD
    ## 2        0      0 NoFence NoMiscFeature       0      5   2007       WD
    ## 3        0      0 NoFence NoMiscFeature       0      9   2008       WD
    ## 4        0      0 NoFence NoMiscFeature       0      2   2006       WD
    ## 5        0      0 NoFence NoMiscFeature       0     12   2008       WD
    ## 6        0      0   MnPrv          Shed     700     10   2009       WD
    ##   SaleCondition SalePrice
    ## 1        Normal    208500
    ## 2        Normal    181500
    ## 3        Normal    223500
    ## 4       Abnorml    140000
    ## 5        Normal    250000
    ## 6        Normal    143000

Feature Selection done on the engineered.

``` r
nulllog<-lm(SalePrice~1,data=H_Lo)
fulllog<-lm(SalePrice~.,data=H_Lo)

StepF_Log<-stepAIC(nulllog, scope=list(lower=nulllog, upper=fulllog), direction='forward', trace=F)
StepB_Log<-stepAIC(fulllog, direction='backward', trace=F)
StepS_Log<-stepAIC(fulllog,direction='both', trace=F)
```

``` r
summary(StepF_Log)
```

    ## 
    ## Call:
    ## lm(formula = SalePrice ~ OverallQual + GrLivArea + Neighborhood + 
    ##     MSSubClass + BsmtFinSF1 + OverallCond + GarageCars + YearBuilt + 
    ##     RoofMatl + LotArea + MSZoning + BsmtExposure + Condition2 + 
    ##     Functional + SaleCondition + KitchenQual + Condition1 + Exterior1st + 
    ##     BsmtFullBath + Fireplaces + BsmtQual + HeatingQC + PoolQC + 
    ##     ScreenPorch + Heating + LotConfig + GarageArea + Foundation + 
    ##     WoodDeckSF + CentralAir + SaleType + X1stFlrSF + X2ndFlrSF + 
    ##     Street + YearRemodAdd + MoSold + KitchenAbvGr + BedroomAbvGr + 
    ##     EnclosedPorch + TotRmsAbvGrd, data = H_Lo)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.67551 -0.04722  0.00133  0.05405  0.52939 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          -9.572e-01  8.006e-01  -1.196 0.232081    
    ## OverallQual           4.713e-02  4.254e-03  11.078  < 2e-16 ***
    ## GrLivArea             3.160e-01  4.840e-02   6.529 9.39e-11 ***
    ## NeighborhoodBlueste   2.774e-02  8.532e-02   0.325 0.745156    
    ## NeighborhoodBrDale    3.850e-02  4.954e-02   0.777 0.437234    
    ## NeighborhoodBrkSide   5.313e-02  4.042e-02   1.314 0.188928    
    ## NeighborhoodClearCr   1.882e-02  3.984e-02   0.472 0.636753    
    ## NeighborhoodCollgCr  -3.174e-02  3.221e-02  -0.986 0.324531    
    ## NeighborhoodCrawfor   9.927e-02  3.710e-02   2.676 0.007544 ** 
    ## NeighborhoodEdwards  -6.241e-02  3.521e-02  -1.772 0.076547 .  
    ## NeighborhoodGilbert  -2.432e-02  3.399e-02  -0.716 0.474422    
    ## NeighborhoodIDOTRR   -8.467e-03  4.631e-02  -0.183 0.854957    
    ## NeighborhoodMeadowV  -9.606e-02  5.252e-02  -1.829 0.067643 .  
    ## NeighborhoodMitchel  -5.775e-02  3.583e-02  -1.612 0.107224    
    ## NeighborhoodNAmes    -2.984e-02  3.417e-02  -0.873 0.382771    
    ## NeighborhoodNoRidge   6.434e-02  3.670e-02   1.753 0.079830 .  
    ## NeighborhoodNPkVill   6.397e-02  4.859e-02   1.317 0.188209    
    ## NeighborhoodNridgHt   9.295e-02  3.176e-02   2.927 0.003481 ** 
    ## NeighborhoodNWAmes   -3.869e-02  3.531e-02  -1.096 0.273333    
    ## NeighborhoodOldTown  -2.122e-02  4.136e-02  -0.513 0.607915    
    ## NeighborhoodSawyer   -1.803e-02  3.575e-02  -0.504 0.614102    
    ## NeighborhoodSawyerW  -2.170e-02  3.471e-02  -0.625 0.532083    
    ## NeighborhoodSomerst   1.493e-02  3.925e-02   0.380 0.703662    
    ## NeighborhoodStoneBr   1.243e-01  3.605e-02   3.447 0.000585 ***
    ## NeighborhoodSWISU     9.476e-03  4.148e-02   0.228 0.819355    
    ## NeighborhoodTimber   -1.689e-02  3.603e-02  -0.469 0.639410    
    ## NeighborhoodVeenker   5.730e-03  4.624e-02   0.124 0.901409    
    ## MSSubClass1NHFin      5.408e-02  2.902e-02   1.864 0.062552 .  
    ## MSSubClass1NHUnf      9.546e-02  4.495e-02   2.123 0.033900 *  
    ## MSSubClass1SFA        2.389e-02  6.281e-02   0.380 0.703763    
    ## MSSubClass1SNEW       8.825e-02  2.948e-02   2.994 0.002805 ** 
    ## MSSubClass1SOLD       4.939e-02  3.575e-02   1.382 0.167275    
    ## MSSubClass1SPUD       5.000e-02  2.872e-02   1.741 0.081918 .  
    ## MSSubClass2FCONV      3.018e-02  3.821e-02   0.790 0.429764    
    ## MSSubClass2NHS        3.430e-02  4.239e-02   0.809 0.418648    
    ## MSSubClass2SNEW       3.710e-02  2.498e-02   1.485 0.137767    
    ## MSSubClass2SOLD       5.960e-02  3.299e-02   1.806 0.071083 .  
    ## MSSubClassDUPL        5.235e-02  3.897e-02   1.343 0.179462    
    ## MSSubClassMLPUD       4.653e-02  4.554e-02   1.022 0.307091    
    ## MSSubClassSPL         5.240e-02  3.079e-02   1.702 0.088985 .  
    ## MSSubClassSPLF        6.935e-02  3.722e-02   1.863 0.062659 .  
    ## BsmtFinSF1            9.282e-03  1.326e-03   6.999 4.09e-12 ***
    ## OverallCond           3.753e-02  3.576e-03  10.495  < 2e-16 ***
    ## GarageCars            2.575e-02  9.432e-03   2.730 0.006417 ** 
    ## YearBuilt             2.261e-03  3.353e-04   6.744 2.29e-11 ***
    ## RoofMatlCompShg       1.691e+00  1.238e-01  13.660  < 2e-16 ***
    ## RoofMatlMembran       1.818e+00  1.660e-01  10.955  < 2e-16 ***
    ## RoofMatlMetal         1.755e+00  1.660e-01  10.570  < 2e-16 ***
    ## RoofMatlRoll          1.673e+00  1.671e-01  10.012  < 2e-16 ***
    ## RoofMatlTar&Grv       1.656e+00  1.276e-01  12.977  < 2e-16 ***
    ## RoofMatlWdShake       1.695e+00  1.349e-01  12.567  < 2e-16 ***
    ## RoofMatlWdShngl       1.856e+00  1.313e-01  14.139  < 2e-16 ***
    ## LotArea               8.017e-02  1.060e-02   7.563 7.32e-14 ***
    ## MSZoningFV            4.329e-01  5.171e-02   8.371  < 2e-16 ***
    ## MSZoningRH            3.971e-01  5.196e-02   7.642 4.07e-14 ***
    ## MSZoningRL            3.937e-01  4.390e-02   8.969  < 2e-16 ***
    ## MSZoningRM            3.656e-01  4.114e-02   8.885  < 2e-16 ***
    ## BsmtExposure          1.685e-02  3.534e-03   4.768 2.07e-06 ***
    ## Condition2Feedr       2.836e-02  9.697e-02   0.292 0.769970    
    ## Condition2Norm        2.184e-02  8.222e-02   0.266 0.790600    
    ## Condition2PosA        3.627e-01  1.380e-01   2.628 0.008699 ** 
    ## Condition2PosN       -6.573e-01  1.168e-01  -5.628 2.22e-08 ***
    ## Condition2RRAe       -9.630e-02  1.386e-01  -0.695 0.487396    
    ## Condition2RRAn       -5.406e-03  1.355e-01  -0.040 0.968171    
    ## Condition2RRNn        6.261e-02  1.138e-01   0.550 0.582374    
    ## Functional            3.764e-02  4.716e-03   7.981 3.11e-15 ***
    ## SaleConditionAdjLand  1.328e-01  5.982e-02   2.220 0.026577 *  
    ## SaleConditionAlloca   4.765e-02  3.636e-02   1.310 0.190266    
    ## SaleConditionFamily   1.384e-02  2.709e-02   0.511 0.609448    
    ## SaleConditionNormal   6.836e-02  1.249e-02   5.475 5.24e-08 ***
    ## SaleConditionPartial -6.300e-02  6.559e-02  -0.961 0.336974    
    ## KitchenQual           2.152e-02  6.914e-03   3.112 0.001897 ** 
    ## Condition1Feedr       3.579e-02  2.165e-02   1.654 0.098441 .  
    ## Condition1Norm        8.311e-02  1.793e-02   4.635 3.93e-06 ***
    ## Condition1PosA        4.531e-02  4.334e-02   1.045 0.295999    
    ## Condition1PosN        8.686e-02  3.232e-02   2.688 0.007288 ** 
    ## Condition1RRAe       -4.711e-02  3.831e-02  -1.230 0.219099    
    ## Condition1RRAn        5.277e-02  2.971e-02   1.776 0.075935 .  
    ## Condition1RRNe        2.250e-02  7.876e-02   0.286 0.775154    
    ## Condition1RRNn        1.126e-01  5.501e-02   2.048 0.040765 *  
    ## Exterior1stAsphShn   -4.161e-02  1.120e-01  -0.371 0.710392    
    ## Exterior1stBrkComm   -1.663e-01  8.368e-02  -1.988 0.047042 *  
    ## Exterior1stBrkFace    1.009e-01  3.129e-02   3.223 0.001297 ** 
    ## Exterior1stCBlock    -2.373e-02  1.119e-01  -0.212 0.832164    
    ## Exterior1stCemntBd    5.918e-02  3.247e-02   1.823 0.068554 .  
    ## Exterior1stHdBoard    1.257e-02  2.843e-02   0.442 0.658326    
    ## Exterior1stImStucc   -1.203e-02  1.103e-01  -0.109 0.913147    
    ## Exterior1stMetalSd    4.307e-02  2.758e-02   1.561 0.118647    
    ## Exterior1stPlywood    1.162e-02  2.982e-02   0.390 0.696890    
    ## Exterior1stStone     -3.861e-04  8.603e-02  -0.004 0.996420    
    ## Exterior1stStucco     3.864e-02  3.499e-02   1.104 0.269698    
    ## Exterior1stVinylSd    2.988e-02  2.784e-02   1.073 0.283328    
    ## Exterior1stWd Sdng    8.949e-03  2.749e-02   0.326 0.744827    
    ## Exterior1stWdShing    1.522e-02  3.445e-02   0.442 0.658605    
    ## BsmtFullBath          2.933e-02  7.347e-03   3.993 6.89e-05 ***
    ## Fireplaces            2.176e-02  5.868e-03   3.708 0.000217 ***
    ## BsmtQual              2.097e-02  6.431e-03   3.260 0.001141 ** 
    ## HeatingQC             1.540e-02  4.181e-03   3.683 0.000240 ***
    ## PoolQC                4.306e-02  1.197e-02   3.597 0.000334 ***
    ## ScreenPorch           8.942e-03  2.111e-03   4.236 2.44e-05 ***
    ## HeatingGasA           8.202e-02  1.103e-01   0.744 0.457134    
    ## HeatingGasW           1.563e-01  1.131e-01   1.383 0.167008    
    ## HeatingGrav          -5.288e-03  1.171e-01  -0.045 0.963991    
    ## HeatingOthW          -4.843e-03  1.361e-01  -0.036 0.971620    
    ## HeatingWall           1.424e-01  1.258e-01   1.132 0.257790    
    ## LotConfigCulDSac      1.813e-02  1.380e-02   1.314 0.189139    
    ## LotConfigFR2         -3.227e-02  1.765e-02  -1.828 0.067746 .  
    ## LotConfigFR3         -9.078e-02  5.625e-02  -1.614 0.106801    
    ## LotConfigInside      -1.052e-02  7.656e-03  -1.374 0.169738    
    ## GarageArea            9.023e-05  3.224e-05   2.799 0.005204 ** 
    ## FoundationCBlock      2.032e-02  1.369e-02   1.484 0.137926    
    ## FoundationPConc       3.883e-02  1.495e-02   2.598 0.009494 ** 
    ## FoundationSlab        2.221e-02  3.515e-02   0.632 0.527599    
    ## FoundationStone       9.982e-02  4.580e-02   2.180 0.029461 *  
    ## FoundationWood       -1.158e-01  6.388e-02  -1.812 0.070206 .  
    ## WoodDeckSF            6.885e-05  2.543e-05   2.707 0.006877 ** 
    ## CentralAirY           3.707e-02  1.588e-02   2.334 0.019719 *  
    ## SaleTypeCon           6.485e-02  7.922e-02   0.819 0.413147    
    ## SaleTypeConLD         1.342e-01  4.241e-02   3.164 0.001590 ** 
    ## SaleTypeConLI         6.553e-03  5.134e-02   0.128 0.898464    
    ## SaleTypeConLw         8.632e-03  5.254e-02   0.164 0.869529    
    ## SaleTypeCWD           5.072e-02  5.730e-02   0.885 0.376235    
    ## SaleTypeNew           1.812e-01  6.798e-02   2.665 0.007787 ** 
    ## SaleTypeOth           7.025e-02  6.496e-02   1.081 0.279683    
    ## SaleTypeWD           -4.426e-03  1.838e-02  -0.241 0.809717    
    ## X1stFlrSF             1.457e-01  4.050e-02   3.598 0.000332 ***
    ## X2ndFlrSF             9.108e-05  2.820e-05   3.229 0.001270 ** 
    ## StreetPave            9.666e-02  4.885e-02   1.979 0.048078 *  
    ## YearRemodAdd          4.073e-04  2.313e-04   1.761 0.078479 .  
    ## MoSold               -1.539e-03  1.066e-03  -1.443 0.149280    
    ## KitchenAbvGr         -1.067e-01  6.027e-02  -1.771 0.076816 .  
    ## BedroomAbvGr         -1.092e-02  5.800e-03  -1.883 0.059915 .  
    ## EnclosedPorch         2.897e-03  1.959e-03   1.479 0.139300    
    ## TotRmsAbvGrd          5.583e-03  4.011e-03   1.392 0.164162    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1043 on 1326 degrees of freedom
    ##   (1459 observations deleted due to missingness)
    ## Multiple R-squared:  0.9381, Adjusted R-squared:  0.9318 
    ## F-statistic:   151 on 133 and 1326 DF,  p-value: < 2.2e-16

``` r
summary(StepB_Log)
```

    ## 
    ## Call:
    ## lm(formula = SalePrice ~ MSZoning + LotArea + Street + LotConfig + 
    ##     Neighborhood + Condition1 + Condition2 + BldgType + OverallQual + 
    ##     OverallCond + YearBuilt + YearRemodAdd + RoofMatl + Exterior1st + 
    ##     Foundation + BsmtQual + BsmtCond + BsmtExposure + BsmtFinSF1 + 
    ##     TotalBsmtSF + Heating + HeatingQC + CentralAir + X1stFlrSF + 
    ##     X2ndFlrSF + GrLivArea + BsmtFullBath + BedroomAbvGr + KitchenAbvGr + 
    ##     KitchenQual + Functional + Fireplaces + GarageType + GarageFinish + 
    ##     GarageCars + GarageArea + GarageQual + WoodDeckSF + EnclosedPorch + 
    ##     ScreenPorch + PoolQC + MoSold + SaleType + SaleCondition, 
    ##     data = H_Lo)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.68257 -0.05012  0.00110  0.05466  0.53795 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          -1.211e+00  7.521e-01  -1.610 0.107690    
    ## MSZoningFV            4.175e-01  5.128e-02   8.142 8.88e-16 ***
    ## MSZoningRH            4.052e-01  5.163e-02   7.848 8.61e-15 ***
    ## MSZoningRL            3.867e-01  4.355e-02   8.880  < 2e-16 ***
    ## MSZoningRM            3.621e-01  4.094e-02   8.844  < 2e-16 ***
    ## LotArea               8.213e-02  1.023e-02   8.031 2.11e-15 ***
    ## StreetPave            7.701e-02  4.939e-02   1.559 0.119199    
    ## LotConfigCulDSac      1.617e-02  1.376e-02   1.175 0.240158    
    ## LotConfigFR2         -3.028e-02  1.764e-02  -1.716 0.086366 .  
    ## LotConfigFR3         -9.838e-02  5.613e-02  -1.753 0.079850 .  
    ## LotConfigInside      -1.023e-02  7.682e-03  -1.331 0.183338    
    ## NeighborhoodBlueste   1.120e-02  8.284e-02   0.135 0.892462    
    ## NeighborhoodBrDale    2.801e-02  4.496e-02   0.623 0.533400    
    ## NeighborhoodBrkSide   3.194e-02  3.868e-02   0.826 0.409134    
    ## NeighborhoodClearCr   6.575e-03  3.904e-02   0.168 0.866271    
    ## NeighborhoodCollgCr  -3.589e-02  3.113e-02  -1.153 0.249134    
    ## NeighborhoodCrawfor   1.021e-01  3.578e-02   2.852 0.004406 ** 
    ## NeighborhoodEdwards  -6.960e-02  3.381e-02  -2.059 0.039705 *  
    ## NeighborhoodGilbert  -3.522e-02  3.273e-02  -1.076 0.282112    
    ## NeighborhoodIDOTRR   -2.777e-02  4.466e-02  -0.622 0.534194    
    ## NeighborhoodMeadowV  -8.866e-02  4.720e-02  -1.879 0.060527 .  
    ## NeighborhoodMitchel  -5.907e-02  3.456e-02  -1.709 0.087612 .  
    ## NeighborhoodNAmes    -3.549e-02  3.276e-02  -1.083 0.278872    
    ## NeighborhoodNoRidge   4.952e-02  3.530e-02   1.403 0.160937    
    ## NeighborhoodNPkVill   6.152e-02  4.745e-02   1.297 0.194999    
    ## NeighborhoodNridgHt   9.130e-02  3.117e-02   2.929 0.003453 ** 
    ## NeighborhoodNWAmes   -4.726e-02  3.413e-02  -1.385 0.166386    
    ## NeighborhoodOldTown  -3.364e-02  3.993e-02  -0.842 0.399684    
    ## NeighborhoodSawyer   -2.254e-02  3.443e-02  -0.655 0.512853    
    ## NeighborhoodSawyerW  -3.041e-02  3.370e-02  -0.902 0.367013    
    ## NeighborhoodSomerst   1.177e-02  3.816e-02   0.309 0.757711    
    ## NeighborhoodStoneBr   1.180e-01  3.541e-02   3.334 0.000879 ***
    ## NeighborhoodSWISU    -1.892e-03  4.023e-02  -0.047 0.962505    
    ## NeighborhoodTimber   -2.147e-02  3.486e-02  -0.616 0.538028    
    ## NeighborhoodVeenker  -1.083e-05  4.551e-02   0.000 0.999810    
    ## Condition1Feedr       4.017e-02  2.139e-02   1.878 0.060609 .  
    ## Condition1Norm        8.517e-02  1.757e-02   4.847 1.40e-06 ***
    ## Condition1PosA        4.300e-02  4.290e-02   1.002 0.316351    
    ## Condition1PosN        8.755e-02  3.175e-02   2.758 0.005896 ** 
    ## Condition1RRAe       -4.289e-02  3.832e-02  -1.119 0.263276    
    ## Condition1RRAn        5.685e-02  2.936e-02   1.936 0.053088 .  
    ## Condition1RRNe        1.816e-02  7.844e-02   0.232 0.816904    
    ## Condition1RRNn        1.313e-01  5.521e-02   2.378 0.017545 *  
    ## Condition2Feedr       1.473e-02  9.637e-02   0.153 0.878556    
    ## Condition2Norm        1.309e-02  8.210e-02   0.159 0.873344    
    ## Condition2PosA        3.181e-01  1.359e-01   2.341 0.019382 *  
    ## Condition2PosN       -6.734e-01  1.169e-01  -5.759 1.05e-08 ***
    ## Condition2RRAe       -6.276e-02  1.360e-01  -0.462 0.644460    
    ## Condition2RRAn       -4.399e-02  1.347e-01  -0.327 0.744000    
    ## Condition2RRNn        5.822e-02  1.135e-01   0.513 0.608211    
    ## BldgType              1.219e-02  4.706e-03   2.589 0.009723 ** 
    ## OverallQual           4.731e-02  4.236e-03  11.169  < 2e-16 ***
    ## OverallCond           3.811e-02  3.645e-03  10.458  < 2e-16 ***
    ## YearBuilt             2.256e-03  3.008e-04   7.502 1.15e-13 ***
    ## YearRemodAdd          4.295e-04  2.314e-04   1.856 0.063688 .  
    ## RoofMatlCompShg       1.705e+00  1.226e-01  13.911  < 2e-16 ***
    ## RoofMatlMembran       1.860e+00  1.646e-01  11.300  < 2e-16 ***
    ## RoofMatlMetal         1.767e+00  1.641e-01  10.762  < 2e-16 ***
    ## RoofMatlRoll          1.700e+00  1.642e-01  10.354  < 2e-16 ***
    ## RoofMatlTar&Grv       1.686e+00  1.266e-01  13.310  < 2e-16 ***
    ## RoofMatlWdShake       1.698e+00  1.328e-01  12.788  < 2e-16 ***
    ## RoofMatlWdShngl       1.859e+00  1.298e-01  14.326  < 2e-16 ***
    ## Exterior1stAsphShn   -3.136e-02  1.140e-01  -0.275 0.783188    
    ## Exterior1stBrkComm   -1.957e-01  8.420e-02  -2.324 0.020260 *  
    ## Exterior1stBrkFace    9.488e-02  3.114e-02   3.047 0.002356 ** 
    ## Exterior1stCBlock    -4.178e-02  1.125e-01  -0.371 0.710446    
    ## Exterior1stCemntBd    4.615e-02  3.240e-02   1.424 0.154558    
    ## Exterior1stHdBoard    2.199e-03  2.838e-02   0.077 0.938248    
    ## Exterior1stImStucc   -1.460e-02  1.101e-01  -0.133 0.894513    
    ## Exterior1stMetalSd    3.329e-02  2.753e-02   1.209 0.226819    
    ## Exterior1stPlywood   -4.406e-04  2.972e-02  -0.015 0.988173    
    ## Exterior1stStone     -1.065e-02  8.538e-02  -0.125 0.900719    
    ## Exterior1stStucco     2.994e-02  3.465e-02   0.864 0.387808    
    ## Exterior1stVinylSd    1.908e-02  2.774e-02   0.688 0.491733    
    ## Exterior1stWd Sdng    6.773e-04  2.749e-02   0.025 0.980345    
    ## Exterior1stWdShing    4.392e-03  3.429e-02   0.128 0.898101    
    ## FoundationCBlock      2.725e-02  1.349e-02   2.020 0.043630 *  
    ## FoundationPConc       4.272e-02  1.491e-02   2.865 0.004233 ** 
    ## FoundationSlab        7.069e-02  4.209e-02   1.679 0.093306 .  
    ## FoundationStone       1.188e-01  4.551e-02   2.611 0.009133 ** 
    ## FoundationWood       -1.040e-01  6.411e-02  -1.622 0.105090    
    ## BsmtQual              1.756e-02  7.714e-03   2.277 0.022960 *  
    ## BsmtCond             -1.453e-02  1.053e-02  -1.381 0.167646    
    ## BsmtExposure          1.496e-02  3.326e-03   4.498 7.44e-06 ***
    ## BsmtFinSF1            8.671e-03  1.333e-03   6.503 1.11e-10 ***
    ## TotalBsmtSF           1.287e-02  6.860e-03   1.876 0.060855 .  
    ## HeatingGasA           3.318e-02  1.123e-01   0.296 0.767618    
    ## HeatingGasW           1.097e-01  1.150e-01   0.954 0.340481    
    ## HeatingGrav          -4.714e-02  1.190e-01  -0.396 0.691971    
    ## HeatingOthW          -3.662e-02  1.372e-01  -0.267 0.789545    
    ## HeatingWall           7.076e-02  1.272e-01   0.556 0.578118    
    ## HeatingQC             1.487e-02  4.176e-03   3.560 0.000384 ***
    ## CentralAirY           4.148e-02  1.566e-02   2.649 0.008167 ** 
    ## X1stFlrSF             1.869e-01  3.379e-02   5.530 3.85e-08 ***
    ## X2ndFlrSF             8.538e-05  2.627e-05   3.250 0.001182 ** 
    ## GrLivArea             2.889e-01  4.046e-02   7.141 1.52e-12 ***
    ## BsmtFullBath          3.005e-02  7.304e-03   4.114 4.14e-05 ***
    ## BedroomAbvGr         -7.363e-03  5.210e-03  -1.413 0.157827    
    ## KitchenAbvGr         -8.229e-02  4.406e-02  -1.868 0.062005 .  
    ## KitchenQual           2.354e-02  6.866e-03   3.428 0.000626 ***
    ## Functional            3.894e-02  4.630e-03   8.411  < 2e-16 ***
    ## Fireplaces            1.888e-02  5.839e-03   3.233 0.001255 ** 
    ## GarageTypeAttchd      8.175e-02  4.718e-02   1.733 0.083388 .  
    ## GarageTypeBasment     6.287e-02  5.357e-02   1.174 0.240799    
    ## GarageTypeBuiltIn     7.704e-02  4.916e-02   1.567 0.117366    
    ## GarageTypeCarPort     3.440e-02  5.996e-02   0.574 0.566239    
    ## GarageTypeDetchd      9.549e-02  4.685e-02   2.038 0.041726 *  
    ## GarageTypeNoGarage    1.662e-01  6.154e-02   2.701 0.006998 ** 
    ## GarageFinish          7.962e-03  5.197e-03   1.532 0.125769    
    ## GarageCars            2.955e-02  9.821e-03   3.008 0.002675 ** 
    ## GarageArea            7.990e-05  3.310e-05   2.414 0.015931 *  
    ## GarageQual            2.160e-02  1.281e-02   1.686 0.092067 .  
    ## WoodDeckSF            7.143e-05  2.533e-05   2.820 0.004878 ** 
    ## EnclosedPorch         2.844e-03  1.947e-03   1.461 0.144268    
    ## ScreenPorch           9.048e-03  2.097e-03   4.315 1.72e-05 ***
    ## PoolQC                3.856e-02  1.181e-02   3.265 0.001124 ** 
    ## MoSold               -1.502e-03  1.058e-03  -1.419 0.156127    
    ## SaleTypeCon           6.394e-02  7.902e-02   0.809 0.418606    
    ## SaleTypeConLD         1.110e-01  4.173e-02   2.661 0.007879 ** 
    ## SaleTypeConLI        -1.010e-02  5.157e-02  -0.196 0.844714    
    ## SaleTypeConLw        -2.929e-03  5.228e-02  -0.056 0.955334    
    ## SaleTypeCWD           4.023e-02  5.716e-02   0.704 0.481691    
    ## SaleTypeNew           1.669e-01  6.779e-02   2.463 0.013920 *  
    ## SaleTypeOth           6.610e-02  6.493e-02   1.018 0.308842    
    ## SaleTypeWD           -1.304e-02  1.840e-02  -0.709 0.478736    
    ## SaleConditionAdjLand  1.422e-01  5.818e-02   2.445 0.014621 *  
    ## SaleConditionAlloca   5.488e-02  3.629e-02   1.512 0.130727    
    ## SaleConditionFamily   1.902e-02  2.702e-02   0.704 0.481631    
    ## SaleConditionNormal   6.907e-02  1.248e-02   5.534 3.76e-08 ***
    ## SaleConditionPartial -5.458e-02  6.543e-02  -0.834 0.404322    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1042 on 1330 degrees of freedom
    ##   (1459 observations deleted due to missingness)
    ## Multiple R-squared:  0.938,  Adjusted R-squared:  0.932 
    ## F-statistic:   156 on 129 and 1330 DF,  p-value: < 2.2e-16

``` r
summary(StepS_Log)
```

    ## 
    ## Call:
    ## lm(formula = SalePrice ~ MSZoning + LotArea + Street + LotConfig + 
    ##     Neighborhood + Condition1 + Condition2 + BldgType + OverallQual + 
    ##     OverallCond + YearBuilt + YearRemodAdd + RoofMatl + Exterior1st + 
    ##     Foundation + BsmtQual + BsmtCond + BsmtExposure + BsmtFinSF1 + 
    ##     TotalBsmtSF + Heating + HeatingQC + CentralAir + X1stFlrSF + 
    ##     X2ndFlrSF + GrLivArea + BsmtFullBath + BedroomAbvGr + KitchenAbvGr + 
    ##     KitchenQual + Functional + Fireplaces + GarageType + GarageFinish + 
    ##     GarageCars + GarageArea + GarageQual + WoodDeckSF + EnclosedPorch + 
    ##     ScreenPorch + PoolQC + MoSold + SaleType + SaleCondition, 
    ##     data = H_Lo)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.68257 -0.05012  0.00110  0.05466  0.53795 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          -1.211e+00  7.521e-01  -1.610 0.107690    
    ## MSZoningFV            4.175e-01  5.128e-02   8.142 8.88e-16 ***
    ## MSZoningRH            4.052e-01  5.163e-02   7.848 8.61e-15 ***
    ## MSZoningRL            3.867e-01  4.355e-02   8.880  < 2e-16 ***
    ## MSZoningRM            3.621e-01  4.094e-02   8.844  < 2e-16 ***
    ## LotArea               8.213e-02  1.023e-02   8.031 2.11e-15 ***
    ## StreetPave            7.701e-02  4.939e-02   1.559 0.119199    
    ## LotConfigCulDSac      1.617e-02  1.376e-02   1.175 0.240158    
    ## LotConfigFR2         -3.028e-02  1.764e-02  -1.716 0.086366 .  
    ## LotConfigFR3         -9.838e-02  5.613e-02  -1.753 0.079850 .  
    ## LotConfigInside      -1.023e-02  7.682e-03  -1.331 0.183338    
    ## NeighborhoodBlueste   1.120e-02  8.284e-02   0.135 0.892462    
    ## NeighborhoodBrDale    2.801e-02  4.496e-02   0.623 0.533400    
    ## NeighborhoodBrkSide   3.194e-02  3.868e-02   0.826 0.409134    
    ## NeighborhoodClearCr   6.575e-03  3.904e-02   0.168 0.866271    
    ## NeighborhoodCollgCr  -3.589e-02  3.113e-02  -1.153 0.249134    
    ## NeighborhoodCrawfor   1.021e-01  3.578e-02   2.852 0.004406 ** 
    ## NeighborhoodEdwards  -6.960e-02  3.381e-02  -2.059 0.039705 *  
    ## NeighborhoodGilbert  -3.522e-02  3.273e-02  -1.076 0.282112    
    ## NeighborhoodIDOTRR   -2.777e-02  4.466e-02  -0.622 0.534194    
    ## NeighborhoodMeadowV  -8.866e-02  4.720e-02  -1.879 0.060527 .  
    ## NeighborhoodMitchel  -5.907e-02  3.456e-02  -1.709 0.087612 .  
    ## NeighborhoodNAmes    -3.549e-02  3.276e-02  -1.083 0.278872    
    ## NeighborhoodNoRidge   4.952e-02  3.530e-02   1.403 0.160937    
    ## NeighborhoodNPkVill   6.152e-02  4.745e-02   1.297 0.194999    
    ## NeighborhoodNridgHt   9.130e-02  3.117e-02   2.929 0.003453 ** 
    ## NeighborhoodNWAmes   -4.726e-02  3.413e-02  -1.385 0.166386    
    ## NeighborhoodOldTown  -3.364e-02  3.993e-02  -0.842 0.399684    
    ## NeighborhoodSawyer   -2.254e-02  3.443e-02  -0.655 0.512853    
    ## NeighborhoodSawyerW  -3.041e-02  3.370e-02  -0.902 0.367013    
    ## NeighborhoodSomerst   1.177e-02  3.816e-02   0.309 0.757711    
    ## NeighborhoodStoneBr   1.180e-01  3.541e-02   3.334 0.000879 ***
    ## NeighborhoodSWISU    -1.892e-03  4.023e-02  -0.047 0.962505    
    ## NeighborhoodTimber   -2.147e-02  3.486e-02  -0.616 0.538028    
    ## NeighborhoodVeenker  -1.083e-05  4.551e-02   0.000 0.999810    
    ## Condition1Feedr       4.017e-02  2.139e-02   1.878 0.060609 .  
    ## Condition1Norm        8.517e-02  1.757e-02   4.847 1.40e-06 ***
    ## Condition1PosA        4.300e-02  4.290e-02   1.002 0.316351    
    ## Condition1PosN        8.755e-02  3.175e-02   2.758 0.005896 ** 
    ## Condition1RRAe       -4.289e-02  3.832e-02  -1.119 0.263276    
    ## Condition1RRAn        5.685e-02  2.936e-02   1.936 0.053088 .  
    ## Condition1RRNe        1.816e-02  7.844e-02   0.232 0.816904    
    ## Condition1RRNn        1.313e-01  5.521e-02   2.378 0.017545 *  
    ## Condition2Feedr       1.473e-02  9.637e-02   0.153 0.878556    
    ## Condition2Norm        1.309e-02  8.210e-02   0.159 0.873344    
    ## Condition2PosA        3.181e-01  1.359e-01   2.341 0.019382 *  
    ## Condition2PosN       -6.734e-01  1.169e-01  -5.759 1.05e-08 ***
    ## Condition2RRAe       -6.276e-02  1.360e-01  -0.462 0.644460    
    ## Condition2RRAn       -4.399e-02  1.347e-01  -0.327 0.744000    
    ## Condition2RRNn        5.822e-02  1.135e-01   0.513 0.608211    
    ## BldgType              1.219e-02  4.706e-03   2.589 0.009723 ** 
    ## OverallQual           4.731e-02  4.236e-03  11.169  < 2e-16 ***
    ## OverallCond           3.811e-02  3.645e-03  10.458  < 2e-16 ***
    ## YearBuilt             2.256e-03  3.008e-04   7.502 1.15e-13 ***
    ## YearRemodAdd          4.295e-04  2.314e-04   1.856 0.063688 .  
    ## RoofMatlCompShg       1.705e+00  1.226e-01  13.911  < 2e-16 ***
    ## RoofMatlMembran       1.860e+00  1.646e-01  11.300  < 2e-16 ***
    ## RoofMatlMetal         1.767e+00  1.641e-01  10.762  < 2e-16 ***
    ## RoofMatlRoll          1.700e+00  1.642e-01  10.354  < 2e-16 ***
    ## RoofMatlTar&Grv       1.686e+00  1.266e-01  13.310  < 2e-16 ***
    ## RoofMatlWdShake       1.698e+00  1.328e-01  12.788  < 2e-16 ***
    ## RoofMatlWdShngl       1.859e+00  1.298e-01  14.326  < 2e-16 ***
    ## Exterior1stAsphShn   -3.136e-02  1.140e-01  -0.275 0.783188    
    ## Exterior1stBrkComm   -1.957e-01  8.420e-02  -2.324 0.020260 *  
    ## Exterior1stBrkFace    9.488e-02  3.114e-02   3.047 0.002356 ** 
    ## Exterior1stCBlock    -4.178e-02  1.125e-01  -0.371 0.710446    
    ## Exterior1stCemntBd    4.615e-02  3.240e-02   1.424 0.154558    
    ## Exterior1stHdBoard    2.199e-03  2.838e-02   0.077 0.938248    
    ## Exterior1stImStucc   -1.460e-02  1.101e-01  -0.133 0.894513    
    ## Exterior1stMetalSd    3.329e-02  2.753e-02   1.209 0.226819    
    ## Exterior1stPlywood   -4.406e-04  2.972e-02  -0.015 0.988173    
    ## Exterior1stStone     -1.065e-02  8.538e-02  -0.125 0.900719    
    ## Exterior1stStucco     2.994e-02  3.465e-02   0.864 0.387808    
    ## Exterior1stVinylSd    1.908e-02  2.774e-02   0.688 0.491733    
    ## Exterior1stWd Sdng    6.773e-04  2.749e-02   0.025 0.980345    
    ## Exterior1stWdShing    4.392e-03  3.429e-02   0.128 0.898101    
    ## FoundationCBlock      2.725e-02  1.349e-02   2.020 0.043630 *  
    ## FoundationPConc       4.272e-02  1.491e-02   2.865 0.004233 ** 
    ## FoundationSlab        7.069e-02  4.209e-02   1.679 0.093306 .  
    ## FoundationStone       1.188e-01  4.551e-02   2.611 0.009133 ** 
    ## FoundationWood       -1.040e-01  6.411e-02  -1.622 0.105090    
    ## BsmtQual              1.756e-02  7.714e-03   2.277 0.022960 *  
    ## BsmtCond             -1.453e-02  1.053e-02  -1.381 0.167646    
    ## BsmtExposure          1.496e-02  3.326e-03   4.498 7.44e-06 ***
    ## BsmtFinSF1            8.671e-03  1.333e-03   6.503 1.11e-10 ***
    ## TotalBsmtSF           1.287e-02  6.860e-03   1.876 0.060855 .  
    ## HeatingGasA           3.318e-02  1.123e-01   0.296 0.767618    
    ## HeatingGasW           1.097e-01  1.150e-01   0.954 0.340481    
    ## HeatingGrav          -4.714e-02  1.190e-01  -0.396 0.691971    
    ## HeatingOthW          -3.662e-02  1.372e-01  -0.267 0.789545    
    ## HeatingWall           7.076e-02  1.272e-01   0.556 0.578118    
    ## HeatingQC             1.487e-02  4.176e-03   3.560 0.000384 ***
    ## CentralAirY           4.148e-02  1.566e-02   2.649 0.008167 ** 
    ## X1stFlrSF             1.869e-01  3.379e-02   5.530 3.85e-08 ***
    ## X2ndFlrSF             8.538e-05  2.627e-05   3.250 0.001182 ** 
    ## GrLivArea             2.889e-01  4.046e-02   7.141 1.52e-12 ***
    ## BsmtFullBath          3.005e-02  7.304e-03   4.114 4.14e-05 ***
    ## BedroomAbvGr         -7.363e-03  5.210e-03  -1.413 0.157827    
    ## KitchenAbvGr         -8.229e-02  4.406e-02  -1.868 0.062005 .  
    ## KitchenQual           2.354e-02  6.866e-03   3.428 0.000626 ***
    ## Functional            3.894e-02  4.630e-03   8.411  < 2e-16 ***
    ## Fireplaces            1.888e-02  5.839e-03   3.233 0.001255 ** 
    ## GarageTypeAttchd      8.175e-02  4.718e-02   1.733 0.083388 .  
    ## GarageTypeBasment     6.287e-02  5.357e-02   1.174 0.240799    
    ## GarageTypeBuiltIn     7.704e-02  4.916e-02   1.567 0.117366    
    ## GarageTypeCarPort     3.440e-02  5.996e-02   0.574 0.566239    
    ## GarageTypeDetchd      9.549e-02  4.685e-02   2.038 0.041726 *  
    ## GarageTypeNoGarage    1.662e-01  6.154e-02   2.701 0.006998 ** 
    ## GarageFinish          7.962e-03  5.197e-03   1.532 0.125769    
    ## GarageCars            2.955e-02  9.821e-03   3.008 0.002675 ** 
    ## GarageArea            7.990e-05  3.310e-05   2.414 0.015931 *  
    ## GarageQual            2.160e-02  1.281e-02   1.686 0.092067 .  
    ## WoodDeckSF            7.143e-05  2.533e-05   2.820 0.004878 ** 
    ## EnclosedPorch         2.844e-03  1.947e-03   1.461 0.144268    
    ## ScreenPorch           9.048e-03  2.097e-03   4.315 1.72e-05 ***
    ## PoolQC                3.856e-02  1.181e-02   3.265 0.001124 ** 
    ## MoSold               -1.502e-03  1.058e-03  -1.419 0.156127    
    ## SaleTypeCon           6.394e-02  7.902e-02   0.809 0.418606    
    ## SaleTypeConLD         1.110e-01  4.173e-02   2.661 0.007879 ** 
    ## SaleTypeConLI        -1.010e-02  5.157e-02  -0.196 0.844714    
    ## SaleTypeConLw        -2.929e-03  5.228e-02  -0.056 0.955334    
    ## SaleTypeCWD           4.023e-02  5.716e-02   0.704 0.481691    
    ## SaleTypeNew           1.669e-01  6.779e-02   2.463 0.013920 *  
    ## SaleTypeOth           6.610e-02  6.493e-02   1.018 0.308842    
    ## SaleTypeWD           -1.304e-02  1.840e-02  -0.709 0.478736    
    ## SaleConditionAdjLand  1.422e-01  5.818e-02   2.445 0.014621 *  
    ## SaleConditionAlloca   5.488e-02  3.629e-02   1.512 0.130727    
    ## SaleConditionFamily   1.902e-02  2.702e-02   0.704 0.481631    
    ## SaleConditionNormal   6.907e-02  1.248e-02   5.534 3.76e-08 ***
    ## SaleConditionPartial -5.458e-02  6.543e-02  -0.834 0.404322    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1042 on 1330 degrees of freedom
    ##   (1459 observations deleted due to missingness)
    ## Multiple R-squared:  0.938,  Adjusted R-squared:  0.932 
    ## F-statistic:   156 on 129 and 1330 DF,  p-value: < 2.2e-16

Train&lt;-H\_Clean\[!is.na(H\_Clean$SalePrice),-1\]

``` r
StepF_Log$call
```

    ## lm(formula = SalePrice ~ OverallQual + GrLivArea + Neighborhood + 
    ##     MSSubClass + BsmtFinSF1 + OverallCond + GarageCars + YearBuilt + 
    ##     RoofMatl + LotArea + MSZoning + BsmtExposure + Condition2 + 
    ##     Functional + SaleCondition + KitchenQual + Condition1 + Exterior1st + 
    ##     BsmtFullBath + Fireplaces + BsmtQual + HeatingQC + PoolQC + 
    ##     ScreenPorch + Heating + LotConfig + GarageArea + Foundation + 
    ##     WoodDeckSF + CentralAir + SaleType + X1stFlrSF + X2ndFlrSF + 
    ##     Street + YearRemodAdd + MoSold + KitchenAbvGr + BedroomAbvGr + 
    ##     EnclosedPorch + TotRmsAbvGrd, data = H_Lo)

``` r
StepB_Log$call
```

    ## lm(formula = SalePrice ~ MSZoning + LotArea + Street + LotConfig + 
    ##     Neighborhood + Condition1 + Condition2 + BldgType + OverallQual + 
    ##     OverallCond + YearBuilt + YearRemodAdd + RoofMatl + Exterior1st + 
    ##     Foundation + BsmtQual + BsmtCond + BsmtExposure + BsmtFinSF1 + 
    ##     TotalBsmtSF + Heating + HeatingQC + CentralAir + X1stFlrSF + 
    ##     X2ndFlrSF + GrLivArea + BsmtFullBath + BedroomAbvGr + KitchenAbvGr + 
    ##     KitchenQual + Functional + Fireplaces + GarageType + GarageFinish + 
    ##     GarageCars + GarageArea + GarageQual + WoodDeckSF + EnclosedPorch + 
    ##     ScreenPorch + PoolQC + MoSold + SaleType + SaleCondition, 
    ##     data = H_Lo)

``` r
StepS_Log$call
```

    ## lm(formula = SalePrice ~ MSZoning + LotArea + Street + LotConfig + 
    ##     Neighborhood + Condition1 + Condition2 + BldgType + OverallQual + 
    ##     OverallCond + YearBuilt + YearRemodAdd + RoofMatl + Exterior1st + 
    ##     Foundation + BsmtQual + BsmtCond + BsmtExposure + BsmtFinSF1 + 
    ##     TotalBsmtSF + Heating + HeatingQC + CentralAir + X1stFlrSF + 
    ##     X2ndFlrSF + GrLivArea + BsmtFullBath + BedroomAbvGr + KitchenAbvGr + 
    ##     KitchenQual + Functional + Fireplaces + GarageType + GarageFinish + 
    ##     GarageCars + GarageArea + GarageQual + WoodDeckSF + EnclosedPorch + 
    ##     ScreenPorch + PoolQC + MoSold + SaleType + SaleCondition, 
    ##     data = H_Lo)

``` r
#summary(StepB_Log)
#summary(StepS_Log)
identical(StepB_Log$call, StepS_Log$call) # Shows they are same.
```

    ## [1] TRUE

``` r
identical(StepF_Log$call, StepS_Log$call)
```

    ## [1] FALSE

``` r
a<-(lm(SalePrice~.,data=H_Lo[,-1])) # Removing ID 
b<-lm(StepF_Log$call,data=H_Lo[!is.na(H_Lo$SalePrice),-1])
c<-lm(StepS_Log$call,data=H_Lo[!is.na(H_Lo$SalePrice),-1])
par(mfrow=c(2,2))
plot(a, sub='ALL Var')
```

    ## Warning: not plotting observations with leverage one:
    ##   121, 272, 347, 399, 584, 596, 1004, 1012, 1188, 1231, 1271, 1276, 1299, 1322, 1371, 1387

    ## Warning: not plotting observations with leverage one:
    ##   121, 272, 347, 399, 584, 596, 1004, 1012, 1188, 1231, 1271, 1276, 1299, 1322, 1371, 1387

![](136_Liz_Project_Step5_Tune_Log_files/figure-markdown_github/unnamed-chunk-12-1.png)

``` r
plot(b, sub='Foward Sel')
```

    ## Warning: not plotting observations with leverage one:
    ##   121, 272, 584, 1004, 1012, 1188, 1231, 1276, 1299, 1322, 1371

    ## Warning: not plotting observations with leverage one:
    ##   121, 272, 584, 1004, 1012, 1188, 1231, 1276, 1299, 1322, 1371

![](136_Liz_Project_Step5_Tune_Log_files/figure-markdown_github/unnamed-chunk-12-2.png)

``` r
plot(c, sub='Backwards El')
```

    ## Warning: not plotting observations with leverage one:
    ##   121, 272, 584, 1004, 1012, 1188, 1231, 1276, 1299, 1322, 1371

    ## Warning: not plotting observations with leverage one:
    ##   121, 272, 584, 1004, 1012, 1188, 1231, 1276, 1299, 1322, 1371

![](136_Liz_Project_Step5_Tune_Log_files/figure-markdown_github/unnamed-chunk-12-3.png)

``` r
plot(a,which=c(4))
plot(b,which=c(4))


#pdtrain<-predict(a,newdata=H_Lo[!is.na(H_Lo$SalePrice),])
pd_Log_LM_All<-predict(a,newdata=H_Lo[is.na(H_Lo$SalePrice),])
```

    ## Warning in predict.lm(a, newdata = H_Lo[is.na(H_Lo$SalePrice), ]):
    ## prediction from a rank-deficient fit may be misleading

``` r
pd_Log_LM_f2<-predict(b,newdata=H_Lo[is.na(H_Lo$SalePrice),])
pd_Log_LM_f3<-predict(c,newdata=H_Lo[is.na(H_Lo$SalePrice),])
#trainSP<-H_Lo$SalePrice[!is.na(H_Lo$SalePrice)]


#plot(exp(trainSP),exp(pdtrain), col='blue', ylab='Prediction')
#points(actual,exp(pdtest), col='red')
#abline(b=1,a=0)
#RMSE(exp(trainSP),exp(pdtrain))
#RMSE(actual,exp(pdtest))

print("RMSLE for All Var - Log")
```

    ## [1] "RMSLE for All Var - Log"

``` r
RMSE(pd_Log_LM_All,log(actual))
```

    ## [1] 0.1370111

``` r
RMSE(pd_Log_LM_f2,log(actual))
```

    ## [1] 0.1332974

``` r
RMSE(pd_Log_LM_f3,log(actual))
```

    ## [1] 0.1329731

![](136_Liz_Project_Step5_Tune_Log_files/figure-markdown_github/unnamed-chunk-12-4.png) Try without outliers

``` r
summary(a)
```

    ## 
    ## Call:
    ## lm(formula = SalePrice ~ ., data = H_Lo[, -1])
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.68026 -0.04655  0.00146  0.05276  0.51426 
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               5.045e+00  4.694e+00   1.075 0.282697    
    ## MSSubClass1NHFin          6.061e-02  7.208e-02   0.841 0.400630    
    ## MSSubClass1NHUnf         -1.313e-01  1.207e-01  -1.088 0.276777    
    ## MSSubClass1SFA           -7.218e-02  1.067e-01  -0.677 0.498804    
    ## MSSubClass1SNEW           6.346e-02  6.909e-02   0.919 0.358476    
    ## MSSubClass1SOLD           2.001e-02  7.332e-02   0.273 0.785010    
    ## MSSubClass1SPUD           5.498e-02  4.329e-02   1.270 0.204349    
    ## MSSubClass2FCONV          1.188e-02  6.015e-02   0.197 0.843524    
    ## MSSubClass2NHS           -4.697e-02  8.806e-02  -0.533 0.593873    
    ## MSSubClass2SNEW           1.709e-02  6.047e-02   0.283 0.777533    
    ## MSSubClass2SOLD           5.412e-02  6.457e-02   0.838 0.402047    
    ## MSSubClassDUPL            5.983e-02  5.634e-02   1.062 0.288516    
    ## MSSubClassMLPUD           1.071e-01  6.253e-02   1.713 0.086924 .  
    ## MSSubClassSPL             8.540e-02  8.143e-02   1.049 0.294509    
    ## MSSubClassSPLF            6.841e-02  8.237e-02   0.831 0.406356    
    ## MSZoningFV                4.694e-01  5.489e-02   8.552  < 2e-16 ***
    ## MSZoningRH                4.325e-01  5.463e-02   7.918 5.29e-15 ***
    ## MSZoningRL                4.278e-01  4.666e-02   9.168  < 2e-16 ***
    ## MSZoningRM                4.016e-01  4.405e-02   9.118  < 2e-16 ***
    ## LotFrontage               1.979e-02  1.643e-02   1.204 0.228700    
    ## LotArea                   8.276e-02  1.249e-02   6.627 5.09e-11 ***
    ## StreetPave                6.791e-02  5.247e-02   1.294 0.195836    
    ## AlleyNoAlley              1.468e-03  1.920e-02   0.076 0.939060    
    ## AlleyPave                 4.284e-02  2.778e-02   1.542 0.123293    
    ## LotShapeIR2               2.858e-02  1.901e-02   1.504 0.132943    
    ## LotShapeIR3               3.039e-02  3.902e-02   0.779 0.436207    
    ## LotShapeReg               1.324e-02  7.312e-03   1.811 0.070396 .  
    ## LandContourHLS            3.097e-02  2.331e-02   1.329 0.184176    
    ## LandContourLow           -9.416e-03  2.849e-02  -0.330 0.741115    
    ## LandContourLvl            2.448e-02  1.673e-02   1.464 0.143557    
    ## LotConfigCulDSac          3.067e-02  1.681e-02   1.824 0.068329 .  
    ## LotConfigFR2             -3.214e-02  1.847e-02  -1.740 0.082044 .  
    ## LotConfigFR3             -1.102e-01  5.736e-02  -1.922 0.054888 .  
    ## LotConfigInside          -9.307e-03  8.187e-03  -1.137 0.255826    
    ## LandSlope                -3.458e-03  1.515e-02  -0.228 0.819432    
    ## NeighborhoodBlueste       7.960e-02  9.032e-02   0.881 0.378329    
    ## NeighborhoodBrDale        4.698e-02  5.376e-02   0.874 0.382381    
    ## NeighborhoodBrkSide       2.687e-02  4.370e-02   0.615 0.538689    
    ## NeighborhoodClearCr       6.686e-03  4.327e-02   0.155 0.877213    
    ## NeighborhoodCollgCr      -3.654e-02  3.453e-02  -1.058 0.290132    
    ## NeighborhoodCrawfor       9.715e-02  4.042e-02   2.404 0.016384 *  
    ## NeighborhoodEdwards      -7.962e-02  3.783e-02  -2.105 0.035533 *  
    ## NeighborhoodGilbert      -3.045e-02  3.650e-02  -0.834 0.404298    
    ## NeighborhoodIDOTRR       -2.647e-02  4.974e-02  -0.532 0.594721    
    ## NeighborhoodMeadowV      -9.359e-02  5.660e-02  -1.653 0.098498 .  
    ## NeighborhoodMitchel      -5.890e-02  3.853e-02  -1.529 0.126633    
    ## NeighborhoodNAmes        -4.531e-02  3.682e-02  -1.231 0.218686    
    ## NeighborhoodNoRidge       5.122e-02  3.942e-02   1.299 0.194065    
    ## NeighborhoodNPkVill       3.788e-02  6.369e-02   0.595 0.552099    
    ## NeighborhoodNridgHt       8.645e-02  3.507e-02   2.465 0.013824 *  
    ## NeighborhoodNWAmes       -4.949e-02  3.793e-02  -1.305 0.192237    
    ## NeighborhoodOldTown      -3.603e-02  4.479e-02  -0.804 0.421326    
    ## NeighborhoodSawyer       -2.729e-02  3.811e-02  -0.716 0.474036    
    ## NeighborhoodSawyerW      -2.387e-02  3.694e-02  -0.646 0.518245    
    ## NeighborhoodSomerst      -2.591e-03  4.214e-02  -0.061 0.950988    
    ## NeighborhoodStoneBr       1.345e-01  3.866e-02   3.479 0.000520 ***
    ## NeighborhoodSWISU        -2.291e-03  4.487e-02  -0.051 0.959291    
    ## NeighborhoodTimber       -2.385e-02  3.857e-02  -0.618 0.536503    
    ## NeighborhoodVeenker       8.173e-03  4.887e-02   0.167 0.867210    
    ## Condition1Feedr           3.225e-02  2.255e-02   1.430 0.152880    
    ## Condition1Norm            8.463e-02  1.867e-02   4.533 6.37e-06 ***
    ## Condition1PosA            4.075e-02  4.515e-02   0.902 0.366968    
    ## Condition1PosN            8.476e-02  3.359e-02   2.523 0.011749 *  
    ## Condition1RRAe           -5.133e-02  3.956e-02  -1.298 0.194614    
    ## Condition1RRAn            6.193e-02  3.132e-02   1.978 0.048197 *  
    ## Condition1RRNe            2.015e-02  8.018e-02   0.251 0.801652    
    ## Condition1RRNn            1.015e-01  5.828e-02   1.742 0.081715 .  
    ## Condition2Feedr           1.278e-01  1.147e-01   1.114 0.265522    
    ## Condition2Norm            9.933e-02  1.013e-01   0.981 0.326833    
    ## Condition2PosA            3.922e-01  1.560e-01   2.515 0.012042 *  
    ## Condition2PosN           -5.678e-01  1.316e-01  -4.316 1.72e-05 ***
    ## Condition2RRAe           -2.522e-01  2.436e-01  -1.035 0.300747    
    ## Condition2RRAn            5.207e-02  1.491e-01   0.349 0.726941    
    ## Condition2RRNn            1.558e-01  1.298e-01   1.201 0.230107    
    ## BldgType                  8.717e-03  1.612e-02   0.541 0.588711    
    ## HouseStyle1.5Unf          2.366e-01  1.022e-01   2.314 0.020824 *  
    ## HouseStyle1Story          3.363e-02  4.043e-02   0.832 0.405638    
    ## HouseStyle2.5Fin          4.991e-02  7.464e-02   0.669 0.503837    
    ## HouseStyle2.5Unf          1.161e-01  7.118e-02   1.631 0.103239    
    ## HouseStyle2Story          1.420e-02  3.683e-02   0.386 0.699812    
    ## HouseStyleSFoyer          5.348e-03  5.451e-02   0.098 0.921856    
    ## HouseStyleSLvl           -1.921e-02  5.903e-02  -0.326 0.744857    
    ## OverallQual               4.177e-02  4.630e-03   9.020  < 2e-16 ***
    ## OverallCond               3.694e-02  3.918e-03   9.428  < 2e-16 ***
    ## YearBuilt                 2.249e-03  3.835e-04   5.864 5.79e-09 ***
    ## YearRemodAdd              4.965e-04  2.458e-04   2.020 0.043632 *  
    ## RoofStyleGable            2.890e-03  8.218e-02   0.035 0.971956    
    ## RoofStyleGambrel         -4.201e-02  8.991e-02  -0.467 0.640409    
    ## RoofStyleHip              1.549e-02  8.236e-02   0.188 0.850879    
    ## RoofStyleMansard          2.848e-02  9.545e-02   0.298 0.765451    
    ## RoofStyleShed             2.467e-01  1.694e-01   1.456 0.145517    
    ## RoofMatlCompShg           1.714e+00  1.355e-01  12.646  < 2e-16 ***
    ## RoofMatlMembran           1.900e+00  1.941e-01   9.787  < 2e-16 ***
    ## RoofMatlMetal             1.780e+00  1.969e-01   9.044  < 2e-16 ***
    ## RoofMatlRoll              1.641e+00  1.772e-01   9.258  < 2e-16 ***
    ## RoofMatlTar&Grv           1.699e+00  1.589e-01  10.689  < 2e-16 ***
    ## RoofMatlWdShake           1.675e+00  1.516e-01  11.051  < 2e-16 ***
    ## RoofMatlWdShngl           1.892e+00  1.441e-01  13.134  < 2e-16 ***
    ## Exterior1stAsphShn       -1.211e-01  1.503e-01  -0.806 0.420531    
    ## Exterior1stBrkComm       -3.295e-01  1.263e-01  -2.609 0.009187 ** 
    ## Exterior1stBrkFace        5.809e-02  5.792e-02   1.003 0.316019    
    ## Exterior1stCBlock        -5.025e-02  1.157e-01  -0.434 0.664226    
    ## Exterior1stCemntBd       -8.163e-02  8.568e-02  -0.953 0.340879    
    ## Exterior1stHdBoard       -4.194e-02  5.844e-02  -0.718 0.473059    
    ## Exterior1stImStucc       -8.661e-02  1.277e-01  -0.678 0.497893    
    ## Exterior1stMetalSd        5.858e-03  6.646e-02   0.088 0.929783    
    ## Exterior1stPlywood       -3.338e-02  5.767e-02  -0.579 0.562823    
    ## Exterior1stStone         -3.940e-02  1.078e-01  -0.365 0.714858    
    ## Exterior1stStucco         9.675e-03  6.346e-02   0.152 0.878846    
    ## Exterior1stVinylSd       -1.712e-02  6.114e-02  -0.280 0.779467    
    ## Exterior1stWd Sdng       -7.148e-02  5.622e-02  -1.271 0.203821    
    ## Exterior1stWdShing       -1.715e-02  6.077e-02  -0.282 0.777859    
    ## Exterior2ndAsphShn        1.059e-01  1.009e-01   1.050 0.293947    
    ## Exterior2ndBrk Cmn        1.363e-01  9.321e-02   1.463 0.143821    
    ## Exterior2ndBrkFace        1.756e-02  5.921e-02   0.297 0.766811    
    ## Exterior2ndCBlock                NA         NA      NA       NA    
    ## Exterior2ndCmentBd        1.363e-01  8.397e-02   1.623 0.104736    
    ## Exterior2ndHdBoard        4.978e-02  5.552e-02   0.896 0.370162    
    ## Exterior2ndImStucc        8.157e-02  6.447e-02   1.265 0.206000    
    ## Exterior2ndMetalSd        3.442e-02  6.442e-02   0.534 0.593282    
    ## Exterior2ndOther         -8.459e-02  1.242e-01  -0.681 0.495886    
    ## Exterior2ndPlywood        4.146e-02  5.393e-02   0.769 0.442213    
    ## Exterior2ndStone          3.832e-02  7.740e-02   0.495 0.620651    
    ## Exterior2ndStucco         1.323e-02  6.150e-02   0.215 0.829721    
    ## Exterior2ndVinylSd        4.257e-02  5.855e-02   0.727 0.467279    
    ## Exterior2ndWd Sdng        8.478e-02  5.361e-02   1.582 0.114012    
    ## Exterior2ndWd Shng        3.354e-02  5.625e-02   0.596 0.551135    
    ## MasVnrTypeBrkFace         4.209e-02  3.009e-02   1.399 0.162109    
    ## MasVnrTypeNone            4.889e-02  4.058e-02   1.205 0.228601    
    ## MasVnrTypeStone           6.157e-02  3.195e-02   1.927 0.054200 .  
    ## MasVnrArea                2.780e-03  5.369e-03   0.518 0.604741    
    ## ExterQual                 3.594e-03  9.547e-03   0.376 0.706676    
    ## ExterCond                -1.262e-02  9.546e-03  -1.323 0.186220    
    ## FoundationCBlock          2.025e-02  1.442e-02   1.405 0.160326    
    ## FoundationPConc           4.002e-02  1.551e-02   2.579 0.010010 *  
    ## FoundationSlab            5.843e-02  4.495e-02   1.300 0.193867    
    ## FoundationStone           9.900e-02  4.985e-02   1.986 0.047267 *  
    ## FoundationWood           -1.139e-01  6.708e-02  -1.698 0.089741 .  
    ## BsmtQual                  9.915e-03  8.215e-03   1.207 0.227663    
    ## BsmtCond                 -7.024e-03  1.121e-02  -0.626 0.531206    
    ## BsmtExposure              1.614e-02  3.806e-03   4.240 2.40e-05 ***
    ## BsmtFinType1              1.486e-03  3.374e-03   0.440 0.659694    
    ## BsmtFinSF1                6.039e-03  2.952e-03   2.046 0.040980 *  
    ## BsmtFinType2             -2.037e-05  7.724e-03  -0.003 0.997896    
    ## BsmtFinSF2               -1.454e-03  3.720e-03  -0.391 0.695836    
    ## BsmtUnfSF                -2.071e-05  1.543e-05  -1.342 0.179827    
    ## TotalBsmtSF               1.790e-02  8.140e-03   2.199 0.028066 *  
    ## HeatingGasA               2.290e-02  1.161e-01   0.197 0.843624    
    ## HeatingGasW               9.842e-02  1.193e-01   0.825 0.409715    
    ## HeatingGrav              -8.648e-02  1.244e-01  -0.695 0.487066    
    ## HeatingOthW              -7.868e-02  1.424e-01  -0.553 0.580624    
    ## HeatingWall               7.320e-02  1.344e-01   0.545 0.586072    
    ## HeatingQC                 1.348e-02  4.350e-03   3.098 0.001994 ** 
    ## CentralAirY               4.521e-02  1.738e-02   2.601 0.009410 ** 
    ## ElectricalFuseF          -1.296e-02  2.607e-02  -0.497 0.619115    
    ## ElectricalFuseP          -1.741e-02  7.689e-02  -0.226 0.820897    
    ## ElectricalMix            -2.585e-02  1.181e-01  -0.219 0.826813    
    ## ElectricalSBrkr          -1.497e-02  1.345e-02  -1.113 0.265852    
    ## X1stFlrSF                 1.326e-01  4.739e-02   2.799 0.005210 ** 
    ## X2ndFlrSF                 1.069e-04  3.324e-05   3.216 0.001335 ** 
    ## LowQualFinSF             -7.406e-04  5.479e-03  -0.135 0.892503    
    ## GrLivArea                 3.040e-01  5.657e-02   5.375 9.14e-08 ***
    ## BsmtFullBath              2.883e-02  8.697e-03   3.315 0.000943 ***
    ## BsmtHalfBath              5.678e-03  1.978e-02   0.287 0.774128    
    ## BedroomAbvGr             -9.380e-03  6.234e-03  -1.505 0.132655    
    ## KitchenAbvGr             -1.028e-01  6.811e-02  -1.510 0.131285    
    ## KitchenQual               2.344e-02  7.389e-03   3.172 0.001548 ** 
    ## TotRmsAbvGrd              6.193e-03  4.218e-03   1.468 0.142326    
    ## Functional                3.501e-02  5.114e-03   6.845 1.20e-11 ***
    ## Fireplaces                1.933e-02  9.868e-03   1.959 0.050374 .  
    ## FireplaceQu               4.258e-04  3.601e-03   0.118 0.905888    
    ## GarageTypeAttchd          1.003e-01  4.989e-02   2.010 0.044610 *  
    ## GarageTypeBasment         9.076e-02  5.777e-02   1.571 0.116432    
    ## GarageTypeBuiltIn         8.957e-02  5.216e-02   1.717 0.086176 .  
    ## GarageTypeCarPort         9.420e-02  6.504e-02   1.448 0.147753    
    ## GarageTypeDetchd          1.154e-01  4.978e-02   2.318 0.020602 *  
    ## GarageTypeNoGarage        2.227e-01  7.035e-02   3.165 0.001588 ** 
    ## GarageYrBlt              -9.914e-05  2.696e-04  -0.368 0.713165    
    ## GarageFinish              8.479e-03  5.456e-03   1.554 0.120399    
    ## GarageCars                2.620e-02  1.015e-02   2.582 0.009926 ** 
    ## GarageArea                9.076e-05  3.551e-05   2.556 0.010699 *  
    ## GarageQual                2.411e-02  1.738e-02   1.387 0.165549    
    ## GarageCond                7.311e-03  1.804e-02   0.405 0.685407    
    ## PavedDrive                6.472e-03  7.530e-03   0.859 0.390252    
    ## WoodDeckSF                8.369e-05  2.663e-05   3.142 0.001717 ** 
    ## OpenPorchSF               2.631e-03  1.730e-03   1.520 0.128703    
    ## EnclosedPorch             4.039e-03  2.060e-03   1.961 0.050101 .  
    ## X3SsnPorch                4.668e-03  4.584e-03   1.018 0.308691    
    ## ScreenPorch               9.678e-03  2.184e-03   4.432 1.01e-05 ***
    ## PoolArea                 -2.250e-03  2.855e-02  -0.079 0.937196    
    ## PoolQC                    4.981e-02  4.553e-02   1.094 0.274195    
    ## FenceGdWo                -2.056e-02  2.210e-02  -0.930 0.352360    
    ## FenceMnPrv                1.602e-02  1.808e-02   0.886 0.375750    
    ## FenceMnWw                 3.348e-03  3.734e-02   0.090 0.928572    
    ## FenceNoFence              1.605e-02  1.645e-02   0.975 0.329506    
    ## MiscFeatureNoMiscFeature -1.455e-01  1.686e-01  -0.863 0.388488    
    ## MiscFeatureOthr          -6.815e-02  1.582e-01  -0.431 0.666713    
    ## MiscFeatureShed          -6.949e-02  1.171e-01  -0.594 0.552948    
    ## MiscFeatureTenC          -1.997e-01  1.842e-01  -1.084 0.278702    
    ## MiscVal                  -1.235e-02  1.352e-02  -0.914 0.361156    
    ## MoSold                   -1.451e-03  1.104e-03  -1.314 0.189066    
    ## YrSold                   -3.046e-03  2.302e-03  -1.324 0.185908    
    ## SaleTypeCon               8.844e-02  8.028e-02   1.102 0.270800    
    ## SaleTypeConLD             1.359e-01  4.421e-02   3.074 0.002158 ** 
    ## SaleTypeConLI            -2.262e-03  5.253e-02  -0.043 0.965660    
    ## SaleTypeConLw             4.906e-04  5.411e-02   0.009 0.992767    
    ## SaleTypeCWD               6.305e-02  5.841e-02   1.079 0.280630    
    ## SaleTypeNew               1.635e-01  6.919e-02   2.363 0.018278 *  
    ## SaleTypeOth               2.697e-02  7.287e-02   0.370 0.711320    
    ## SaleTypeWD               -8.569e-03  1.893e-02  -0.453 0.650903    
    ## SaleConditionAdjLand      1.339e-01  6.659e-02   2.011 0.044567 *  
    ## SaleConditionAlloca       5.174e-02  3.924e-02   1.319 0.187542    
    ## SaleConditionFamily       5.733e-03  2.773e-02   0.207 0.836234    
    ## SaleConditionNormal       6.415e-02  1.303e-02   4.923 9.69e-07 ***
    ## SaleConditionPartial     -6.213e-02  6.650e-02  -0.934 0.350301    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1041 on 1246 degrees of freedom
    ##   (1459 observations deleted due to missingness)
    ## Multiple R-squared:  0.942,  Adjusted R-squared:  0.9321 
    ## F-statistic:    95 on 213 and 1246 DF,  p-value: < 2.2e-16

``` r
summary(H_Lo$Exterior2nd)
```

    ## AsbShng AsphShn Brk Cmn BrkFace  CBlock CmentBd HdBoard ImStucc MetalSd 
    ##      38       4      22      47       3     126     406      15     447 
    ##   Other Plywood   Stone  Stucco VinylSd Wd Sdng Wd Shng 
    ##       1     270       6      47    1015     391      81

Remove Outlier and check

``` r
a_o<-(lm(SalePrice~.,data=H_Lo[-c(524,1299),-1])) # Removing ID 
b_o<-lm(StepF_Log$call,data=H_Lo[-c(524,1299),-1])
c_o<-lm(StepS_Log$call,data=H_Lo[-c(524,1299),-1])
par(mfrow=c(2,2))
plot(a_o, main='ALL Var')
```

    ## Warning: not plotting observations with leverage one:
    ##   121, 347, 399, 583, 595, 825, 1003, 1011, 1187, 1230, 1270, 1275, 1320, 1369, 1385

    ## Warning: not plotting observations with leverage one:
    ##   121, 347, 399, 583, 595, 825, 1003, 1011, 1187, 1230, 1270, 1275, 1320, 1369, 1385

    ## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced

    ## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced

![](136_Liz_Project_Step5_Tune_Log_files/figure-markdown_github/unnamed-chunk-14-1.png)

``` r
plot(b_o, main='Foward Sel')
```

    ## Warning: not plotting observations with leverage one:
    ##   272, 583, 825, 1003, 1011, 1187, 1230, 1275, 1320, 1369

    ## Warning: not plotting observations with leverage one:
    ##   272, 583, 825, 1003, 1011, 1187, 1230, 1275, 1320, 1369

    ## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced

    ## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced

![](136_Liz_Project_Step5_Tune_Log_files/figure-markdown_github/unnamed-chunk-14-2.png)

``` r
plot(c_o, main='Backwards El')
```

    ## Warning: not plotting observations with leverage one:
    ##   121, 272, 825, 1003, 1011, 1187, 1230, 1275, 1320, 1369

    ## Warning: not plotting observations with leverage one:
    ##   121, 272, 825, 1003, 1011, 1187, 1230, 1275, 1320, 1369

    ## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced

    ## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced

![](136_Liz_Project_Step5_Tune_Log_files/figure-markdown_github/unnamed-chunk-14-3.png)

``` r
plot(a_o,which=c(4))
plot(b_o,which=c(4))
plot(c_o,which=c(4))

#pdtrain<-predict(a,newdata=H_Lo[!is.na(H_Lo$SalePrice),])
pd_Log_LM_All_o<-predict(a_o,newdata=H_Lo[is.na(H_Lo$SalePrice),])
```

    ## Warning in predict.lm(a_o, newdata = H_Lo[is.na(H_Lo$SalePrice), ]):
    ## prediction from a rank-deficient fit may be misleading

``` r
pd_Log_LM_f2_o<-predict(b_o,newdata=H_Lo[is.na(H_Lo$SalePrice),])
pd_Log_LM_f3_o<-predict(c_o,newdata=H_Lo[is.na(H_Lo$SalePrice),])
#trainSP<-H_Lo$SalePrice[!is.na(H_Lo$SalePrice)]


#plot(exp(trainSP),exp(pdtrain), col='blue', ylab='Prediction')
#points(actual,exp(pdtest), col='red')
#abline(b=1,a=0)
#RMSE(exp(trainSP),exp(pdtrain))
#RMSE(actual,exp(pdtest))

print("RMSLE for All Var - Log")
```

    ## [1] "RMSLE for All Var - Log"

``` r
RMSE(pd_Log_LM_All_o,log(actual))
```

    ## [1] 0.1341367

``` r
RMSE(pd_Log_LM_f2_o,log(actual))
```

    ## [1] 0.1302135

``` r
RMSE(pd_Log_LM_f3_o,log(actual))
```

    ## [1] 0.1298088

![](136_Liz_Project_Step5_Tune_Log_files/figure-markdown_github/unnamed-chunk-14-4.png) \#\#\#\#\# Lasso Applied - Only done on all variables for Lasso/Ridge/Elasticnet

``` r
tc<-trainControl(method="cv", number=10)
#rainset -DummyVariables
H_DummyLo_Train<-  as.data.frame(model.matrix(~.-1,data=H_Lo[1:1460,-1])) # Remove ID

#TestSet  -DummyVariables
H_DummyLo_Test<-  as.data.frame(model.matrix(~.-1,data=H_Lo[1461:2919,-c(1,78)])) # RemoveID, salePrice
  #as.data.frame

dim(H_DummyLo_Train)
```

    ## [1] 1460  216

``` r
set.seed(12334)

myLasso1Log <-train(SalePrice~.,
                 data = H_DummyLo_Train
                 ,method='glmnet', 
                 tuneGrid=expand.grid(alpha=1,lambda=seq(0.0001,0.01,length=51)), trControl=tc)
myLasso2Log <-train(SalePrice~., data = H_DummyLo_Train[-c(524,1299),]
  ,method='glmnet',tuneGrid=expand.grid(alpha=1,lambda=seq(0.0001,0.01,length=51)), trControl=tc)
#myLasso3 <-train(f3, data = H_Dummy_Train
#  ,method='glmnet',tuneGrid=expand.grid(alpha=1,lambda=seq(500,1500,length=50)), trControl=tc)
#myLasso4 <-train(f4, data = H_Dummy_Train
 # ,method='glmnet',tuneGrid=expand.grid(alpha=1,lambda=seq(500,1500,length=50)), trControl=tc)

sum(is.na(H_DummyLo_Train))
```

    ## [1] 0

Cannot find what this error means - There were missing values in resampled performance measures.There were missing values in resampled performance measures This was due to my high value of grid for Measures. Once I reduced to appropriate numbers, it is okay.

``` r
set.seed(12334)

plot(myLasso1Log, main='ALL')
```

![](136_Liz_Project_Step5_Tune_Log_files/figure-markdown_github/unnamed-chunk-16-1.png)

``` r
plot(myLasso2Log, main='Outlier Removed')
```

![](136_Liz_Project_Step5_Tune_Log_files/figure-markdown_github/unnamed-chunk-16-2.png)

``` r
plot(varImp(myLasso1Log), Scale=F, top=30)
```

![](136_Liz_Project_Step5_Tune_Log_files/figure-markdown_github/unnamed-chunk-16-3.png)

``` r
plot(varImp(myLasso2Log), Scale=F ,top=30)
```

![](136_Liz_Project_Step5_Tune_Log_files/figure-markdown_github/unnamed-chunk-16-4.png)

``` r
varImp(myLasso1Log)
```

    ## glmnet variable importance
    ## 
    ##   only 20 most important variables shown (out of 215)
    ## 
    ##                     Overall
    ## Condition2PosN       100.00
    ## GrLivArea             91.41
    ## NeighborhoodStoneBr   25.92
    ## RoofMatlWdShngl       24.90
    ## NeighborhoodCrawfor   22.51
    ## NeighborhoodNridgHt   21.91
    ## Exterior1stBrkComm    21.87
    ## NeighborhoodNoRidge   18.85
    ## KitchenAbvGr          17.92
    ## X1stFlrSF             17.91
    ## SaleTypeNew           17.76
    ## NeighborhoodIDOTRR    17.18
    ## LotArea               16.82
    ## Condition2PosA        15.04
    ## LotShapeIR3           14.22
    ## OverallQual           13.63
    ## Exterior1stBrkFace    12.98
    ## GarageCars            11.36
    ## HeatingGrav           10.75
    ## NeighborhoodSomerst   10.68

``` r
varImp(myLasso2Log)
```

    ## glmnet variable importance
    ## 
    ##   only 20 most important variables shown (out of 215)
    ## 
    ##                     Overall
    ## GrLivArea            100.00
    ## X1stFlrSF             48.17
    ## Exterior1stBrkComm    45.01
    ## KitchenAbvGr          35.15
    ## RoofMatlWdShngl       34.96
    ## NeighborhoodStoneBr   34.88
    ## NeighborhoodCrawfor   34.88
    ## SaleTypeNew           32.76
    ## Condition2PosA        30.97
    ## StreetPave            27.42
    ## LotArea               24.05
    ## NeighborhoodNridgHt   23.10
    ## HeatingGrav           22.77
    ## NeighborhoodNoRidge   20.29
    ## Exterior1stBrkFace    19.75
    ## HeatingOthW           18.55
    ## NeighborhoodIDOTRR    18.36
    ## FoundationWood        17.93
    ## OverallQual           17.34
    ## SaleConditionNormal   15.58

``` r
myLasso1Log$bestTune
```

    ##    alpha   lambda
    ## 23     1 0.004456

``` r
myLasso2Log$bestTune
```

    ##    alpha   lambda
    ## 14     1 0.002674

``` r
#myLasso3$bestTune
#myLasso4$bestTune

plot(myLasso1Log$finalModel, label=T)
```

![](136_Liz_Project_Step5_Tune_Log_files/figure-markdown_github/unnamed-chunk-16-5.png)

``` r
plot(myLasso2Log$finalModel ,label=T)
```

![](136_Liz_Project_Step5_Tune_Log_files/figure-markdown_github/unnamed-chunk-16-6.png)

``` r
#plot(myLasso3$finalModel)
#plot(myLasso4$finalModel)
```

Predict and Measure RMSE of Log

``` r
PdmyLasso1Log<-predict(myLasso1Log, newdata=H_DummyLo_Test)
PdmyLasso2Log<-predict(myLasso2Log, newdata=H_DummyLo_Test)
     

RMSE(PdmyLasso1Log,log(actual))
```

    ## [1] 0.1269534

``` r
RMSE(PdmyLasso2Log,log(actual))
```

    ## [1] 0.1240513

##### Ridge of Log

``` r
set.seed(12334)
myRidge1Log <-train(SalePrice~.,
                  data = H_DummyLo_Train
                 ,method='glmnet', 
                 tuneGrid=expand.grid(alpha=0,lambda=seq(0.00001,0.08,length=51)), trControl=tc)
set.seed(12334)
myRidge2Log <-train(SalePrice~., data = H_DummyLo_Train[-c(524,1299),]
  ,method='glmnet',tuneGrid=expand.grid(alpha=0,lambda=seq(0.0001,0.08,length=51)), trControl=tc) # without outlier
```

``` r
par(mfrow=c(1,2))
plot(myRidge1Log, main='ALL')
```

![](136_Liz_Project_Step5_Tune_Log_files/figure-markdown_github/unnamed-chunk-19-1.png)

``` r
plot(myRidge2Log, main='Outlier Removed')
```

![](136_Liz_Project_Step5_Tune_Log_files/figure-markdown_github/unnamed-chunk-19-2.png)

``` r
plot(varImp(myRidge1Log), Scale=F, top=30,main='ALL')
```

![](136_Liz_Project_Step5_Tune_Log_files/figure-markdown_github/unnamed-chunk-19-3.png)

``` r
plot(varImp(myRidge2Log), Scale=F ,top=30, main='Outlier Removed')
```

![](136_Liz_Project_Step5_Tune_Log_files/figure-markdown_github/unnamed-chunk-19-4.png)

``` r
varImp(myRidge1Log,main='ALL')
```

    ## glmnet variable importance
    ## 
    ##   only 20 most important variables shown (out of 215)
    ## 
    ##                     Overall
    ## Condition2PosN       100.00
    ## RoofMatlWdShngl       70.32
    ## Condition2PosA        63.03
    ## RoofMatlMembran       56.51
    ## Exterior1stBrkComm    52.49
    ## GrLivArea             39.88
    ## RoofStyleShed         34.74
    ## RoofMatlCompShg       32.96
    ## NeighborhoodStoneBr   32.28
    ## X1stFlrSF             31.49
    ## RoofMatlMetal         30.78
    ## Condition2RRAe        28.40
    ## NeighborhoodMeadowV   28.37
    ## StreetPave            26.07
    ## `RoofMatlTar&Grv`     24.95
    ## NeighborhoodNridgHt   23.49
    ## HeatingGrav           23.39
    ## RoofMatlWdShake       23.26
    ## NeighborhoodCrawfor   22.83
    ## Condition2RRNn        22.63

``` r
varImp(myRidge2Log,main='Outlier Removed')
```

    ## glmnet variable importance
    ## 
    ##   only 20 most important variables shown (out of 215)
    ## 
    ##                      Overall
    ## Condition2PosA        100.00
    ## Exterior1stBrkComm     94.49
    ## GrLivArea              71.56
    ## X1stFlrSF              61.53
    ## Condition2RRAe         61.19
    ## RoofMatlWdShngl        59.65
    ## RoofStyleShed          58.50
    ## MiscFeatureTenC        56.01
    ## RoofMatlMembran        51.63
    ## NeighborhoodStoneBr    49.82
    ## StreetPave             44.72
    ## NeighborhoodMeadowV    39.47
    ## NeighborhoodCrawfor    39.15
    ## HeatingGrav            36.82
    ## SaleConditionAdjLand   35.80
    ## KitchenAbvGr           33.23
    ## SaleTypeCon            32.95
    ## Condition1RRNn         32.00
    ## Condition2RRNn         31.77
    ## Exterior1stBrkFace     31.38

``` r
myRidge1Log$bestTune
```

    ##    alpha    lambda
    ## 32     0 0.0496038

``` r
myRidge2Log$bestTune
```

    ##    alpha  lambda
    ## 21     0 0.03206

``` r
plot(myRidge1Log$finalModel,main='ALL')
plot(myRidge2Log$finalModel,main='Outlier Removed')
```

![](136_Liz_Project_Step5_Tune_Log_files/figure-markdown_github/unnamed-chunk-19-5.png)

``` r
PdmyRidge1Log<-predict(myRidge1Log, newdata=H_DummyLo_Test)
PdmyRidge2Log<-predict(myRidge2Log, newdata=H_DummyLo_Test)
     

RMSE(PdmyRidge1Log,log(actual))
```

    ## [1] 0.1315939

``` r
RMSE(PdmyRidge2Log,log(actual))
```

    ## [1] 0.128344

##### Elasticnet of Log

``` r
set.seed(12334)
myEla1Log<-train(SalePrice~.,data = H_DummyLo_Train,method='glmnet',tuneGrid=expand.grid(alpha=seq(0, 1,length=11),lambda=seq(0.0001,0.1,length=30)), trControl=tc)
set.seed(12334)
myEla2Log<-train(SalePrice~.,data = H_DummyLo_Train[-c(524,1299),],method='glmnet',tuneGrid=expand.grid(alpha=seq(0, 1,length=11),lambda=seq(0.0001,0.1,length=30)), trControl=tc)
```

``` r
par(mfrow=c(1,2))
plot(myEla1Log, main='ALL')
```

![](136_Liz_Project_Step5_Tune_Log_files/figure-markdown_github/unnamed-chunk-22-1.png)

``` r
plot(myEla2Log, main='Outlier Removed')
```

![](136_Liz_Project_Step5_Tune_Log_files/figure-markdown_github/unnamed-chunk-22-2.png)

``` r
#plot(varImp(myEla1Log),top=25)
#varImp(myEla1Log)
myEla1Log$bestTune
```

    ##    alpha     lambda
    ## 67   0.2 0.02076897

``` r
myEla2Log$bestTune
```

    ##    alpha     lambda
    ## 65   0.2 0.01387931

``` r
PdEla1Log<-predict(myEla1Log, newdata=H_DummyLo_Test)
PdEla2Log<-predict(myEla2Log, newdata=H_DummyLo_Test)
RMSE(PdEla1Log, log(actual))
```

    ## [1] 0.1276619

``` r
RMSE(PdEla2Log, log(actual))
```

    ## [1] 0.1246807

Origial Lasso/Ridge/Elastic - removing outlier didn't help and actually gave worse results (barely) but once log transformed, it improved the results although little.

##### Random Forest for Log

Do a quick tuning.

``` r
set.seed(100)
tuneRF10<-tuneRF(H_Lo[1:1460,-c(1,78)],H_Lo[1:1460,78], mtryStart=16,StepFactor=1 ,improve=0.0001, Trace=T,Plot=T,ntreeTry=500, doBest = T)
```

    ## mtry = 16  OOB error = 0.01840473 
    ## Searching left ...
    ## mtry = 8     OOB error = 0.01920318 
    ## -0.04338288 1e-04 
    ## Searching right ...
    ## mtry = 32    OOB error = 0.01815909 
    ## 0.01334663 1e-04 
    ## mtry = 64    OOB error = 0.01908387 
    ## -0.05092706 1e-04

![](136_Liz_Project_Step5_Tune_Log_files/figure-markdown_github/unnamed-chunk-23-1.png)

Best ntree=32. Apply rf1=randomForest(SalePrice~.,data=Train, ntree=500,mtry=32)

``` r
RFLog=randomForest(SalePrice~.,data=H_Lo[1:1460,-1],  mtry = 32,ntree=500)
PdRFLog<-predict(RFLog,newdata= H_Lo[1461:2919,-78])
RMSE(PdRFLog,log(actual))
```

    ## [1] 0.1399177

SVM Tuning

``` r
SV2logtune1<-tune.svm(SalePrice~.,data=H_Lo[,-1],gamma=c(0.0001,0.001,0.01,0.1,1) ,cost=1:10,epsilon=0.06)
```

``` r
SV1logtune1<-tune.svm(SalePrice~.,data=H_Lo[,-1],gamma=0.1*c(0.0001, 0.0005,0.001,0.005, 0.01, 0.15,0.2, 0.3,0.5) ,cost=1:10)
```

``` r
print(SV1logtune1)
```

    ## 
    ## Parameter tuning of 'svm':
    ## 
    ## - sampling method: 10-fold cross validation 
    ## 
    ## - best parameters:
    ##  gamma cost
    ##  0.001   10
    ## 
    ## - best performance: 0.01465721

``` r
plot(SV1logtune1)
```

![](136_Liz_Project_Step5_Tune_Log_files/figure-markdown_github/unnamed-chunk-27-1.png)

``` r
sqrt(SV1logtune1$best.performance)
```

    ## [1] 0.121067

Need more fining tuning up to 0.02 - 0.001 4 '

``` r
SV1logtune2<-tune.svm(SalePrice~.,data=H_Lo[,-1],gamma=0.002*(1:10) ,cost=1:10)
```

``` r
print(SV1logtune2)
```

    ## 
    ## Parameter tuning of 'svm':
    ## 
    ## - sampling method: 10-fold cross validation 
    ## 
    ## - best parameters:
    ##  gamma cost
    ##  0.002    2
    ## 
    ## - best performance: 0.0152761

``` r
plot(SV1logtune2)
```

![](136_Liz_Project_Step5_Tune_Log_files/figure-markdown_github/unnamed-chunk-29-1.png)

``` r
sqrt(SV1logtune2$best.performance)
```

    ## [1] 0.1235965

``` r
SV1logtune3<-tune.svm(SalePrice~.,data=H_Lo[,-1],gamma=0.0005*(1:10) ,cost=1:7)
```

``` r
print(SV1logtune3)
```

    ## 
    ## Parameter tuning of 'svm':
    ## 
    ## - sampling method: 10-fold cross validation 
    ## 
    ## - best parameters:
    ##  gamma cost
    ##  0.001    4
    ## 
    ## - best performance: 0.01571751

``` r
plot(SV1logtune3)
```

![](136_Liz_Project_Step5_Tune_Log_files/figure-markdown_github/unnamed-chunk-31-1.png)

``` r
sqrt(SV1logtune3$best.performance)
```

    ## [1] 0.1253695

Find best epsilon

``` r
SV1logtune4<-tune.svm(SalePrice~.,data=H_Lo[,-1],gamma= 0.002, cost=2, epsilon=seq(0.02,0.4,length.out=5))
```

``` r
print(SV1logtune4)
```

    ## 
    ## Parameter tuning of 'svm':
    ## 
    ## - sampling method: 10-fold cross validation 
    ## 
    ## - best parameters:
    ##  gamma cost epsilon
    ##  0.002    2    0.02
    ## 
    ## - best performance: 0.01500109

``` r
SVLog1<-svm(SalePrice~.,data=H_Lo[,-1], gamma= 0.002, cost=2)
```

``` r
pdSVLog1<-predict(SVLog1, newdata=H_Lo[1461:2919,-78])
RMSE(pdSVLog1,log(actual))
```

    ## [1] 0.1264159
