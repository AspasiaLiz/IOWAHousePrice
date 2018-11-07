# IOWAHousePrice
This is data from Kaggle Competition :  https://www.kaggle.com/c/house-prices-advanced-regression-techniques/data

Step 1 : Basically looking at the data. Data Description/Visualization/Checking for NAs. Initial Cleaning of removing ID (meaningless) and changing a numerical value that represents categorical value into categorical value for correctness.  Rough looking at outliers. Looking at some visualizations. Histogram/Boxplot for numeric variables and barplot for categorical variables. Plot against Saleprice for all too see correlations. Correlation Matrix. Look at skewness (non-normality) to see what should be log transformed if any.


Step 2 : Data Cleaning - 
Mostly working on imputation of NAs.
Also modifying ordinal variables into numeric variables.
Correcting incorrect values (Year 2207 for example, and NAs where it should have been 0 and other categories.)


Step 3 : Feature Selection
Look at foward selection, backwards elimination, stepwise regression and use Boruta package (random forest tree - feature selection) to select features.

Step 3.2 : Log Transformation 

Step 4 : Regression using 3 models from step 3 + using all features (4 options)
1. Linear Regression
2. Random Forest 
3. SVM 
prediction are done and RMSE/RMSLE calculated. SVM performs the best but will do some tuning.
Also working on Lasso/Ridge/Elasticnet.


Step 4.2 : Quantile Regression - Encountering many errors. Working on it.











----------------
Below changed from factor to numerical variables. 
Col Name	type	N/A(%)	R - Output
PoolQC	F	100%	Factor w/ 3 levels "Ex","Fa","Gd": NA NA NA NA NA NA NA NA NA NA ...
GarageFinish	F	6%	Factor w/ 3 levels "Fin","RFn","Unf": 2 2 2 3 2 3 2 2 3 2 ...
LandSlope	F	0%	Factor w/ 3 levels "Gtl","Mod","Sev": 1 1 1 1 1 1 1 1 1 1 ...
PavedDrive	F	0%	Factor w/ 3 levels "N","P","Y": 3 3 3 3 3 3 3 3 3 3 ...
BsmtExposure	F	3%	Factor w/ 4 levels "Av","Gd","Mn",..: 4 2 3 4 1 4 1 3 4 4 ...
BsmtQual	F	3%	Factor w/ 4 levels "Ex","Fa","Gd",..: 3 3 3 4 3 3 1 3 4 4 ...
KitchenQual	F	0%	Factor w/ 4 levels "Ex","Fa","Gd",..: 3 4 3 3 3 4 3 4 4 4 ...
ExterQual	F	0%	Factor w/ 4 levels "Ex","Fa","Gd",..: 3 4 3 4 3 4 3 4 4 4 ...
BsmtCond	F	3%	Factor w/ 4 levels "Fa","Gd","Po",..: 4 4 4 2 4 4 4 4 4 4 ...
Fence	F	81%	Factor w/ 4 levels "GdPrv","GdWo",..: NA NA NA NA NA 3 NA NA NA NA ...
HeatingQC	F	0%	Factor w/ 5 levels "Ex","Fa","Gd",..: 1 1 1 3 1 1 1 1 3 1 ...
GarageQual	F	6%	Factor w/ 5 levels "Ex","Fa","Gd",..: 5 5 5 5 5 5 5 5 2 3 ...
ExterCond	F	0%	Factor w/ 5 levels "Ex","Fa","Gd",..: 5 5 5 5 5 5 5 5 5 5 ...
GarageCond	F	6%	Factor w/ 5 levels "Ex","Fa","Gd",..: 5 5 5 5 5 5 5 5 5 5 ...
FireplaceQu	F	47%	Factor w/ 5 levels "Ex","Fa","Gd",..: NA 5 5 3 5 NA 3 5 5 5 ...
BsmtFinType1	F	3%	Factor w/ 6 levels "ALQ","BLQ","GLQ",..: 3 1 3 1 3 3 3 1 6 3 ...
BsmtFinType2	F	3%	Factor w/ 6 levels "ALQ","BLQ","GLQ",..: 6 6 6 6 6 6 6 2 6 6 ...
Functional	F	0%	Factor w/ 7 levels "Maj1","Maj2",..: 7 7 7 7 7 7 7 7 3 7 ...
 
Some less obvious ones too:
Col Name	type	N/A(%)	R - Output	Description
Utilities	F	0%	Factor w/ 2 levels "AllPub","NoSeWa": 1 1 1 1 1 1 1 1 1 1 ...	 Type of utilities available
Alley	F	94%	Factor w/ 2 levels "Grvl","Pave": NA NA NA NA NA NA NA NA NA NA ...	 Type of alley access to property
BldgType	F	0%	Factor w/ 5 levels "1Fam","2fmCon",..: 1 1 1 1 1 1 1 1 1 2 ...	 Type of dwelling

Column	# of NA	comment	Percentage 
BsmtQual	37	impute to 0 	3%
BsmtCond	37	impute to 0 	3%
BsmtExposure	38	impute to 0 , one is a typo. Impute that to 1 (no)	3%
BsmtFinType1	37	impute to 0	3%
BsmtFinType2	38	impute to 0, one is a typo, impute that to most common value 3 (rec) among the ones that has SF for Basement type 2	3%
GarageType	81	impute to "noGarage"	6%
GarageYrBlt	81	impute to 1 lower than lowest value	6%
GarageFinish	81	guessing no garage	6%
GarageQual	81	guessing no garage	6%
GarageCond	81	guessing no garage	6%
LotFrontage	259	Impute	18%
MasVnrType	8	Impute to none	1%
MasVnrArea	8	Impute to 0	1%
Electrical	1	impute to skrbr because anything after 2000 was only them and the building was built after 2000 and has pretty much all features otherwise	0%
Alley	1369	No Alley - Change to NotExist	94%
Fence	1179	NO Fence - Change to NoFence	81%
FireplaceQu	690	NO Fireplace - Change to NoFirep	47%
MiscFeature	1406	No miscFeature - Change to No MisFeat	96%
PoolQC	1453	No Pool - Change to NoPool	100%

