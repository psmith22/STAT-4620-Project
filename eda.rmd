---
title: "STAT 4620 Project EDA"
author: "Patrick Smith, Nate Rising, Keefer Aman, and Luke Dinan"
date: "11/11/2021"
output: html_document
---

```{r, message = FALSE}
# install and load packages

#install.packages("tidyverse")
library(tidyverse)

#install.packages("corrplot")
library(corrplot)
```

```{r}
# avoid scientific notation
options(scipen = 999)

# load data
load("ames.Rdata")

# glance at data
head(train)
```

#### What kind of variables are there?
```{r}
# check types of data
str(train)
```

Based on the output above, the predictors are mainly stored as either ints or strings. It is somewhat difficult to differentiate between the types of data based on the way they are stored, but from the dataset description we know that there are 23 nominal, 23 ordinal, 14 discrete, and 20 continuous predictors. 

#### Is there any missing data?
```{r}
# check for missing data
# arrange by count of missing 
na_count <- sapply(train, function(y) sum(length(which(is.na(y)))))
data.frame(colnames(train), na_count) %>% arrange(-na_count)

# deal with missing values
# replace categorical na values with category name
train$Alley <- ifelse(is.na(train$Alley), "None", train$Alley)
train$Fireplace.Qu <- ifelse(is.na(train$Fireplace.Qu), "None", train$Fireplace.Qu)
train$Bsmt.Qual <- ifelse(is.na(train$Bsmt.Qual), "No Basement", train$Bsmt.Qual)
train$Bsmt.Cond <- ifelse(is.na(train$Bsmt.Cond), "No Basement", train$Bsmt.Cond)
train$Bsmt.Exposure <- ifelse(is.na(train$Bsmt.Exposure), "No Basement", train$Bsmt.Exposure)
train$BsmtFin.Type.1 <- ifelse(is.na(train$BsmtFin.Type.1), "No Basement", train$BsmtFin.Type.1)
train$BsmtFin.Type.2 <- ifelse(is.na(train$BsmtFin.Type.2), "No Basement", train$BsmtFin.Type.2)
train$Pool.QC <- ifelse(is.na(train$Pool.QC), "No Pool", train$Pool.QC)
train$Garage.Type <- ifelse(is.na(train$Garage.Type), "No Garage", train$Garage.Type)
train$Garage.Finish <- ifelse(is.na(train$Garage.Finish), "No Garage", train$Garage.Finish)
train$Garage.Qual <- ifelse(is.na(train$Garage.Qual), "No Garage", train$Garage.Qual)
train$Garage.Cond <- ifelse(is.na(train$Garage.Cond), "No Garage", train$Garage.Cond)
train$Fence <- ifelse(is.na(train$Fence), "No Fence", train$Fence)
train$Misc.Feature <- ifelse(is.na(train$Misc.Feature), "None", train$Misc.Feature)

# deal with continuous missing values
# replace Lot.Frontage and Garage.Yr.Blt with mean based on neighborhoods in future
# for now just replace any missing values with median
train$Lot.Frontage[is.na(train$Lot.Frontage)]<-median(train$Lot.Frontage,na.rm=TRUE)
train$Garage.Yr.Blt[is.na(train$Garage.Yr.Blt)]<-median(train$Garage.Yr.Blt,na.rm=TRUE)
train$Mas.Vnr.Area[is.na(train$Mas.Vnr.Area)]<-median(train$Mas.Vnr.Area,na.rm=TRUE)
train$Bsmt.Full.Bath[is.na(train$Bsmt.Full.Bath)]<-median(train$Bsmt.Full.Bath,na.rm=TRUE)
train$Bsmt.Half.Bath[is.na(train$Bsmt.Half.Bath)]<-median(train$Bsmt.Half.Bath,na.rm=TRUE)
train$BsmtFin.SF.1[is.na(train$BsmtFin.SF.1)]<-median(train$BsmtFin.SF.1,na.rm=TRUE)
train$BsmtFin.SF.2[is.na(train$BsmtFin.SF.2)]<-median(train$BsmtFin.SF.2,na.rm=TRUE)
train$Bsmt.Unf.SF[is.na(train$Bsmt.Unf.SF)]<-median(train$Bsmt.Unf.SF,na.rm=TRUE)
train$Total.Bsmt.SF[is.na(train$Total.Bsmt.SF)]<-median(train$Total.Bsmt.SF,na.rm=TRUE)
train$Garage.Cars[is.na(train$Garage.Cars)]<-median(train$Garage.Cars,na.rm=TRUE)
train$Garage.Area[is.na(train$Garage.Area)]<-median(train$Garage.Area,na.rm=TRUE)
```

#### Is there any potentially problematic collinearity amongst the predictor variables?
```{r}
# create df of only continuous predictors
ames.cont <- train %>% 
  dplyr::select(Lot.Frontage, Lot.Area, Mas.Vnr.Area, BsmtFin.SF.1, BsmtFin.SF.2, Bsmt.Unf.SF, Total.Bsmt.SF, X1st.Flr.SF, X2nd.Flr.SF, Low.Qual.Fin.SF, Gr.Liv.Area, Garage.Area, Wood.Deck.SF, Open.Porch.SF, Enclosed.Porch, X3Ssn.Porch, Screen.Porch, Pool.Area, Misc.Val, SalePrice)

# correlation matrix
corrplot(cor(ames.cont))

# create df of correlations
correlations <- as.data.frame(as.table(cor(ames.cont))) %>% 
  rename(correlation = Freq) %>% 
  filter(correlation < -0.25 | correlation > 0.5 & correlation != 1) %>% 
  arrange(correlation)

correlations[seq_len(nrow(correlations)) %% 2 == 1,]
```

It seems that many of the predictors involving square footage are highly correlated with `SalePrice`. Later on, it may be useful to create a total square footage predictor and use that in the modeling phase. There are high correlations between many of the predictors. For example, `X1st.Flr.Sf` and `Total.Bsmt.SF` are highly correlated with a correlation coefficient of 0.788. `Gr.Liv.Area` and `X2nd.Flr.SF` are also highly correlated with a coefficient of 0.655. Overall, it seems that houses that are larger in one area are also larger in another area. 

#### Can you detect early signs of what variables are likely to be important in predicting the response?
```{r}
# investigate sale price
# observe scatterplot of relationships between predictors and sale price

# 1st floor square footage vs sale price
# mostly linear but there seems to be a couple outliers with high sf and low saleprice
ggplot(train, aes(x = X1st.Flr.SF, y = SalePrice)) + geom_point() + ggtitle("1st Floor Square Footage vs Sale Price")

# log of 1st floor square footage vs sale price
# log transformation is more linear --> use log of sf in model
ggplot(train, aes(x = log(X1st.Flr.SF), y = SalePrice)) + geom_point() + ggtitle("Log of 1st Floor Square Footage vs Sale Price")

# enclosed porch vs sale price
# no relationship between enclosed porch and sale price
ggplot(train, aes(x = Enclosed.Porch, y = SalePrice)) + geom_point() + ggtitle("Enclose Porch vs Sale Price")

# investigate some categorical predictors vs sale price
ggplot(train, aes(x = House.Style, y = SalePrice, col = House.Style)) + geom_boxplot() + ggtitle("House Style vs Sale Price")
```

With such a large amount of predictors, it is not worth while at this point to explore the relationship of every predictor with `SalePrice`. If necessary, the relationships between `SalePrice` and other predictors could be explored later on. We plan on using a ridge regression model as well as a LASSO model in the modeling phase. With these models, it is not necessary to eliminate predictors from the dataset. Lastly, for certain variables like `Enclose.Porch` or `Pool.Area`, there are many zero values that could possibly lead to a low correlation with `SalePrice`. It may be useful to simplify these predictors to a binary categorical value of either having a porch or pool or not having one. 
