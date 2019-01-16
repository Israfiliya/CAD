# CAD Detection
## Introduction
Coronary Heart Disease (CAD) was one of the most lethal disease in United States. In order to create an accurate model and analyze the remarkable risk factors, a data mining classification task that involes 5 different methods (logistic, LDA, QDA, Tree, SVM) is applied onn the Z-Alizadeh Sani Dataset.

## Data Set Information
The Z-Alizadeh Sani Dataset (Z-Ali Dataset) is collected by Dr. Zahra
Alizadeh Sani, the Associate Professor of Cardiology at Iran University in
November 2017. The dataset has 303 observations and 56 variables, This dataset is restored in UCI Machine Learning Repository: https://archive.ics.uci.edu/ml/index.php

## Variable Selection
Generally, more complex model = more variance. According to correlation matrix, a lot of 55 predictors will provide invaluable or redundant information.
<p align="center">
<img src="https://raw.githubusercontent.com/Israfiliya/CAD/master/Outputs/CorP-.png">
<I>Correlation matrix for Z-Ali dataset(Top 20 with "Cath")</I>
</p>
In order to findout valuable predictors, 200 Random Forests are performed and the top 8 predictors for each random forest are recorded.

| Variable          | Frequency |
|-------------------|-----------|
| Typical.Ches.Pain | 200       |
| Age               | 200       |
| Atypical          | 200       |
| EF.TTE            | 200       |
| Region.RWMA       | 179       |
| TG                | 106       |
| FBS               | 104       |
| HTN               | 100       |
| BMI               | 88        |
| Nonanginal        | 69        |
| ESR               | 56        |
| Tinversation      | 50        |
| BP                | 48        |
At the end, we are able to show that the top 8 predictors in the frequency table are valuable in CAD detection. And those 8 variable will be used to  build reduced models and obtaining 10-fold Cross Validation Error:

## Performance with reduced model and full model
