 # Boston Housing price analysis

## Goal and Background:

The median price of a house in a city is determined by many micro and macro variables. Factors like per capita income of the city, crime 
rate of the area, age of the house etc. play a key role in determining the median pricing of the house.The goal of the project is to 
predict the median house price in Boston (1970) using different factors. The dataset used is the Boston housing dataset, which provides 
the median house value (medv) in 1970. There a total of 14 variables in the dataset including the median value.

## Approach:

Different modeling techniques are used to predict the housing prices. The various models built using the methods are:
- Linear regression: Interactions were studied between independents and step function was used
- CART: The tree was plotted and pruned based on the cp statistic (to avoid overfitting)
- Generalized Additive Models (GAMs): Smoothing was applied on the numeric variables
- Neural networks: Size and regularization parameter tuned based on multiple iterations and tested on the cross validation dataset

To test the accuracy of different models, the data is split into 80% training and 20% testing. Training data is used to build the models
and the best model from each technique is selected and all models are compared against each other for in sample and out of sample errors.

## Major findings:
The table below summarizes the in sample and out of sample performance of each of the models. Clearly, a tuned neural network has the 
best performance in terms of average squared errors for both the test and the train data. Model specific criteria is represented in the 
below tables (cannot be compared across models). The order of performance for these models in terms of out of sample errors is Neural 
networks, Generalized Additive Models, Linear Regression, and CART.

Method | Model specific parameters | In sample avg. sum squares | Out sample avg. sum squares | Rank based on test error |
--- | ---------------------------------------| -----------| -------------| ---------|
Linear Regression | Adj. R square : 80.4% AIC : 2293 BIC : 2349 | 15.93 | 15.99 | 3 |
CART | Size of tree : 9 | 13.67 | 23.19 |4 |
Generalized Additive Models | Adj. R square : 88.1% df : 403 edf : 349.9 AIC : 2128 BIC : 2349 | 8.65 |10.36| 2 |
Neural Networks | No. of hidden layers : 1, Size of the hidden layer: 17, Decay parameter : 1| 1.77| 9.26 |1|

## Techniques
Linear Regression, Classification And Regression Trees, Generalized Additive Models, Neural Networks 
