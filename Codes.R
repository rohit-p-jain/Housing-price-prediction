
# Declaring the libraries required

library(MASS)                    # Boston housing data
library(tidyverse)               # Cleaning the data
library(stringi)                 # String pattern detections
library(nnet)                    # Neural Networks
library(gamlss.add)              # Plot the neural networls
library(boot)                    # Cross validation
library(mgcv)                    # Genralized additive models  
library(rpart)                   # Classification and Regression Trees
library(PerformanceAnalytics)    # For good EDA plots

#Setting seed for reporoducible results
set.seed(12397009)

# Loding Dataset
Boston <- MASS::Boston

# Sampling - 80:20
samp <- sample(nrow(Boston), size = 0.8*nrow(Boston))

Boston_train <- Boston[samp,]
Boston_test <- Boston[-samp,]

# Unsderstanding the variables by a single plot
chart.Correlation(Boston[,!names(Boston) %in% c('crim','zn','chas','rad')])

#####################################################################################################
############################ Linear Regression as the base model ###################################
###################################################################################################

# Fitting a linear regression
full_model <- lm(data = Boston_train, formula = medv ~ .)

#Summary of the full model
summary(full_model)
#The model has an Adj Rsq of hust 72.8%, with an MSE of 22.76! Need to improve the model

# Checking all possible Interactions

# Step 1. Create all possible combinations
varnames <- names(Boston_train)
varnames <- varnames[!varnames == "medv"]

# Step 2. Expand the grid
ee <- expand.grid(varnames,varnames)

# Step 3. Remove the repeating permutations
ee$dummy = 1
ee_t <- spread(ee, key=Var2, value=dummy)
ee_t1 <- ee_t[,1]
ee_t2 <- ee_t[,-1]

# Making the upper triange as NA to remve it
ee_t2[upper.tri(ee_t2, diag=T)] <- NA 

# Gather again
ee_fin <- cbind(ee_t1,ee_t2) %>%
  gather(key="Var2", value="Val",-ee_t1) %>%
  filter(!is.na(Val))

# Conver to character
ee_fin$ee_t1 <- as.character(ee_fin$ee_t1)

names(ee_fin) <- c("Var1", "Var2", "dummy")

# Blank output table
rows <- dim(ee_fin)[1]
Final_table <- data.frame(Variable1 = rep(NA,rows), Variable2 = rep(NA,rows), min_p = rep(NA,rows), all = rep(NA,rows))

# Step 4. Looping through possible combinations
for (i in 1:rows) {
  newdata <- cbind(Boston_train["medv"], Boston_train[ee_fin[i,"Var1"]],Boston_train[ee_fin[i,"Var2"]])
  model <- lm(medv ~ .^2, data = newdata)
  
  tab1 <- data.frame(summary(model)$coefficients)
  table <- data.frame(Column = row.names(tab1), tab1[,1:4])
  
  interact <- table[stri_detect_fixed(table[,"Column"], ":"),]
  min_p <- min(interact[,5], na.rm=T)
  vals <- paste(paste(interact[,c(1)],interact[,c(5)], sep=" = "), collapse = ",")
  Final_table[i,1] <- ee_fin[i,"Var1"]
  Final_table[i,2] <- ee_fin[i,"Var2"]
  Final_table[i,3] <- min_p
  Final_table[i,4] <- vals
}

# Viewing the significant interactions
Final_table %>% filter(min_p <= 0.05) %>% arrange(min_p) %>% View()


#Using interaction results - top 4
mod_Test <- lm(data = Boston_train, formula = medv ~ . +lstat:rm + rad:rm +tax:rm +rm:nox)


#Comparing to the base model in terms of comparable statistics (Yet to write)
adjrsq.lm <- c(summary(full_model)$adj.r.squared,summary(mod_Test)$adj.r.squared)
MSE.lm <- c(sum(full_model$residuals^2)/(full_model$df.residual),sum(mod_Test$residuals^2)/(mod_Test$df.residual))
AIC.lm <- c(AIC(full_model), AIC(mod_Test))

Comparsion.lm <- data.frame(Type = c("Base", "New") ,Adj.R.Sq = adjrsq.lm, MSE = MSE.lm, AIC = AIC.lm)

# Great improvement!

# Running a step function on the new model to identify factors that are not important
mod_null <- lm(data = Boston_train, formula = medv ~ 1 )

# Using Stepwise
step <- step(mod_null,scope = list(lower=mod_null, upper=mod_Test), direction= 'both' ,trace=F)

# Summary of stepwise
summary(step)

# Wo observe that zn has a non-significant pvale

# Running an anova on reduced vs full
model_final = update(step, . ~ .  - zn)

# P-value indicates it isn't a good fit
anova(model_final, step)

# P-value indicates that Zn is not in important variable

#Cross Validation to do a final check if the error is not highly variant
model.glm1 = glm(medv~., data = Boston_train)
model.glm2 = glm(medv~lstat+rm+ptratio+dis+crim+nox+rad+chas+tax+black+zn+lstat:rm+rm:tax, data = Boston_train)
model.glm3 = glm(medv~lstat+rm+ptratio+dis+crim+nox+rad+chas+tax+black+lstat:rm+rm:tax, data = Boston_train)

#CV Setting the seed
set.seed(12397009)

cv.glm(data = Boston_train, glmfit = model.glm1, K = 3)$delta[2]
cv.glm(data = Boston_train, glmfit = model.glm2, K = 3)$delta[2]
cv.glm(data = Boston_train, glmfit = model.glm3, K = 3)$delta[2]

# Only a minor different in error when zn is removed, hence we can remove Zn

# Final mode chosen
summary(model_final)
par(mfrow=c(2,2))
plot(model_final)
mtext("Model diagnostic plots", side = 3, line = -1, outer = TRUE, cex = 1.2)
par(mfrow=c(1,1))


#MSE of chosen model
i.mse.lm <- sum((Boston_train$medv - model_final$fitted)^2)/(nrow(Boston_train) - 13)
#Avg. SE
i.avgse.lm <- mean((Boston_train$medv - model_final$fitted)^2)

##Out of sample##
out.pred.lm <- predict(model_final, Boston_test)

## sse
o.avgse.lm <- mean((Boston_test$medv - out.pred.lm)^2)


#####################################################################################################
############################ Classification and Regression Trees ###################################
###################################################################################################

# Creating the tree
boston.rpart <- rpart(formula = medv ~ . , data = Boston_train)

# Plotting the tree
plot(boston.rpart)
text(boston.rpart)

# In sample predictions from the tree
insamp_pred_tree <- predict(boston.rpart)

# In sample avg.se
i.mse.tree <- mean((Boston_train$medv - insamp_pred_tree)^2)

# Out of sample statistics
outsamp_pred_tree = predict(boston.rpart,Boston_test)

o.avgse.tree <- mean((Boston_test$medv - outsamp_pred_tree)^2)

# Very high when compared to linear reg

#####################################################################################################
################################# Generalized Additive Models ######################################
###################################################################################################

# Creating the the formula on all possble variables - First model
colnames <- names(Boston_train)

# Gam needs to have a formula beforehand, can't input directly in the model
# Only variables with higher degrees of freedom can be smoothened
gam_formula <- as.formula(paste("medv ~",paste(paste0("s(",colnames[-which(colnames %in% c("rad","medv", "chas"))],")"),collapse = "+")
                                ,"+","chas +",        colnames[which(colnames=="rad")]))

# GAM model
boston.gam <- gam(formula = gam_formula,  data = Boston_train)

# Summary Stats
summary(boston.gam)
AIC(boston.gam)
BIC(boston.gam)


# Some smooths are not significant. Hence we will not use the smoothing parameters on them
gam_formula <- as.formula(paste("medv ~",paste(paste0("s(",colnames[-which(colnames %in% c("rad","medv", "chas","zn","age","black"))],")"),collapse = "+")
                                ,"+","chas + zn +age+ black +" ,        colnames[which(colnames=="rad")]))


boston.gam1 <- gam(formula = gam_formula,  data = Boston_train)

# Summary - All smmots are now significant
summary(boston.gam1)

# Plotting the smoothing applied on each variable
plot(boston.gam1, shade = TRUE, seWithMean = TRUE, scale = 0, pages=1)

# MSE of gam
i.mse.gam <- boston.gam1$dev/boston.gam1$df.res
i.avgse.gam <-boston.gam1$dev/nrow(Boston_train)


# Residuals vs. fitted
plot(fitted(boston.gam1), residuals(boston.gam1),
     xlab = 'fitted', ylab = 'residuals', main =
       'Residuals by Fitted from GAM')

# out of sample 
outsamp_pred_gam <- predict(boston.gam1,Boston_test)

o.avgse.gam <- mean((Boston_test$medv - outsamp_pred_gam)^2)

bostonedf <- boston.gam1$df.res
AIC.gam <- AIC(boston.gam1)
BIC.gam <- BIC(boston.gam1)


#####################################################################################################
##################################### Neural Networks ##############################################
###################################################################################################

## Standardize the X variables (Normal Standardization)
data1 <- Boston_train[,-c(4,14)]
  
# Calculating the mean and the deviation for train data
means <- sapply(data1,mean)
sds <- sapply(data1,sd)

# standardizing  
Boston_train_stan <- cbind(sweep(sweep(data1,2,means), 2, sds,FUN="/"),Boston_train[,c(4,14)])


#Using the same training mean and sd for the test data
data2 <- Boston_test[,-c(4,14)]
Boston_test_stan <- cbind(sweep(sweep(data2,2,means), 2, sds,FUN="/"),Boston_test[,c(4,14)])


# Train networks with sizes of hidden units ranging from 0 to 20, and decay from 0.1 to 0.9 
# then calculate the average SSE for training and test datasets
Decay <- seq(0.1,1,length.out = 10)
nodes <- 1:20

dn <- expand.grid(Decay, nodes)

dn$average_train_sse <- NA
dn$average_test_sse <- NA

# Running thr loop across combinations- Can take time
for (n in 1:nrow(dn)){ 
  train.predict <- 0
  test.predict <- 0
  for(i in 1:10){ # for each size, train 10 networks with different random starting points, averaging the ten results to make the prediction more accurate
    set.seed(i*100)
    Boston.nnet <- nnet(medv ~ ., size = dn[n,"Var2"], data = Boston_train_stan, maxit=10000, decay=dn[n,"Var1"], linout = TRUE, trace =F)
    train.predict<-train.predict + sum((Boston_train_stan$medv-predict(Boston.nnet, Boston_train_stan))^2)
    test.predict<-test.predict + sum((Boston_test_stan$medv-predict(Boston.nnet, Boston_test_stan))^2)
  }
  # average outcomes of 10 networks
  train.predict.avg <-train.predict/10 
  test.predict.avg <-test.predict/10
  
  train.predict.origin <- train.predict.avg 
  test.predict.origin <- test.predict.avg
  
  dn[n,"average_train_sse"] <- train.predict.origin / nrow(Boston_train_stan)
  dn[n,"average_test_sse"] <- test.predict.origin / nrow(Boston_test_stan)
}


dn[which(min(dn[,"average_test_sse"])==dn[,"average_test_sse"]),]


# Plotting hte hidden nodes for a sinlge decay value
dn22 <- dn[which(dn$Var1==0.8), ]


ggplot() +
  geom_line(data=dn22, aes(x=Var2,y=average_train_sse, col="red")) + 
  geom_line(data=dn22, aes(x=Var2,y=average_test_sse, col="blue")) +
  xlab("Hidden Nodes") +
  ggtitle("Avg. SSE vs. Hidden nodes") +
  ylab("Avg. SSE") +
  scale_colour_manual(name = "Type",values =c('red'='red','blue'='blue'), labels = c('Test','Train'))


# Plotting different nodes for 1 decay
dn22 <- dn[which(dn$Var2 %in% c(16,17,18,19)), c("Var1", "Var2", "average_test_sse")]


ggplot(data=dn22, aes(x=Var1,y=average_test_sse, col=as.factor(Var2))) +
  geom_line() +
  xlab("Decay") +
  ggtitle("Avg. SSE vs. decay") +
  ylab("Avg. SSE") 
 

# For plotting the neural network 
Boston.nnet.fin <- nnet(medv ~ ., size = 17, data = Boston_train_stan, maxit=10000, decay=1, linout = TRUE, trace=F)


"plot"(Boston.nnet.fin)

#size done!

# In  sample mse
i.mse.nn <- dn[170,3]

# Out sample MSE
o.avgse.nn <- dn[170,4]

