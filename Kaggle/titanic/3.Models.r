if(!require(dplyr)) install.packages("dplyr")
library(dplyr)

glm.models = function(df){
  lm.all = lm(Survived ~ ., data = df)
  lm.glm.reduced = lm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Ticket + Fare + Cabin + Embarked, data = df)
  glm.all = glm(Survived ~ ., data = df)
  glm.reduced = glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Ticket + Fare + Cabin + Embarked, data = df) 
  
  return(list(lm.all, lm.glm.reduced, glm.all, glm.reduced))
}


# regression tree
reg.tree = function(df){
  if(!require(rpart)) install.packages("rpart")
  library(rpart)
  if(!require(rpart.plot)) install.packages("rpart.plot")
  library(rpart.plot)
  
  m1 <- rpart(
    formula = Survived ~ .,
    data    = train[1:500,] %>% select(-c(Name, Ticket, Cabin)),
    method  = "anova"
  ) 
  
  return(m1)
}


rpart.plot(m1)
plotcp(m1)

pred <- predict(m1, newdata = train[501:891,])
a = train[501:891,"Survived"]
a = as.data.frame(cbind(a,pred))
a$pred1 = ifelse(a$pred<0.5, 0 ,1)
sum(a$a==a$pred1)/nrow(a)
RMSE(pred = pred, obs = train[501:891,]$Survived)


# load the library
if(!require(mlbench)) install.packages("mlbench")
library(mlbench)
if(!require(caret)) install.packages("caret")
library(caret)
# load the dataset
data(PimaIndiansDiabetes)
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(Survived~., data=train, method="lvq", preProcess="scale")
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)



predict.lm(model1, test)
```

Add a new chunk by clicking the *Insert Chunk* button on the toolbar or by pressing *Ctrl+Alt+I*.

When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Ctrl+Shift+K* to preview the HTML file).

The preview shows you a rendered HTML copy of the contents of the editor. Consequently, unlike *Knit*, *Preview* does not run any R code chunks. Instead, the output of the chunk when it was last run in the editor is displayed.

```{r, Lasso-Regression}
# Instructions: https://www.r-bloggers.com/2021/05/lasso-regression-model-with-r-code/
  
# Lasso regression
if(!require(glmnet)) install.packages("glmnet")
library(glmnet)

X = as.matrix(train[c("Pclass", "Age", "SibSp", "Parch", "Fare")])
y = as.matrix(train[c("Survived")])


lambda = 0.01
N = 500 # number of observations
    p = 20  # number of variables
X = matrix(rnorm(N*p), ncol=p)
# before standardization
    colMeans(X)    # mean
    apply(X,2,sd)  # standard deviation
 
# scale : mean = 0, std=1
    X = scale(X)
 
# after standardization
    colMeans(X)    # mean
    apply(X,2,sd)  # standard deviation
    
beta = c( 0.15, –0.33,  0.25, –0.25, 0.05,rep(0, p/2–5), 
             –0.25,  0.12, –0.125, rep(0, p/2–3))
 
    # Y variable, standardized Y
    y = X%*%beta + rnorm(N, sd=0.5)
    y = scale(y)
    
li.eq = lm(y ~ X-1)  
la.eq = glmnet(X, y, lambda=lambda, family="gaussian", intercept = F, alpha=1)

df.comp = data.frame(Linear = li.eq$coefficients,
                     Lasso = la.eq$beta[,1])

df.comp

```
