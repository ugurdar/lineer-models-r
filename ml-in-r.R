library(MASS) # Boston veri seti için.
library(caret) # train fonksiyonu için

df <- Boston


indeks <- sample(2,nrow(df),replace=T,prob = c(0.7,0.3))
train <- df[indeks == 1,]
test <- df[indeks == 2,]


kontrol <- trainControl(method = "repeatedcv",
                        number = 10,
                        repeats = 5)

# Doğrusal Model

lineer <- train(medv~.,
                data = train,
                method = "lm",
                trControl = kontrol )
summary(lineer)
plot(lineer$finalModel)

# Stepwise Regresyon

stepwise <- train(medv~.,
                data = train,
                method = "lmStepAIC",
                trControl = kontrol )
  
  
stepwise$results

# Ridge Regresyon

ridge <- train(medv~.,
                  data = train,
                  method = "glmnet",
                  tuneGrid = expand.grid(alpha = 0,
                                         lambda = seq(0.0001,1,length=50)),
                  trControl = kontrol )
  
  
plot(ridge$finalModel,xvar="lambda")
plot(ridge)


# Lasso Regresyon

lasso <- train(medv~.,
               data = train,
               method = "glmnet",
               tuneGrid = expand.grid(alpha = 1,
                                      lambda = seq(0.0001,1,length=50)),
               trControl = kontrol )
plot(lasso$finalModel,xvar="lambda")
plot(varImp(lasso))

# Elastic - Net Regresyon

elastic <- train(medv~.,
               data = train,
               method = "glmnet",
               tuneGrid = expand.grid(alpha = seq(0,1,length=10),
                                      lambda = seq(0.0001,1,length=50)),
               tuneLenghth = 10,
               trControl = kontrol )

# PCR
pcr <- train(medv~.,
                 data = train,
                 method = "pcr",
                 scale = TRUE,
                 tuneLenghth = 11,
                 trControl = kontrol )
# PLS 
pls <- train(medv~.,
             data = train,
             method = "pls",
             scale = TRUE,
             trControl = kontrol )
############################################

# Lineer Regresyon
# Eğitim Hatası
p <- predict(lineer,train)
lineer_e <- sqrt(mean((train$medv-p)^2))

# Test Hatası
p <- predict(lineer,test)
lineer_t <- sqrt(mean((test$medv-p)^2))


# Stepwise Regresyon
# Eğitim Hatası
p <- predict(stepwise,train)
stepwise_e <- sqrt(mean((train$medv-p)^2))

# Test Hatası
p <- predict(stepwise,test)
stepwise_t <- sqrt(mean((test$medv-p)^2))

# Ridge Regresyon
# Eğitim Hatası
p <- predict(ridge,train)
ridge_e <- sqrt(mean((train$medv-p)^2))

# Test Hatası
p <- predict(ridge,test)
ridge_t<- sqrt(mean((test$medv-p)^2))

# Lasso Regresyon
# Eğitim Hatası
p <- predict(lasso,train)
lasso_e <- sqrt(mean((train$medv-p)^2))

# Test Hatası
p <- predict(lasso,test)
lasso_t[i]<- sqrt(mean((test$medv-p)^2))

# Elastic-Net Regesyon
# Eğitim Hatası
p <- predict(elastic,train)
elastic_e<- sqrt(mean((train$medv-p)^2))

# Test Hatası
p <- predict(elastic,test)
elastic_t <- sqrt(mean((test$medv-p)^2))


# PCR Regesyon
# Eğitim Hatası
p <- predict(pcr,train)
pcr_e <- sqrt(mean((train$medv-p)^2))

# Test Hatası
p <- predict(pcr,test)
pcr_t <- sqrt(mean((test$medv-p)^2))

# PLS 
p <- predict(pls,train)
pls_e <- sqrt(mean((train$medv-p)^2))

# Test Hatası
p <- predict(pls,test)
pls_t <- sqrt(mean((test$medv-p)^2))

model_liste <- list(DogrusalModel = lineer,Stepwise = stepwise,
                    Ridge = ridge, Lasso = lasso,
                    ElasticNet = elastic, PCR= pcr,
                    PLS = pls)
resamp <- resamples(model_liste)
summary(resamp)


###########################################

test_h <- list(Lineer =lineer_t,Stepwise = stepwise_t,
               Ridge = ridge_t, Lasso = lasso_t,
               ElasticNet = elastic_t, PCR = pcr_t,
               PLS = pls_t)

egitim_h <- list(Lineer =lineer_e,Stepwise = stepwise_e,
               Ridge = ridge_e, Lasso = lasso_e,
               ElasticNet = elastic_e, PCR = pcr_e,
               PLS = pls_e)


test_h <- c(lineer_t,stepwise_t,
               ridge_t,lasso_t,
               elastic_t,pcr_t,pls_t)
egitim_h <- c(lineer_e,stepwise_e,
              ridge_e,lasso_e,
              elastic_e,pcr_e,pls_e)
Yontemler <- c("Lineer" ,"Stepwise",
                "Ridge", "Lasso",
                "ElasticNet" , "PCR",
                "PLS" )

hatalar <- data.frame("Yöntemler"= Yontemler,
                      "Eğitim Hatası" = egitim_h,
                      "Test Hatası" = test_h)


print(hatalar)
