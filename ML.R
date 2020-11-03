#Modelos Lineares de Regressão

library(DAAG)
litters

#Data on the body and brain weights of 20 mice, together with the size of the litter. Two mice were taken from each litter size.
#lsize = litter size
#bodywt = body weight
#brainwt = brain weight

attach(litters)
names(litters)

reg1 <- lm(brainwt ~ lsize + bodywt)
summary(reg1)
reg1
#Todas as variáveis são significativas para o modelo, porém o R^2 e o R^2 robusto é de 0,6505 e 0,6094 respectivamente.

library(scatterplot3d)

z=litters[,1]
x=litters[,2]
y=litters[,3]
scatterplot3d(x, y, z, color = "red",
              grid = TRUE, box = FALSE, col.grid = "gray", col.axis = "steelblue", 
              xlab="Peso Corporal", ylab="Tamaho do Cérebro",  zlab = "Tamanho da Ninhada",
              mar = c(5, 3, 0.8, 2))

library(rgl)
library(magick)
plot3d(x, y, z,
       grid = TRUE, box = FALSE, col.grid = "gray", col.axis = "steelblue", 
       xlab="Peso Corporal", ylab="Tamaho do Cérebro",  zlab = "Tamanho da Ninhada",
       mar = c(5, 3, 0.8, 2))

#...........................................................
#Base para as Regressões Ridge, Lasso e Elastic Net
rm(list=ls())

swiss

X <- swiss[,-1] #conjunto de variáveis independentes
y <- swiss[,1] #variável dependente

library(glmnet)

#Criando a matriz x para a função cv.glmnet
c1 <- X[,1]
c2 <- X[,2]
c3 <- X[,3]
c4 <- X[,4]
c5 <- X[,5]
x <- matrix(c(c1, c2, c3, c4, c5), 47, 5)
colnames(x) <- c("Agriculture", "Examination", "Education", "Catholic", "Infant.Mortality")

##Regressão Ridge
set.seed(123)
model1 <- cv.glmnet(x, y, alfa = 0, lambda = 10^seq(4, -1, -0.1)) #validação cruzada
best_lambda <- model1$lambda.min #melhor lambda

ridge_coeff <- predict(model1, s = best_lambda, type = "coefficients")
ridge_coeff #Coeficentes de regressão

##Regressão Lasso
set.seed(123)
model2 <- cv.glmnet(x, y, alfa = 1, parallel = TRUE, lambda = 10^seq(4, -1, -0.1))
best_lambda1 <- model2$lambda.min #melhor lambda


lasso_coeff <- predict(model2, s = best_lambda1, type = "coefficients")
lasso_coeff #Coeficentes de regressão

##Regressão Elastic Net
set.seed(123)
model3 <- cv.glmnet(x, y, alfa = 0.5, lambda = 10^seq(4, -1, -0.1))
best_lambda2 <- model3$lambda.min #melhor lambda

en_coeff <- predict(model3, s = best_lambda2, type = "coefficients")
en_coeff #Coeficentes de regressão
