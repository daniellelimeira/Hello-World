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

