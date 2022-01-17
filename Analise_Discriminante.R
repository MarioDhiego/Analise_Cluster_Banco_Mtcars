
# Um Exemplo e Apliacação Básica do Método Estatístico Multivariado
# Análise Discriminante na Linguagem de Programação R.

################# Analise Discriminante ########################################################
################################################################################################

# Realizar Aplicacao Pratica da Análise Discriminante Linear


##### Instalar Pacotes #########################################################################
install.packages("ISLR","MASS","caret","tidyverse")

################################################################################################


##### Ativar Pacotes ###########################################################################

library(ISLR)      # utilizar banco de dados marketing
library(MASS)      # utilizar Linear Discriminant Analysis  
library(caret)     #
library(tidyverse) # fazer manipula??o de dado
library(magrittr)
################################################################################################


############### Leitura de Base de Dados #######################################################
data("Smarket")
################################################################################################

############## Analise Estrutural Dados ########################################################
View(Smarket)
names(Smarket)
dim(Smarket)
summary(Smarket)
str(Smarket)

# Retirar a var Ano
Smarket1 <- Smarket[,-1]
attach(Smarket)
###############################################################################################

############## Divis?o do Banco ###############################################################
# Fazer a divis?o do Banco de Dados em Treinamneto e Teste 

Smarket_Treino <- Smarket[1:1000,]
Smarket_Teste <- Smarket[1001:1250,]
View(Smarket_Teste)

###############################################################################################


############## Analise Discriminante ##########################################################
# Modelo Dicriminante 1/ por Ressubstitui??o
Model.fit1 <- lda(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume+Today,
               data = Smarket_Teste, CV=FALSE)
# Resultados
Model.fit1

# Gr?fico
plot(Model.fit1)


# Modelo Dicriminante 2/ Com Valida??o Cruzada
Model.fit2 <- lda(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume+Today,
               data = Smarket_Teste, CV=TRUE)
# Resultados
Model.fit2
plot(Model.fit2)


##### Predição 
Model.Predicao1 = predict(Model.fit1, Smarket_Teste[,-9])
names(Model.Predicao1)
Model1.class = Model.Predicao1$class
plot(Model1.class)

coef(Model.fit1)
coef(Model.fit2)

##### Classificação dos Elementos
tabela1 <- table(lda.class, Smarket_Teste$Direction)
tabela1

# Probabilidade de Classifica??o dos Elementos
prop.table(tabela1)

# Acuracia
1-(10+0)/(10+0+99+141)


################ Probabilidades ##############################################################
# Class     : indica em qual grupo cada objeto foi classificado
# posterior : probabilidade a posteriori estimado pelo modelo
# x         : valor da fun??o discriminante estimada
 
# Classifica??o dos 10 primeiros elementos
head(lda.pred$class, 10)

# Probabilidade de Pertencer a grupo 0 e 1
head(lda.pred$posterior, 10)

# Valor do Score Estimado pela fun??o
head(lda.pred$x, 10)


#### Analise da Qualidade p/ Substitui??o
l <- mean(lda.pred$class == Smarket_Treino$Direction)
print(l)
###############################################################################################


##### Matriz de Confusao

predito <- lda.pred$class
observado <- as.factor(Smarket_Teste$Direction)
confusionMatrix(predito, observado)

# Escores das Obs
FD <- as.matrix(Smarket_Teste[,-5]) %>% coef(Model.fit1) 
dim(FD)


# Escore Medio

FDb <- (Model.fit1$means) %>% 
  coef(Model.fit1)

cores <- rainbow(Volume(levels(Smarket_Teste[,"ksksks"])))
stripchart(FD[,1]~Direction, 
           pch=20, 
           xlab="Funcao1",
           ylab="Direcao",
           col=cores, 
           method="stack",
           data=Smarket_Teste)
points(FDb[,1], (1:Volume(Model.fit1$lev))+ 0.05, pch=13,col=cores, cex=1.5)





