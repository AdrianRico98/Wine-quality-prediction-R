#Carga de paquetes necesarios
library(tidyverse)
library(caret)
library(rpart)
library(Cubist)
library(corrplot)
library(purrr)
library(rpart.plot)
library(gridExtra)

#importar los datos
setwd("C:/Users/adria/Desktop/PROYECTOS/vinos-regresion-R")
data <- read.csv("winequalityred.csv", sep = ";")
data <- data %>% #preprocesamiento necesario para la transformación logaritmica de los datos
  mutate(citric.acid = ifelse(citric.acid == 0,0.0001,citric.acid))

#particiones aleatorias
set.seed(2021)
ind <- createDataPartition(data$quality, times = 1, p = 0.2, list = FALSE)
train_data <- data[-ind,]
test_data <- data[ind,]

#regresión lineal múltiple
#posibles problemas: correlaciones y distribución
cor(train_data[,c(1:11)])
matriz_correlaciones <- cor(train_data[,c(1:11)]) 
corrplot(matriz_correlaciones, #forma mas visual de ver las correlaciones.
         method = "shade",
         order = "hclust", 
         tl.col='black', 
         tl.cex=0.7,
         tl.srt = 45) 

hist(train_data$alcohol)

#preprocesamiento: eliminación de problemas de multicolinealidad y transformación logarítmica de las variables
train_data_1 <- as.data.frame(sapply(c(1:12),function(n){map_dbl(train_data[,n],log)}))
colnames(train_data_1) <- colnames(train_data)
train_data_1 <- train_data_1 %>%
  select(-free.sulfur.dioxide,-fixed.acidity)

hist(train_data_1$alcohol)

test_data_1 <- as.data.frame(sapply(c(1:12),function(n){map_dbl(test_data[,n],log)}))
colnames(test_data_1) <- colnames(test_data)
test_data_1 <- test_data_1 %>%
  select(-free.sulfur.dioxide,-fixed.acidity)

#modelo de regresión lineal múltiple
lm <- lm(quality ~ ., data = train_data_1)
summary(lm)
lm_predictions <- predict(lm,test_data_1[,1:9])
lm_predictions <- exp(lm_predictions)
rmse <- function(real, prediction){
  mean(abs(real-prediction))
}
rmse(test_data$quality,lm_predictions)

#modelo de arbol de regresion
rp <- rpart(quality ~ ., data = train_data)
rpart.plot(rp,digits = 4,fallen.leaves = TRUE, type = 3,extra = 101)
rp_predictions <- predict(rp,test_data[,1:11])
rmse(test_data$quality,rp_predictions)

#modelo arbol de regresión con modelos lineales en los nodos finales.
cb <- cubist(train_data[,c(1:11)], train_data$quality)
summary(cb)
cb_predictions <- predict(cb,test_data[,1:11])
rmse(test_data$quality,cb_predictions)

#ploteamos correlación de predicciones y datos reales
data_plot <- tibble(regression = lm_predictions,
                    regression_tree = rp_predictions,
                    model_tree = cb_predictions,
                    actual_values = test_data$quality
                    )
p1 <- data_plot %>%
  ggplot(aes(actual_values,regression)) +
  geom_point(col = "red") + 
  ggtitle("Regresion vs valores reales") + 
  theme_classic() +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(4,8,1))
p2 <- data_plot %>%
  ggplot(aes(actual_values,model_tree)) +
  geom_point(col = "blue")+ 
  ggtitle("Modelo de árbol vs valores reales") + 
  theme_classic() +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(4,8,1))
grid.arrange(p1,p2, nrow = 1)
summary(cb_predictions)
summary(lm_predictions)
