##########################
# Image Classification ###
###########################
library(EBImage)
library(ggplot2) # nice plots
library(pROC) # ROC curve analysis
library(kerasR) # keras extensions
library(keras) # keras
K <- keras::backend() # loading of backend

# loading of TRAIN data
direc<-"/home/andres/Dropbox/Inteligencia Artificial con R/Imagenes2_Train/"
setwd(direc) # setting the directory
lista<-list.files() # list of files
cant<-length(lista)
imagenes<-list()
for (i in 1:cant)
{
  imagenes[[i]]<-readImage(lista[[i]]) 
}
#display(imagenes[[12]])
# Resizing the images
resol<-100 # new image resolution
#t(sapply(imagenes,dim))
imagres<-list()
for (i in 1:cant)
{
  ima<-resize(imagenes[[i]],resol,resol) 
  imagres[[i]]<-channel(ima,"gray")
  }
#display(imagres[[26]])
#str(imagres)
#t(sapply(imagres,dim))
# Combining all images together
imacomb.train<-combine(imagres)
array.train<-aperm(imacomb.train,c(3,1,2))
array.train<-array_reshape(array.train,dim=c(cant,resol,resol,1))

# defining the TRAIN response
clase<-substr(as.character(lista),1,3)
label.train<-(clase=="ten")*1
display(tile(imacomb.train))

# loading of TEST data
direc<-"/home/andres/Dropbox/Inteligencia Artificial con R/Imagenes2_Test/"
setwd(direc) # setting the directory
lista<-list.files() # list of files
cant<-length(lista)
imagenes<-list()
for (i in 1:cant)
{
  imagenes[[i]]<-readImage(lista[[i]]) 
}
#display(imagenes[[2]])
# Resizing the images
resol<-100 # new image resolution
#t(sapply(imagenes,dim))
imagres<-list()
for (i in 1:cant)
{
  ima<-resize(imagenes[[i]],resol,resol) 
  imagres[[i]]<-channel(ima,"gray")
}
#display(imagres[[2]])
#str(imagres)
#t(sapply(imagres,dim))
# Combining all images together
imacomb.test<-combine(imagres)
array.test<-aperm(imacomb.test,c(3,1,2))
array.test<-array_reshape(array.test,dim=c(cant,resol,resol,1))

# defining the TEST response
clase<-substr(as.character(lista),1,3)
label.test<-(clase=="ten")*1
display(tile(imacomb.test))

#### all images
imacomb.tot<-combine(imacomb.train,imacomb.test)
display(tile(imacomb.tot))
#### A Conv2D Model
set.seed(123)
vent<-3
filtros<-64
salida<-256
modelo <- keras_model_sequential()
modelo %>% 
  layer_conv_2d(filters = filtros, kernel_size = c(vent,vent),
                activation = 'relu',
                input_shape = c(resol,resol,1)) %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_conv_2d(filters = filtros, kernel_size = c(vent,vent),
                activation = 'relu') %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_conv_2d(filters = filtros, kernel_size = c(vent,vent),
                activation = 'relu') %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_conv_2d(filters = filtros, kernel_size = c(vent,vent),
                activation = 'relu') %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_flatten() %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = salida, activation = 'relu') %>%
  layer_dense(units = 1, activation = 'sosftmax')
  
modelo

modelo %>%  compile(
  loss = 'binary_crossentropy',
  optimizer = RMSprop(),
  metrics = c('accuracy'))

# historia <- modelo %>%
#   fit(array.train, label.train, batch_size = 8,
#       epochs = 45,validation_split = 0.2)

historia <- modelo %>%
  fit(array.train, label.train, batch_size = 16,
      epochs = 100,validation_data = list(array.test,label.test))

plot(historia)

# Evaluation on TEST dta

modelo %>% evaluate(array.test,label.test)
# Predictions

pred.train <- predict(modelo,array.train)
pred.test <- predict(modelo,array.test)

boxplot(split(pred.train,label.train))
boxplot(split(pred.test,label.test))

# Inspecting the fitted net

pesos<-get_weights(modelo)
str(pesos)
image(pesos[[1]][,,1,4])
image(pesos[[3]][,,2,4])
