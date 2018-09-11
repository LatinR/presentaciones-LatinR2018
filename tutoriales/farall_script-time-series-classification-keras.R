##################
# Time sries clasification with KERAS
################
library(ggplot2) # nice plots
library(pROC) # ROC curve analysis
library(kerasR) # keras extensions
library(keras) # keras
K <- keras::backend() # loading of backend
set.seed(1) # fix the seed
# Data Generation, 2 classes of time series
largo<-500 # length of the time series
N<-200 # number of samples for each class of ts
dife<-1/10 # difference in AR(1) coef
despla<-1/30 # shift in location
# Train dataset generation
data.train<-matrix(NA,2*N,largo)
label.train<-c(rep(0,N),rep(1,N))
for (i in 1:N)
{
  elcoef<-(dife) # class 1
  ts.sim <- arima.sim(list(order = c(1,0,0), ar = elcoef), n = largo)
  x1<-as.numeric(ts.sim)
  elcoef<-(-dife) # class 2
  ts.sim <- arima.sim(list(order = c(1,0,0), ar = elcoef), n = largo)
  x2<-as.numeric(ts.sim)  
  data.train[i,]<-x1-despla # class 1
  data.train[i+N,]<-x2+despla # class 2
}
array.train<-array(data.train,dim=c(2*N,largo,1))

# Test dataset
data.test<-matrix(NA,2*N,largo)
label.test<-c(rep(0,N),rep(1,N))
for (i in 1:N)
{
  elcoef<-(dife) # class 1
  ts.sim <- arima.sim(list(order = c(1,0,0), ar = elcoef), n = largo)
  x1<-as.numeric(ts.sim)
  elcoef<-(-dife) #class 2
  ts.sim <- arima.sim(list(order = c(1,0,0), ar = elcoef), n = largo)
  x2<-as.numeric(ts.sim)  
  data.test[i,]<-x1-despla # class 1
  data.test[i+N,]<-x2+despla# class 2
}
array.test<-array(data.test,dim=c(2*N,largo,1))
# Plotting
cuantos<-100
datos.df<-data.frame(x1=data.train[1,1:cuantos],x2=data.train[201,1:cuantos])
ggplot() +
  geom_line(data=datos.df, aes(x=1:cuantos, y=x1),color=2) +
  geom_line(data=datos.df, aes(x=1:cuantos, y=x2),color=3)

#### A Conv1D Model

vent<-10 # size of time window
modelo <- keras_model_sequential()
modelo %>% 
  layer_conv_1d(filters = 2, kernel_size = vent, activation = 'relu',
                input_shape = c(largo,1)) %>% 
  layer_global_average_pooling_1d() %>% 
  layer_dense(units = 1, activation = 'sigmoid')
  
modelo %>%  compile(
    loss = 'binary_crossentropy',
    optimizer = 'rmsprop',
    metrics = c('accuracy')
  )

summary(modelo)

historia <- modelo %>% fit(array.train, label.train, batch_size = 16, epochs = 250,validation_split = 0.2)

plot(historia)

modelo %>% evaluate(array.test, label.test)

# let us see the probabilities

predictions <- modelo %>% predict(array.test)
colores<-c(rep(2,200),rep(3,200))
plot(jitter(label.test),predictions,col=colores)
abline(1/2,0)
#
#predictions.train <- model %>% predict(array.train)
#plot(label.train,predictions.train)
#
# ROC analysis
#
escor<-predictions
clase<-as.factor(label.test)
analroc<-roc(clase,escor)
plot(analroc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="blue", print.thres=TRUE)

# Inspecting the convolution layers

pesos<-get_weights(modelo) # parameter extraction
filtro1<-pesos[[1]][,1,1]
filtro2<-pesos[[1]][,1,2]
comb<-pesos[[3]][,1]
barplot(filtro1)
barplot(filtro2)
barplot(comb)



