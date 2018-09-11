####################################
# Very simplemodel with KERAS ######
####################################
library(ggplot2)
library(keras) # high level wrapping interface
K <- keras::backend() # The choosen backend "TensorFlow"

# Data generation, a linear model
set.seed(123)
n<-60
x<-runif(n)-0.5
ruido<-rnorm(n)/5
y<-3+4*x^2+ruido
# Scaling the dataset
x<-scale(x)
y<-scale(y)
datos<-data.frame(x=x,y=y)
# scatter plot
ggplot(datos, aes(x=x, y=y)) + geom_point()
# Splitting train/test
prop.train<-2/3
cuales.train<-sample(1:n,round(n*prop.train),replace=FALSE)
x.train<-x[cuales.train]
x.test<-x[-cuales.train]
y.train<-y[cuales.train]
y.test<-y[-cuales.train]

### KERAS
k_clear_session() # cleansing memory
# Model architecture
modelo <- keras_model_sequential() %>%
   layer_dense(units = 256, activation = "relu" ,kernel_regularizer = regularizer_l1_l2(0,0), kernel_initializer='random_uniform', input_shape = c(1)) %>%
   layer_dense(units = 128, activation = "relu",kernel_regularizer = regularizer_l1_l2(0,0), kernel_initializer='random_uniform') %>%
   layer_dense(units = 64, activation = "relu",,kernel_regularizer = regularizer_l1_l2(0,0), kernel_initializer='random_uniform') %>%
   layer_dense(units = 1, activation = "linear")

modelo %>% compile(
  optimizer = optimizer_sgd(lr=0.1,decay=0.01),
  loss = "mse",
  metrics = "mae")

historia <- modelo %>%
  fit(x.train, y.train, batch_size = 16, epochs = 100,
      validation_data = list(x.test, y.test))
plot(historia)
# Predictions
secu<-seq(min(x),max(x),length.out = 1000)
pred<-predict(modelo,secu)
datos.pred<-data.frame(secu=secu,pred=pred)
# Plot results
datos.train<-data.frame(x=x.train,y=y.train)
ggplot() +
  geom_point(data=datos.train, aes(x=x, y=y)) +
  geom_point(data=datos.pred, aes(x=secu, y=pred),size=1/5,color=3)
