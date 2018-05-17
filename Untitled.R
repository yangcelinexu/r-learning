##R with Tesorflow
##head writen digit

library(keras)
model <- keras_model_sequential()%>%
  layer_conv_2d(filters=32,kernel_size=c(3,3), activation= 'relu',
               input_shape=c(21,21,1))%>%
  layer_conv_2d(filters=64,kernel_size=c(3,3), activation='relu')%>%
  layer_max_pooöing_2d(pool_size=c(2,2))%>%
  layer_flatten()%>%
  layer_dese(units=128,activation='relu')%>%
  layer_dense(units=10,acitvation ='softmax')
