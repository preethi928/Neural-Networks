#we should normalize the data in neural networks to avoid gradient exploding 

concret<-read.csv("C:\\Data_science\\EXCLER\\My Assignments\\NEURAL NETWORKS\\concrete.csv")
View(concret)

#custom normalization function.

normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
concrete_norm<-as.data.frame(lapply(concret,normalize))

#creating training and test data 

concret_train<-concrete_norm[0:700,]
concret_test<-concrete_norm[701:1030,]

#package of ANN
library(neuralnet)

#building ANN model
colnames(concret_train)
model<-neuralnet(strength ~ cement+slag+ash+water+
                   superplastic+coarseagg+fineagg+age+strength,
                 data = concret_train,act.fct = "logistic",stepmax = 3100)

#step max is the maximum number of times the backpropagtion should happen .

#when you execute this model it says that algorithm did not converge it means ,
#these many steps are not enough to build the model , now lets take that off and 
#lets the model take it by default.
model<-neuralnet(strength ~ cement+slag+ash+water+
                   superplastic+coarseagg+fineagg+age+strength,
                 data = concret_train,act.fct = "logistic")
#now the model is converged
#lets visulaize the network topology

plot(model)
#no of inputs = no of varaiables in the data set (or model)
#steps are : 7607 

#Now lets predict the models output by using COMPUTE

model_pred<-compute(model,concret_test)

predicted_strength<- model_pred$net.result

#we will examine the corrolation between predicted and actual data

cor(predicted_strength,concret_test$strength)
 #the accuracy is 0.999

#we can also build the model using hidden as a parameter (which helps us to increase the 
#hidden layers in turn it increases the accuracy )

model1<-neuralnet(strength ~ cement+slag+ash+water+
                   superplastic+coarseagg+fineagg+age+strength,
                 data = concret_train,act.fct = "logistic",hidden = c(5,2))

plot(model1)

#lets predict for this 

model1_pred<-compute(model1,concret_test)
#store the pred strength
pred_str_mod2<-model1_pred$net.result

#lets check for accuracy (when hidden layers are increased)

cor(pred_str_mod2,concret_test$strength)
#there fore its :0.9995522