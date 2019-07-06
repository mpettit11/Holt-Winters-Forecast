library(dplyr)
library(readxl)
library(rpart)
library(stats)
library(forecast)
getwd()
#Read in
Imports<-read.csv("/Users/MattPettit/Documents/Senior/Spring 2019/Res 497/R Sample Data.csv")
attach(Imports)
Quarterly_Imports<-ts(Imports, frequency = 4)
print(Quarterly_Imports)

plot(Quarterly_Imports)

#Develop Wi
n=length(Quarterly_Imports)
n.wi=n-19
Imports_Within<-ts(Quarterly_Imports[1:n.wi],frequency = 4)
print(Imports_Within)

#H-W Smoothing Parameters
Imports_Model<-HoltWinters(Imports_Within,alpha = NULL,beta = NULL,gamma = TRUE)
print(Imports_Model)

HoltWinters(x=Imports_Within,alpha = NULL,beta = NULL,gamma = TRUE)

Imports_Model$ SSE
print(SumSquareError)
Imports_Model$
summary(Imports_Model)

#H-W Forecast
W.in<-fitted(Imports_Model)
print(W.in)

#Developing within sample evaluation--first two observations are dropped
#Within sample will be observations 3:60 (58 total)

print(Imports_Within)
Eval_Imports_Within<-Quarterly_Imports[3:n.wi]
print(Eval_Imports_Within)

#Within RMSE
print(W.in)
W.in.RMSE<-sqrt(94623935289/length(Eval_Imports_Within))
print(W.in.RMSE)  

#Developing post sample evaluation
#Post sample will be observations 60:78 (18 total)

Eval_Imports_Post<-Quarterly_Imports[(n.wi+1):n]
print(Eval_Imports_Post)  



#Post Sample forecasts 1-step  
Imports_post<-ts(Quarterly_Imports[(n.wi+1):n],frequency = 4)
print(Imports_post)

HoltWinters(x=Imports_post, alpha=NULL, beta = NULL,gamma = FALSE)

W.out<-fitted(Imports_post)
View(W.out)
View(Imports_post)
Imports_post$ SSE



#Post RMSE
W.out.RMSE<-sqrt(26776645069.3465/length(Imports_post))
print(W.out.RMSE)






