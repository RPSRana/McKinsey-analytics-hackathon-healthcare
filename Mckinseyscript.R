test<-fread("E:/Analytics Vidhya/Mckinsey/test_v2akXPA.csv")
train<-fread("E:/Analytics Vidhya/Mckinsey/train_ajEneEa.csv")
samplesub<-fread("E:/Analytics Vidhya/Mckinsey/sample_submission_1.csv")
str(train)
summary(train)
#creating numeric features from categorical variables
train<-train[,AgeDiscret := as.factor(round(age/10,0))]
train$agecat <- cut(train$age, 
                       breaks = c(0, 30, 60, 90), 
                       labels = c("young", "adult", "old"), 
                       right = FALSE)
train$ever_married<-factor(train$ever_married)
levels(train$ever_married)
levels(train$ever_married)[1]<-"0"
levels(train$ever_married)[2]<-"1"
train$work_type<-as.numeric(factor(train$work_type))
train$Residence_type<-as.numeric(factor(train$Residence_type))
train$gender<-as.factor(train$gender)
test<-test[,AgeDiscret := as.factor(round(age/10,0))]
test$agecat <- cut(test$age, 
                    breaks = c(0, 30, 60, 90), 
                    labels = c("young", "adult", "old"), 
                    right = FALSE)
test$ever_married<-factor(test$ever_married)
levels(test$ever_married)
levels(test$ever_married)[1]<-"0"
levels(test$ever_married)[2]<-"1"
test$work_type<-as.numeric(factor(test$work_type))
test$Residence_type<-as.numeric(factor(test$Residence_type))
test$gender<-as.factor(test$gender)
train<-train[,id:=NULL]
train$smoking_status[train$smoking_status==""]<-"No answer"
test$smoking_status[test$smoking_status==""]<-"No answer"
train[] <- lapply(train, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
test[] <- lapply(test, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))

#feature engineering with categorical variables
test$id=NULL
test[,stroke:=2]
View(test)
combin = rbind(train, test)
combin$smoking_status<-as.numeric(factor(combin$smoking_status))
combin[,work_type_count:=.N,by=work_type]
combin[,AgeDiscret_count:=.N,by=AgeDiscret]
combin[,agecat_count:=.N,by=agecat]
combin[,smoking_status_count:=.N,by=smoking_status]
combin[,Residence_type_count:=.N,by=Residence_type]
combin[,work_type_count:=work_type_count/max(work_type_count)]
combin[,work_type_count:=work_type_count/max(work_type_count)]
combin[,AgeDiscret_count:=AgeDiscret_count/max(AgeDiscret_count)]
combin[,agecat_count:=agecat_count/max(agecat_count)]
combin[,smoking_status_count:=smoking_status_count/max(smoking_status_count)]
combin[,Residence_type_count:=Residence_type_count/max(Residence_type_count)]
combin[,Agediscret_work:=.N,by=c("AgeDiscret","work_type")]
combin[,Agediscret_agecat:=.N,by=c("AgeDiscret","agecat")]
combin[,Agediscret_gen:=.N,by=c("AgeDiscret","gender")]
combin[,Agediscret_smo:=.N,by=c("AgeDiscret","smoking_status")]
combin[,Agediscret_res:=.N,by=c("AgeDiscret","Residence_type")]
combin[,work_type_agecat:=.N,by=c("work_type","agecat")]
combin[,work_type_smo:=.N,by=c("work_type","smoking_status")]
combin[,work_type_res:=.N,by=c("work_type","Residence_type")]
combin[,agecat_smo:=.N,by=c("agecat","smoking_status")]
combin[,agecat_res:=.N,by=c("agecat","Residence_type")]
combin[,smo_res:=.N,by=c("smoking_status","Residence_type")]
combin[,Agediscret_work:=Agediscret_work/max(Agediscret_work)]
combin[,Agediscret_smo:=Agediscret_smo/max(Agediscret_smo)]
            
combin[,Agediscret_agecat:=Agediscret_agecat/max(Agediscret_agecat)]
combin[,Agediscret_res:=Agediscret_smo/max(Agediscret_res)]
combin[,work_type_agecat:=work_type_agecat/max(work_type_agecat)]
combin[,work_type_smo:=work_type_smo/max(work_type_smo)]
combin[,work_type_res:=work_type_smo/max(work_type_res)]
combin[,agecat_smo:=agecat_smo/max(agecat_smo)]
combin[,agecat_res:=agecat_res/max(agecat_res)]
combin[,work_type_res:=work_type_res/max(work_type_res)]
combin[,smo_res:=smo_res/max(smo_res)]
combin[,Agediscret_work_smo:=.N,by=c("AgeDiscret","work_type","smoking_status")]
combin[,Agediscret_work_agecat:=.N,by=c("AgeDiscret","work_type","agecat")]
combin[,Agediscret_work_res:=.N,by=c("AgeDiscret","work_type","Residence_type")]
combin[,Agediscret_agecat_res:=.N,by=c("AgeDiscret","agecat","Residence_type")]
combin[,Agediscret_agecat_smo:=.N,by=c("AgeDiscret","agecat","smoking_status")]
combin[,work_res_smo:=.N,by=c("work_type","Residence_type","smoking_status")]
combin[,Agediscret_work_smo:=Agediscret_work_smo/max(Agediscret_work_smo)]
combin[,Agediscret_work_agecat:=Agediscret_work_agecat/max(Agediscret_work_agecat)]
combin[,Agediscret_work_res:=Agediscret_work_res/max(Agediscret_work_res)]
combin[,Agediscret_agecat_smo:=Agediscret_agecat_smo/max(Agediscret_agecat_smo)]
combin[,Agediscret_agecat_res:=Agediscret_agecat_res/max(Agediscret_agecat_res)]
combin[,work_res_smo:=work_res_smo/max(work_res_smo)]
combin[,Agediscret_work_smo_res:=.N,by=c("AgeDiscret","work_type","smoking_status","Residence_type")]
combin[,Agediscret_work_smo_res:=Agediscret_work_smo_res/max(Agediscret_work_smo_res)]


traindata = combin[c(1:nrow(train)),]
testdata = combin[-c(1:nrow(train)),]
testdata[,click := NULL]
rm(combin)

#Note I havent't included the validation that you can do by yourself.. I will update later.
#Preparation for xgboost
sparse_matrix <- Matrix::sparse.model.matrix(stroke~.-1, data = traindata)
train_matrix<-xgb.DMatrix(data = as.matrix(sparse_matrix),label=traindata$stroke)
test_sparse<-Matrix::sparse.model.matrix(~.-1,data = testdata)
test_matrix<-xgb.DMatrix(data = as.matrix(test_sparse))
#tuning the parameters this is not my final tuning.. I tuned with different values you can make it better
params <- list("eta"=0.01,
               "max_depth"=10,
               "colsample_bytree"=0.5,
               "min_child_weight"=1,
               "subsample"=0.7,
               "objective"="binary:logistic",
               "eval_metric"="auc"
)
bst <- xgboost(params = params,data =train_matrix , label =as.factor(traindata$stroke),nrounds = 1000)
pred <- predict(bst, newdata=test_matrix)
result <- data.frame(id = samplesub$id  ,
                  stroke = pred)
write.csv(result,file = "submission2.csv", row.names = FALSE)

