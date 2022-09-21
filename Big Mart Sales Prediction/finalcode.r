
data_set = read.csv("E:/5 SEM/3CP05 - APL/Big Mart Sales Prediction/Train.csv")
#View(data_set)

data_set$Item_Identifier = as.factor(data_set$Item_Identifier)
#View(data_set$Item_Identifier)

data_set$Item_Fat_Content = as.factor(data_set$Item_Fat_Content)
#View(data_set$Item_Fat_Content)

data_set$Item_Type = as.factor(data_set$Item_Type)
#View(data_set$Item_Type)

data_set$Outlet_Location_Type = as.factor(data_set$Outlet_Location_Type)
#View(data_set$Outlet_Location_Type)

data_set$Outlet_Type = as.factor(data_set$Outlet_Type)
#View(data_set$Outlet_Type)

summary(data_set)

#str(data_set)

#only imp data 
use_data = data_set[ ,c("Item_Identifier", "Item_Fat_Content" , "Item_Type" , "Outlet_Location_Type", "Item_Weight", "Item_Visibility" , "Outlet_Type" , "Item_MRP")]

head(use_data)  #top6values
summary(use_data)

na_values= na.omit(use_data)
#View(na_values)
summary(na_values)
#str(na_values)

na_values.Item_Identifier_id = sapply(na_values$Item_Identifier,as.numeric)
#View(na_values.Item_Identifier_id)

na_values.Item_Fat_Content_id = sapply(na_values$Item_Fat_Content,as.numeric)
#View(na_values.Item_Fat_Content_id)

na_values.Item_Type_id = sapply(na_values$Item_Type,as.numeric)
#View(na_values.Item_Type_id)

na_values.Outlet_Location_Type_id = sapply(na_values$Outlet_Location_Type,as.numeric)
#View(na_values.Outlet_Location_Type_id)

na_values.Outlet_Type_id = sapply(na_values$Outle_Type,as.numeric)
#View(na_values.Outlet_Type_id)

Final_dataset = cbind(na_values, identifier_id =na_values.Item_Identifier_id ,
                      Fat_Content = na_values.Item_Fat_Content_id ,
                      Item_Type_id =na_values.Item_Type_id,
                      Outlet_Location_id = na_values.Outlet_Location_Type_id)

View(Final_dataset)
#str(Final_dataset)


last_final_dataset = Final_dataset[,c("Item_Identifier", "identifier_id" ,"Item_Fat_Content" , "Fat_Content" , "Item_Type", "Item_Type_id", "Outlet_Location_type", "Outlet_Location_id" , "Item_Weight", "Item_Visibility" , "Item_MRP")]

View(last_final_dataset)


model_data=last_final_dataset
summary(model_data)

model = lm(Item_MRP~identifier_id +  model_data$Item_Type_id + model_data$Outlet_Location_id + model_data$Item_Weight + model_data$Item_Visibility , data=model_data)
summary(model)

Y= 125.239297 + (0.002084)*667  +(0.823373)*14 + (0.086965)*3 +(0.620893)*18.850 +(-7.291514)*0.138190277
print(Y)


