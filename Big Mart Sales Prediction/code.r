
library(data.table) # used for reading and manipulation of data
library(dplyr)      # used for data manipulation and joining
library(ggplot2)    # used for ploting 
library(caret)      # used for modeling
library(corrplot)   # used for making correlation plot
library(cowplot)    # used for combining multiple plots
library(glmnet)       # Lasso and Ridge Regression


train = read.csv("E:/5 SEM/3CP05 - APL/Big Mart Sales Prediction/Train.csv")
test = read.csv("E:/5 SEM/3CP05 - APL/Big Mart Sales Prediction/Test.csv")
submission= read.csv("E:/5 SEM/3CP05 - APL/Big Mart Sales Prediction/Submission.csv")
Prooutput=read.csv("E:/5 SEM/3CP05 - APL/Big Mart Sales Prediction/prooutput.csv")

dim(train)
dim(test)

names(train)
names(test)

str(train)
str(test)

test$Item_Outlet_Sales <- NA
combi= rbind(train,test)
dim(combi)
#View(combi)

ggplot(train) + geom_histogram(aes(train$Item_Outlet_Sales), binwidth = 100, fill="darkgreen" ) + xlab("Item_Outlet_Sales")

p1 = ggplot(combi) + geom_histogram(aes(Item_Weight), binwidth = 0.5, fill = "blue")
p2 = ggplot(combi) + geom_histogram(aes(Item_Visibility), binwidth = 0.005, fill = "blue")
p3 = ggplot(combi) + geom_histogram(aes(Item_MRP), binwidth = 1, fill = "blue")
plot_grid(p1, p2, p3, nrow = 1) # plot_grid() from cowplot package

ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) + geom_bar(aes(Item_Fat_Content, Count), stat = "identity", fill = "coral1")

combi$Item_Fat_Content[combi$Item_Fat_Content == "LF"] = "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == "low fat"] = "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == "reg"] = "Regular"
ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) + geom_bar(aes(Item_Fat_Content, Count), stat = "identity", fill = "coral1")

# plot for Item_Type
p4 = ggplot(combi %>% group_by(Item_Type) %>% summarise(Count = n())) + geom_bar(aes(Item_Type, Count), stat = "identity", fill = "coral1") + xlab("") +
  geom_label(aes(Item_Type, Count, label = Count), vjust = 0.5) + theme(axis.text.x = element_text(angle = 45, hjust = 1))+ ggtitle("Item_Type")

# plot for Outlet_Identifier
p5 = ggplot(combi %>% group_by(Outlet_Identifier) %>% summarise(Count = n())) +  geom_bar(aes(Outlet_Identifier, Count), stat = "identity", fill = "coral1") +
  geom_label(aes(Outlet_Identifier, Count, label = Count), vjust = 0.5) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# plot for Outlet_Size
p6 = ggplot(combi %>% group_by(Outlet_Size) %>% summarise(Count = n())) + geom_bar(aes(Outlet_Size, Count), stat = "identity", fill = "coral1") +
  geom_label(aes(Outlet_Size, Count, label = Count), vjust = 0.5) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

second_row = plot_grid(p5, p6, nrow = 1)
plot_grid(p4, second_row, ncol = 1)

#remaining categorical variables
# plot for Outlet_Establishment_Year
p7 = ggplot(combi %>% group_by(Outlet_Establishment_Year) %>% summarise(Count = n())) + 
  geom_bar(aes(factor(Outlet_Establishment_Year), Count), stat = "identity", fill = "coral1") +
  geom_label(aes(factor(Outlet_Establishment_Year), Count, label = Count), vjust = 0.5) +
  xlab("Outlet_Establishment_Year") +
  theme(axis.text.x = element_text(size = 8.5))
# plot for Outlet_Type
p8 = ggplot(combi %>% group_by(Outlet_Type) %>% summarise(Count = n())) + 
  geom_bar(aes(Outlet_Type, Count), stat = "identity", fill = "coral1") +
  geom_label(aes(factor(Outlet_Type), Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(size = 8.5))

# ploting both plots together
plot_grid(p7, p8, ncol = 2)

#Bivariate Analysis
#we'll explore the independent variables with respect to the target variable

train = combi[1:8523,1:12] # extracting train data from the combined data
#trian = combi[1:nrow(train)]
dim(train)
#Target Variable vs Independent Numerical Variables
# Item_Weight vs Item_Outlet_Sales
p9 = ggplot(train) + 
  geom_point(aes(Item_Weight, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +
  theme(axis.title = element_text(size = 8.5))

# Item_Visibility vs Item_Outlet_Sales
p10 = ggplot(train) + 
  geom_point(aes(Item_Visibility, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +
  theme(axis.title = element_text(size = 8.5))

# Item_MRP vs Item_Outlet_Sales
p11 = ggplot(train) + 
  geom_point(aes(Item_MRP, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +
  theme(axis.title = element_text(size = 8.5))
second_row_2 = plot_grid(p10, p11, ncol = 2)
plot_grid(p9, second_row_2, nrow = 2)

#Target Variable vs Independent Categorical Variables

# Item_Type vs Item_Outlet_Sales
p12 = ggplot(train) + 
  geom_violin(aes(Item_Type, Item_Outlet_Sales), fill = "magenta") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 8.5))

# Item_Fat_Content vs Item_Outlet_Sales
p13 = ggplot(train) + 
  geom_violin(aes(Item_Fat_Content, Item_Outlet_Sales), fill = "magenta") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8.5))

# Outlet_Identifier vs Item_Outlet_Sales
p14 = ggplot(train) + 
  geom_violin(aes(Outlet_Identifier, Item_Outlet_Sales), fill = "magenta") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8.5))
second_row_3 = plot_grid(p13, p14, ncol = 2)
plot_grid(p12, second_row_3, ncol = 1)

#remaining variables

p15 = ggplot(train) + geom_violin(aes(Outlet_Location_Type, Item_Outlet_Sales), fill = "magenta")
p16 = ggplot(train) + geom_violin(aes(Outlet_Type, Item_Outlet_Sales), fill = "magenta")
plot_grid(p15, p16, ncol = 1)

##find missing values in a variable.

sum(is.na(combi$Item_Weight))

##Imputing Missing Value

missing_index = which(is.na(combi$Item_Weight))
for(i in missing_index)
  {
  
  item = combi$Item_Identifier[i]
  combi$Item_Weight[i] = mean(combi$Item_Weight[combi$Item_Identifier == item], na.rm = T)
}

##Cross Check

sum(is.na(combi$Item_Weight))

#Replacing 0's in Item_Visibility variable

ggplot(combi) + geom_histogram(aes(Item_Visibility), bins = 100)

#replace the zeroes

zero_index = which(combi$Item_Visibility == 0)
for(i in zero_index){
  
  item = combi$Item_Identifier[i]
  combi$Item_Visibility[i] = mean(combi$Item_Visibility[combi$Item_Identifier == item], na.rm = T)
  
}



ggplot(combi) + geom_histogram(aes(Item_Visibility), bins = 100)

##Item_Type variable and classify the categories into perishable and non_perishable as per our understanding and make it into a new feature.

perishable = c("Breads", "Breakfast", "Dairy", "Fruits and Vegetables", "Meat", "Seafood")
non_perishable = c("Baking Goods", "Canned", "Frozen Foods", "Hard Drinks", "Health and Hygiene", "Household", "Soft Drinks")

# create a new feature 'Item_Type_new'
combi$Item_Type_new <- ifelse(combi$Item_Type %in% perishable, "perishable", ifelse(combi$Item_Type %in% non_perishable, "non_perishable", "not_sure"))
dim(combi)

#Let's compare Item_Type with the first 2 characters of Item_Identifier, i.e., 'DR', 'FD', and 'NC'. These identifiers most probably stand for drinks, food, and non-consumable.

table(combi$Item_Type, substr(combi$Item_Identifier, 1, 2))

# Item_category Created

combi$Item_category <- substr(combi$Item_Identifier, 1, 2)

#Outlet_Years (years of operation) and price_per_unit_wt (price per unit weight).

combi$Item_Fat_Content[combi$Item_category == "NC"] = "Non-Edible"

combi$Outlet_Years <- (2013 - combi$Outlet_Establishment_Year)
combi$Outlet_Establishment_Year = as.factor(combi$Outlet_Establishment_Year)

# Price per unit weight
combi$price_per_unit_wt <- (combi$Item_MRP/combi$Item_Weight)

#the Item_MRP vs Item_Outlet_Sales plot, we saw Item_MRP was spread across in 4 chunks. Now let's assign a label to each of these chunks and use this label as a new variable.

# creating new independent variable - Item_MRP_clusters
combi$Item_MRP_clusters <- ifelse(combi$Item_MRP < 69, "1st", 
                                   ifelse(combi$Item_MRP >= 69 & combi$Item_MRP < 136, "2nd",
                                          ifelse(combi$Item_MRP >= 136 & combi$Item_MRP < 203, "3rd", "4th")))
dim(combi)
##encoding categorical variable -> label encoding

combi$Outlet_Size_num <- ifelse(combi$Outlet_Size == "Small", 0,
                                 ifelse(combi$Outlet_Size == "Medium", 1, 2))
combi$Outlet_Location_Type_num <- ifelse(combi$Outlet_Location_Type == "Tier 3", 0,
                                          ifelse(combi$Outlet_Location_Type == "Tier 2", 1, 2))

# removing categorical variables after label encoding
combi$Outlet_Size <- NULL
combi$Outlet_Location_Type <- NULL


#One hot encoding for the categorical variable

ohe = dummyVars("~.", data = combi[,c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")], fullRank = T)
ohe_df = data.table(predict(ohe, combi[,c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")]))
combi = cbind(combi[,"Item_Identifier"], ohe_df)

#PreProcessing Data

#Removing Skewness

combi$Item_Visibility <- log(combi$Item_Visibility + 1) # log + 1 to avoid division by zero
combi$price_per_unit_wt <- log(combi$price_per_unit_wt + 1)

##preprocessing

# Scaling and centering is required for linear regression models.

num_vars = which(sapply(combi, is.numeric)) # index of numeric features
num_vars_names = names(num_vars)
combi_numeric = combi[,setdiff(num_vars_names, "Item_Outlet_Sales"), with = F]
prep_num = preProcess(combi_numeric, method=c("center", "scale"))
combi_numeric_norm = predict(prep_num, combi_numeric)

combi[,setdiff(num_vars_names, "Item_Outlet_Sales") := NULL] # removing numeric independent variables
combi = cbind(combi, combi_numeric_norm)


dataset1 <- train[,c("Item_Identifier")]
linear_reg_mod = lm(train$Item_Outlet_Sales ~ ., data = as.data.frame( dataset1 ))
summary(linear_reg_mod)
#Making Predictions on test Data

# preparing dataframe for submission and writing it in a csv file
dataset2 <- test[,c("Item_Identifier")]

submission$Item_Outlet_Sales = predict(linear_reg_mod, as.data.frame(dataset2))
write.csv(submission, "Linear_submit.csv", row.names = F)



