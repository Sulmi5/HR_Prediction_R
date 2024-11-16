# HR Attrition Prediction Using R 

## Problem Statement
Organizations collect HR data, but raw data alone doesn’t tell a story. Analytics organizes and interprets it, answering key questions like:

What trends exist in employee turnover?

What’s driving high turnover or attrition?

Who might be most likely to leave soon?

What business is highly affected by high attrition? 

High turnover can lead to significant costs—both financially and culturally. High attrition drains company resources, disrupts team dynamics, and can lower morale, especially if key players leave. 

Analytics reveals turnover patterns, helping identify and address root causes early. With HR analytics, organizations gain insights to retain talent, reduce hiring costs, and foster a more stable, engaged workforce aligned with their goals.

## From the Analysis ##

**Observation**

- Employees who work overtime are  more likely to leave, as indicated by a high positive coefficient (1.95). This suggests that overtime could lead to burnout or job dissatisfaction.

- Job Roles such as Sales Representative and Laboratory Technician have higher attrition risks (positive coefficients of 1.57 and 1.00, respectively), while Research Directors are less likely to leave.

- Frequent travelers are more likely to leave, and even rare travelers show a slight increase in attrition risk. Travel demands may contribute to work-life imbalance or stress.

- Single employees have a higher attrition likelihood, possibly due to less attachment to the organization or fewer work-life obligations.

**Insights**	

It's worth considering to implement policies to reduce excessive overtime and offer support for affected employees. In order to reduce the number of Sales Representative leaving, the company should address unique challenges in these high-risk roles, perhaps by providing targeted support and career development opportunities. As frequent travelers may affect the wellbeing of the employee, the company should consider flexible travel policies and support for work-life balance. Moreover, the company should enhance engagement and support networks for single employees.


## Steps followed ##

- Step 1 : Load data into R studio 
- Step 2 : Clean data and deal with N/A values if exists 
- Step 3 : Exploratory Data Analysis 
- Step 4 : Model Development_Multiple Linear Regression 
- Step 5 : Train the Model 
- Step 6 : Analysis & Insights 

### Step 1 : Load data into R studio

 ```R
hr_df <- read.csv("WA_Fn-UseC_-HR-Employee-Attrition.csv")
glimpse(hr_df)

```

### Step 2 : Clean data and deal with N/A values if exists 

```R
colSums(is.na(hr_df))
```

Lukily, there's no NA values on the data. 

### Step 3 : Exploratory Data Analysis 

**Summary of Data** 

```{r pressure, echo=FALSE}
summary(hr_df)
```

**Impact of Age on Employee Attrition Rate**

```{r pressure, echo=FALSE}
age_attr <- hr_df %>%
  group_by(Age, Attrition) %>%
  summarise(Counts = n()) %>%
  ungroup()

plot1 <-ggplot(age_attr, aes(x=Age, y= Counts, colour = Attrition )) + 
  geom_line() +
  labs(title = "Impact of Age on Employee Attrition Rate") + 
  theme_grey()

plot1

```
![plot1](https://github.com/user-attachments/assets/d7e462e2-562c-47b0-8779-67e8e439077c)

The graph shows that employees are more likely to leave around their 30s, but attrition decreases as they get older, suggesting they tend to settle down with the company over time.

**Impact of Business Travel on Employee Attrition Rate**

```{r pressure, echo=FALSE}
btravel_attr <- hr_df %>%
  group_by(BusinessTravel, Attrition) %>%
  summarise(Counts = n()) %>%
  ungroup()

plot2 <-ggplot(btravel_attr, aes(x=BusinessTravel, y= Counts, fill = Attrition )) + 
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Impact of Business Travel on Employee Attrition Rate") + 
  theme_grey()

plot2

```
![plot2](https://github.com/user-attachments/assets/b12db3d3-d664-4bff-b850-e3dac330c70e)

Employees who travel frequently for business tend to leave the organization at a higher rate than those who travel rarely or not at all.

**Impact of Department on Employee Attrition Rate**

```{r pressure, echo=FALSE}
dpt_attr1 <- hr_df %>%
  group_by(Department, Attrition) %>%
  summarise(Counts = n(), .groups = 'drop') %>%
  group_by(Department) %>%
  mutate(Perc = round(Counts / sum(Counts) * 100, 2)) %>%
  ungroup()


ggplot(dpt_attr1, aes(x = factor(Department), y = Perc, fill = Attrition)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Attrition Percentage by Department",
       x = "Department",
       y = "Attrition Percentage (%)",
       fill = "Attrition") +
  theme_minimal()

```
![plot3](https://github.com/user-attachments/assets/684e1660-5ae6-426e-a6cb-ec8696b3fc39)

The bar graph shows that employees in Research & Development leave most frequently in absolute numbers. However, as a percentage, employees in Sales have a higher attrition rate (20% vs. 13.84%).

**Distance from Home**
```{r pressure, echo=FALSE}
travel_attr <- hr_df %>%
  group_by(DistanceFromHome, Attrition) %>%
  summarise(Counts = n()) %>%
  ungroup()

plot5 <-ggplot(travel_attr, aes(x=DistanceFromHome, y= Counts, color = Attrition )) + 
  geom_line() +
  labs(title = "Impact of Distance From Home on Employee Attrition Rate") + 
  theme_minimal()
plot5

```
![plot4](https://github.com/user-attachments/assets/29ee7b64-87c7-4594-812d-6fd9d6594b73)

The line graph suggests that employees living farther from the office are more likely to leave, though the effect is not highly significant.

**Job Satisfaction and Employee Attrition Rate**
```{r pressure, echo=FALSE}

job_satisfaction_attr <- hr_df %>%
  group_by(JobSatisfaction, Attrition) %>%
  summarise(Counts = n(), .groups = 'drop') %>%
  group_by(JobSatisfaction) %>%
  mutate(Perc = round(Counts / sum(Counts) * 100, 2)) %>%
  ungroup()


# Create a bar chart for attrition percentages by job satisfaction level
ggplot(job_satisfaction_attr, aes(x = factor(JobSatisfaction), y = Perc, fill = Attrition)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Attrition Percentage by Job Satisfaction Level",
       x = "Job Satisfaction Level",
       y = "Attrition Percentage (%)",
       fill = "Attrition") +
  theme_minimal()

```
![plot5](https://github.com/user-attachments/assets/62b4c97e-e13e-422c-96c5-ff57c4067083)

Employees with lower job satisfaction scores are more likely to leave.

**Number of Companies Worked**
```{r pressure, echo=FALSE}
num_attr <- hr_df %>%
  group_by(NumCompaniesWorked, Attrition) %>%
  summarise(Counts = n()) %>%
  ungroup()

plot8 <-ggplot(num_attr, aes(x=NumCompaniesWorked, y= Counts, fill = Attrition )) + 
  geom_area(position = "stack", alpha=0.6) +
  labs(title = "Number of Companies Worked on Employee Attrition Rate") + 
  theme_minimal()
plot8

```
![plot6](https://github.com/user-attachments/assets/96995142-7cad-44f4-a37b-42aa75ae33bd)

The area chart shows that employees with 1-2 prior jobs are more likely to leave for another job.

**Work-Life Balance**
```{r pressure, echo=FALSE}

worklife_attr <- hr_df %>%
  group_by(WorkLifeBalance, Attrition) %>%
  summarise(Counts = n(), .groups = 'drop') %>%
  group_by(WorkLifeBalance) %>%
  mutate(Perc = round(Counts / sum(Counts) * 100, 2)) %>%
  ungroup()


ggplot(worklife_attr, aes(x = factor(WorkLifeBalance), y = Perc, fill = Attrition)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Attrition Percentage by Work Life Balance",
       x = "Work Life Balance",
       y = "Attrition Percentage (%)",
       fill = "Attrition") +
  theme_minimal()

```
![workbalance](https://github.com/user-attachments/assets/ac582032-cb3b-4e52-906a-d46b131f5ded)

The chart demonstrates that poor work-life balance correlates with higher attrition rates, especially for employees who rated their balance as 1. Those who rated 2-4 do not show significant attrition trends.

**Years at Company on Employee Attrition Rate**
```{r pressure, echo=FALSE}
year_attr <- hr_df %>%
  group_by(YearsAtCompany, Attrition) %>%
  summarise(Counts = n()) %>%
  ungroup()

plot10 <-ggplot(year_attr, aes(x=YearsAtCompany, y= Counts, color = Attrition )) + 
  geom_line() +
  labs(title = "Years at Company on Employee Attrition Rate") + 
  theme_minimal()

plot10

```
![plot7](https://github.com/user-attachments/assets/3cf3b389-448b-4065-9ce1-c52f8e3f0354)

The line chart indicates that employees with less than 5 years at the company tend to have a higher attrition rate.

**Observation Summary** 

Employee attrition is highest among younger employees, those with low job and environment satisfaction, poor work-life balance, and short company tenure. Sales and R&D show notable attrition, especially for employees with 1-2 prior jobs. Addressing these factors can help improve retention.


### Step 5 : Model Development_Multiple Linear Regression 

**Prediction**

```{r pressure, echo=FALSE}
#select relevant column for x and y
x<- hr_df %>%
  select(-Attrition, -StandardHours, -EmployeeCount)
y <- hr_df$Attrition
```

Considering relevant inputs provided, we can predict whether an employee will be staying.  First, let’s look at the relationship between variables. 

```{r pressure, echo=FALSE}
corrplot(cor_matrix, method = "color", col = colorRampPalette(c("yellow","white","green"))(200), tl.cex=0.7, tl.col = "black", number.cex = 0.5, is.corr=TRUE,  addCoef.col = "black", number.digits=2)

```
![heatmap2](https://github.com/user-attachments/assets/9c5c5c66-2c19-4f79-ba00-adf371cd6974)

From the correlation matrix heatmap, we can infer that 
Total Working Years and Job Level have a notable positive correlation, suggesting that employees with more experience are more likely to be at higher job levels.

Years At Company is moderately correlated with Years In CurrentRole, Years With CurrManager, and Years Since Last Promotion. This implies that employees who have been with the company longer tend to have spent more time in their current role, under their current manager, and since their last promotion.
Job Satisfaction and Environment Satisfaction show some positive correlation with Relationship Satisfaction, indicating that employees who are satisfied with their environment and job may also have better workplace relationships.

**Logistic Regression**

```{r pressure, echo=FALSE}
# Split the data into training and test sets

set.seed(0)  
trin_index <- createDataPartition(y, p= 0.75, list = FALSE)
x_train <-x[train_index, ]
x_test <- x[-train_index, ]
y_train <- y[train_index]
y_test <- y[-train_index]


# Train logistic regression mode

log_reg <-glmnet(as.matrix(x_train), as.factor(y_train), family = "binomial", lambda = 0.001, alpha =0)

# Predict and calculate training and test accuracy

y_train_pred <- predict(log_reg, newx = as.matrix(x_train), type = "class")
y_test_pred <- predict(log_reg,newx = as.matrix(x_test), type = "class")

train_accuracy <- mean(y_train_pred == y_train)
test_accuracy <- mean(y_test_pred == y_test)

# Output the results
cat('--------------------------------------------------------------------------\n')
cat('Logistic Regression:\n')
cat(sprintf('Training Model accuracy score: %.3f\n', train_accuracy))
cat(sprintf('Test Model accuracy score: %.3f\n', test_accuracy))
cat('--------------------------------------------------------------------------\n')
```

**Output**

![result1](https://github.com/user-attachments/assets/2b74db80-3749-4ced-9e3e-8e3930c6e1db)

The model achieves 86.2% accuracy on the training dataset, meaning it correctly predicts the outcome for 86.2% of the cases it was trained on. The test model’s accuracy on the test dataset is 83.4%, which is slightly lower than the training accuracy. 

### Step 6 : Train the Model by Using Dummy Variables 

Let’s train the model by using dummy Variables to deal with categorical variables to increase the accuracy of the model. 
```{r pressure, echo=FALSE}
# Load necessary libraries
library(dplyr)
library(caret)
library(glmnet)
library(fastDummies)

str(hr_df)

# One-hot encode categorical variables in the dataset
data_dummies <- dummy_cols(df,
                           select_columns=c("BusinessTravel", "Department",
                                            "EducationField", "Gender",
                                            "JobRole", "MaritalStatus",
                                            "Over18", "OverTime", "Attrition"),
          remove_first_dummy = TRUE,  
        remove_selected_columns = TRUE) 


summary(data_dummies)

#fast dummies create blanks in the col names. remove blanks using make.names 
library(tidyverse)
colnames(data_dummies)

colnames(data_dummies) <- make.names(colnames(data_dummies))

# Select relevant features for X
features <- data_dummies %>%
  select( Age,DistanceFromHome,Education,EmployeeCount,EmployeeNumber,EnvironmentSatisfaction,JobInvolvement,JobLevel,JobSatisfaction,MonthlyIncome,NumCompaniesWorked,PercentSalaryHike,PerformanceRating
,RelationshipSatisfaction,StockOptionLevel,TotalWorkingYears,TrainingTimesLastYear,WorkLifeBalance,YearsAtCompany,YearsInCurrentRole,YearsSinceLastPromotion,YearsWithCurrManager,BusinessTravel_Travel_Frequently,BusinessTravel_Travel_Rarely,Department_Research...Development,Department_Sales,EducationField_Life.Sciences,EducationField_Marketing,EducationField_Medical,EducationField_Other,EducationField_Technical.Degree,Gender_Male,JobRole_Human.Resources,JobRole_Laboratory.Technician,JobRole_Manager,JobRole_Manufacturing.Director,JobRole_Research.Director,JobRole_Research.Scientist,JobRole_Sales.Executive,JobRole_Sales.Representative,MaritalStatus_Married,MaritalStatus_Single,Over18_,OverTime_Yes

)

# Convert features to matrix for logistic regression
x1 <- as.matrix(features)

# Define target variable y
y1 <- as.factor(data_dummies$Attrition_Yes)

# Split data into training and test sets
set.seed(0)  # For reproducibility
train_index <- createDataPartition(y, p = 0.75, list = FALSE)
x1_train <- x1[train_index, ]
x1_test <- x1[-train_index, ]
y1_train <- y1[train_index]
y1_test <- y1[-train_index]

# Predict and calculate accuracy on the trained model

log_reg1 <-glmnet(x1_train, y1_train, family = "binomial", lambda = 0.001, alpha = 0, maxit = 10000)

y1_train_pred <- predict(log_reg1, newx = x1_train, type = "class")
y1_test_pred <-predict(log_reg1, newx = x1_test, type = "class")

train_accuracy1 <- mean(y1_train_pred == y1_train)
test_accuracy1 <- mean(y1_test_pred == y1_test)


# Output the results
cat('--------------------------------------------------------------------------\n')
cat('Logistic Regression:\n')
cat(sprintf('Training Model accuracy score: %.3f\n', train_accuracy1))
cat(sprintf('Test Model accuracy score: %.3f\n', test_accuracy1))
cat('--------------------------------------------------------------------------\n')

```

**Output**

![result2](https://github.com/user-attachments/assets/696ae7c1-7cfb-43e1-9735-ae89d8abe004)

The model achieves 89.7% accuracy on the training dataset, showing that it fits the training data well. The model’s accuracy on the test dataset is 88.6%, which is very close to the training accuracy. This small difference suggests that the model generalizes effectively, making accurate predictions on new, unseen data.

**Coefficient Analysis**
```{r pressure, echo=FALSE}
# Extract coefficients for the trained model
coefficients <- as.matrix(coef(log_reg1, s = 0.001))

coefficients 

# Convert coefficients to a data frame for easy viewing
coefficients_df <- data.frame(
  Feature = rownames(coefficients),
  Coefficient = coefficients[, 1]
)

# Sort by absolute value of the coefficient 
coefficients_df <- coefficients_df %>%
  arrange(desc(abs(Coefficient)))

# Display the top impacting features
head(coefficients_df, 10)
```

![result3](https://github.com/user-attachments/assets/36103647-5696-4642-a2f9-9b2d485573c7)
### Step 7 : Analysis & Insights 
Analysis and Insights from Logistic Regression Coefficients
The logistic regression analysis reveals key factors influencing the likelihood of employee attrition:

**Analysis**

- Employees who work overtime are  more likely to leave, as indicated by a high positive coefficient (1.95). This suggests that overtime could lead to burnout or job dissatisfaction.

- Job Roles: Roles such as Sales Representative and Laboratory Technician have higher attrition risks (positive coefficients of 1.57 and 1.00, respectively), while Research Directors are less likely to leave.

- Frequent travelers are more likely to leave, and even rare travelers show a slight increase in attrition risk. Travel demands may contribute to work-life imbalance or stress.

- Single employees have a higher attrition likelihood, possibly due to less attachment to the organization or fewer work-life obligations.

**Insights**	

It's worth considering to implement policies to reduce excessive overtime and offer support for affected employees. In order to reduce the number of Sales Representative leaving, the company should address unique challenges in these high-risk roles, perhaps by providing targeted support and career development opportunities. As frequent travelers may affect the wellbeing of the employee, the company should consider flexible travel policies and support for work-life balance. Moreover, the company should enhance engagement and support networks for single employees.

