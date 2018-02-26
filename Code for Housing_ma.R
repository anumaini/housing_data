#load the data 
housing_data <- read.csv(file.choose(), header=T)

#Packages used
library(dplyr)

#converting blanks to NA
Variables <- variable.names(housing_data) # converting all variable names to  
housing_data <- data.frame(ifelse(housing_data %in% c(""," ","NA"), NA, housing_data))#Treating blanks and reconverting list to Data Frame 
# renaming the strings
colnames(housing_data) <- Variables  
 
### Overall Data Assesment  
names(housing_data)
summary(housing_data)
str(housing_data)

#understanding the variables' measurement type 
Variable_names <- data.frame(sapply(housing_data, class))
Variable_names$num <- 1:78
library(dplyr)
Variable_names <- rename(Variable_names, data_class= sapply.housing_data..class.)
Variable_factors <- data.frame()
(Variable_names)



### After studying the overall data we need to disect each variable and learn more in groups

##Group 1- Action Taken by Financial Institution
unique(housing_data[,1:2])
# we learn that Variables 1 and 2 have 7 levels and indicate the point where the application stands at 

##Group 2- Agency Name 
unique(housing_data[,3:5])
#these agencies/Institutions provide the mortgage. There are 6 agencies involved in this data set. Although there may be atleast 9 or more but we use only 6.
  
##Group 3- Ethnicity ( Hispanic or Not) 
unique(housing_data[,6:7]) 
#Although there are 4 values, these variables indicate whether the applicant is Hispanic or not

##Unique 4- Income of applicants in thousands (IMPORTANT?) 
unique(housing_data[,8]) 

##Group 5- Race of applicant
unique(housing_data[,9:18]) 
#These indicate the racial diversity of the applicant; can be merged in one with hispanic  



##Factor 

Variable_factor <- Variable_names%>% filter(data_class == "factor") 
factor <- Variable_factor$num
levels(housing_data[,factor])

#### Managerial Questions
#Can we use remaining data to predict application status- Machine learning/ Model based?
  

data.frame(summary(housing_data[, ratio], na.rm=TRUE))

summary(housing_data$applicant_income_000s, na.rm=TRUE)
summary(housing_data$census_tract_number)
summary(housing_data$hud_median_family_income)
summary(housing_data$loan_amount_000s)
summary(housing_data$number_of_1_to_4_family_units)
summary(housing_data$number_of_owner_occupied_units)
summary(housing_data$minority_population)
summary(housing_data$population)
summary(housing_data$tract_to_msamd_income)

IQR(housing_data$applicant_income_000s, na.rm=TRUE)
IQR(housing_data$census_tract_number, na.rm=TRUE)
IQR(housing_data$hud_median_family_income, na.rm=TRUE)
IQR(housing_data$loan_amount_000s, na.rm=TRUE)
IQR(housing_data$number_of_1_to_4_family_units, na.rm=TRUE)
IQR(housing_data$number_of_owner_occupied_units, na.rm=TRUE)
IQR(housing_data$minority_population, na.rm=TRUE)
IQR(housing_data$population, na.rm=TRUE)
IQR(housing_data$tract_to_msamd_income, na.rm=TRUE)

sd(housing_data$applicant_income_000s, na.rm=TRUE)
sd(housing_data$census_tract_number, na.rm=TRUE)
sd(housing_data$hud_median_family_income, na.rm=TRUE)
sd(housing_data$loan_amount_000s, na.rm=TRUE)
sd(housing_data$number_of_1_to_4_family_units, na.rm=TRUE)
sd(housing_data$number_of_owner_occupied_units, na.rm=TRUE)
sd(housing_data$minority_population, na.rm=TRUE)
sd(housing_data$population, na.rm=TRUE)
sd(housing_data$tract_to_msamd_income, na.rm=TRUE)


##density charts for key variables
#*Eric Version Below
applicant_sex <- housing_data$applicant_sex
hist(applicant_sex, xlim = c(1,2), ylim = c(100,300000))
if(housing_data$action_taken==1){ ##only returns one variable for some reason 
  hist(housing_data$applicant_sex)
}

library(dplyr)
library(ggplot2)
library(tidyr)

#gender filter
housing_data_filtered <- filter(housing_data,applicant_sex_name =="Male" | applicant_sex_name == "Female") %>% 
  droplevels()

#ethnicity filter
housing_data_filtered <- filter(housing_data_filtered,
                      applicant_ethnicity_name == "Not Hispanic or Latino" | 
                        applicant_ethnicity_name == "Hispanic or Latino") %>% 
  droplevels()


#histogram of gender
housing_data_gender <- housing_data_filtered %>% 
  group_by(applicant_sex_name) %>%
  summarise(gender_count = n()) 

ggplot(housing_data_gender, aes(applicant_sex_name, gender_count)) +
  geom_bar(stat = 'identity') +ggtitle("Histogram of Gender")+xlab("Gender")+ylab("Frequency")

#histogram of ethnicity
housing_data_ethnicity <- housing_data_filtered %>% 
  group_by(applicant_ethnicity_name) %>%
  summarise(ethnicity_count = n()) 

ggplot(housing_data_ethnicity, aes(applicant_ethnicity_name, ethnicity_count))+
  geom_bar(stat = 'identity') + ggtitle("Histogram of Ethnicity")+xlab("Ethnicity")+ylab("Frequency")

#histogram by loan type
housing_data_loan <- housing_data_filtered %>% 
  group_by(loan_type_name) %>%
  summarise(loan_count = n()) 

ggplot(housing_data_loan, aes(loan_type_name, loan_count)) +
  geom_bar(stat = 'identity') + ggtitle("Histogram of Loan Type")+xlab("Loan Type")+ylab("Frequency")

#Q6

#frequency table for loan type and ethnicity
factor(df_filtered$applicant_ethnicity_name)
FreqTable = table(df_filtered$applicant_ethnicity_name,df_filtered$loan_type_name)
plot(FreqTable)

#frequency table for loan type and gender
#todo


# to observe the relation between loan amount and applicant's income
plot(df_filtered$loan_amount_000s ~ df_filtered$applicant_income_000s, main="Scatterplot of loan amount and applicant's income",xlab="Income(in thousands)",ylab="Loan Amount(in thousands)")

# bivariate plot for relationship between applicant's income (mean) 
# and loan amount (mean) by loan type
df_agg <- aggregate(df_filtered, list(df_filtered$loan_type_name), mean, na.rm=TRUE)
ggplot(df_agg, aes(applicant_income_000s, loan_amount_000s)) + geom_point() +
  geom_text(aes(label=df_agg$Group.1))

# bivariate plot for relationship between applicant's income (mean) 
# and loan amount (mean) by ethnicity
df_agg <- aggregate(df_filtered, list(df_filtered$applicant_ethnicity_name), mean, na.rm=TRUE)
ggplot(df_agg, aes(applicant_income_000s, loan_amount_000s)) + geom_point() +
  geom_text(aes(label=df_agg$Group.1))




