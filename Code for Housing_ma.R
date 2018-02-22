
#Trying the Changes

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


#### Managerial Questions
#Can we use remaining data to predict application status- Machine learning/ Model based?
  
  
