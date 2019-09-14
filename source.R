########################################
#         Visualization in R           #
#                                      #    
########################################
library(readxl)
library(tidyverse)
setwd('C:/Users/Vikraant Pai/workspace/basic_visualzation_in_R/')

#Checking the values in the data set and understanding the data
df <- read_excel("./data.xls")
View(df)

#Cleaning and conforming the data 
df <-  df %>%
  mutate(Gender = case_when(gender == 'm' ~ 'Male',
                            gender == 'f' ~ 'Female'))  %>%
  mutate(Jobtype = case_when(
    jobcat == 1 ~ 'Clerical',
    jobcat == 2 ~ 'Custodial',
    jobcat == 3 ~ 'Manager'
  )) %>%   mutate(Minority = case_when(
    minority == 1 ~ 'Yes',
    minority == 0 ~ 'No'
  )) %>% mutate(increase_in_sal = (salary - salbegin)/jobtime )  #Instead of salary alone we will consider increment in the salary per unit jobtime

#Calculating the average salary for male and average salary for female for particular job type
mean_values <-  df %>%
  group_by(Gender, Jobtype) %>%
  summarise(average_increase_in_salary = mean(increase_in_sal)) %>%  arrange(desc(average_increase_in_salary))
mean_values

median_values <-  df %>%
  group_by(Gender, Jobtype) %>%
  summarise(median_increase_in_salary = median(increase_in_sal)) %>%  arrange(desc(median_increase_in_salary))
median_values



#Converting the value in Data Time
 
df <-  df %>% mutate(age = as.numeric(round(difftime( Sys.Date(), bdate, units = "days")/365)))  %>%  
  mutate(age_group = case_when(
    age < 49 ~ '<49',
    age <= 60 ~ '49-60',
    age <= 70 ~ '61-70' ,
    age <= 80 ~ '71-80',
    age <= 91 ~ '81-91',
    T ~ '>91'
  ))






 