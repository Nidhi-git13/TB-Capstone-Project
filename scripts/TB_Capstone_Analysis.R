data <- read_xlsx("R class/Basics/data/clinical parameters_TBpatients.xlsx")
library(readxl)
View(data)
str(data)
names(data)
colnames(data)
library(janitor)
data <- clean_names(data)
names(data)[1:10]
#1.	Bring the date of assessment column first and rename it to “start”
library(dplyr)
data <- data %>% rename(start= date_of_assessment) %>% 
  relocate(start, .before=end)
#2.	Create a unique id from row-id.
#(Hint see dplyr cheatsheet::rownames_to_column())
library(tibble)
data <- rownames_to_column(data, var = "id")
#3.	Find the mean delay in assessment and mean age. 
#Replace any missing value with mean.
class(data$start)
class(data$end)
data <- data %>%
  mutate(assessment_delay=as.numeric(end-start))
data <- data %>%
  mutate(assessment_delay=as.numeric (difftime(end, start, units = "days")))
mean_delay <- mean(data$assessment_delay, na.rm = TRUE)
mean_delay       
mean_age <- mean(data$age, na.rm = TRUE) 
mean_age
sum(is.na(data$assessment_delay))
sum(is.na(data$age))
data <- data %>%
  mutate( age = ifelse(is.na(age),mean_age,age))
#4.	Find the percentage of patients who had cough as a symptom.
names(data)[grep("cough", names(data))]
table(data$current_symptoms_cough)
cough_percentage <- mean(data$current_symptoms_cough) * 100
cough_percentage
round(cough_percentage, 2)
#5.	Create a stacked column chart of number of patients by district and gender.
table(data$district)
table(data$gender)
library(ggplot2)
ggplot(data, aes(x = district, fill = gender)) +
  geom_bar() +
  labs(
    title = "Number of TB Patients by District and Gender",
    x = "District",
    y = "Number of Patients",
    fill = "Gender"
  ) +
  theme_minimal()
#6.	Comment on whether age follows a normal distribution or not.
ggplot(data, aes(x = age)) +
  geom_histogram(binwidth = 5,
                 fill = "skyblue",
                 color = "black") +
  labs(
    title = "Distribution of Age",
    x = "Age",
    y = "Frequency"
  ) +
  theme_minimal()

#Interpretation of the histogram
#From the plot: 
# 1. The ages are not symmetric.
# 2.Most patients are between 15–35 years.
# 3.There is a long tail towards older ages (50–80 years).
# 4.This indicates a positively (right) skewed distribution, rather than a bell-shaped normal distribution.
#So, based on the histogram alone,age does not appear to follow a normal distribution.
#to confirm statistically 
shapiro.test(data$age)
#The Shapiro-Wilk normality test also indicated that the distribution 
   #of age was significantly different from a normal distribution 
      # (W = 0.94581, p < 0.001). 
#Therefore, age does not follow a normal distribution.


#7.	Stratify the number of abnormal parameters in the following way.
#a.	1 abnormal parameter-low-risk
#b.	2-3 abnormal parameters- moderate risk
#c.	4 and greater than 4 abnormal parameters- high risk.
grep("abnormal", names(data), value = TRUE)
data <- data %>%
  mutate(
    risk_category = case_when(
      no_of_abnormal_parameters == 1 ~ "Low Risk",
      no_of_abnormal_parameters >= 2 &
        no_of_abnormal_parameters <= 3 ~ "Moderate Risk",
      no_of_abnormal_parameters >= 4 ~ "High Risk",
      TRUE ~ NA_character_
    )
  )
table(data$risk_category)
#8.	Cross-Tabulate and find out the number of persons stratified by medical officer 
#as being in low-risk despite the number of abnormal parameters being in high risk. 
#Hint: You may have to rename/clean column names to visualize the result properly.
grep("risk", names(data), value = TRUE)
table(data$final_risk_stratification_by_the_medical_officer)
table(
  data$final_risk_stratification_by_the_medical_officer,
  data$risk_category
)

data$mo_risk <- recode(
  data$final_risk_stratification_by_the_medical_officer,
  "Low risk- Marked for provisional follow-up later." = "Low Risk",
  "Moderate Risk- Marked for mandatory follow-up later at CHC/PHC/HWC" = "Moderate Risk",
  "High risk- Referring to District Hospital/Sub-district hospital or tertiary care facility with availability of intensive or emergency or inpatient care." = "High Risk"
)

table(data$mo_risk, data$risk_category)

#9.Create a descending order column chart showing number of persons having following 
  #abnormal parameters-respiratory rate, bmi, haemoglobin, oxygen saturation. 

grep("resp|bmi|haemo|oxygen|spo2", names(data), 
     value = TRUE, ignore.case = TRUE)
table(data$respiratory_rate)
table(data$bmi)
table(data$haemoglobin)
table(data$oxygen_saturation)

# Create a data frame of abnormal parameter counts
abnormal_counts <- data.frame(
  Parameter = c("Respiratory Rate",
                "BMI",
                "Haemoglobin",
                "Oxygen Saturation"),
  Count = c(171, 352, 138, 57)
)

# Arrange in descending order
abnormal_counts <- abnormal_counts %>%
  arrange(desc(Count))

# Plot descending column chart
ggplot(abnormal_counts,
       aes(x = reorder(Parameter, -Count),
           y = Count)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Number of Persons with Abnormal Parameters",
    x = "Clinical Parameter",
    y = "Number of Persons"
  ) +
  theme_minimal()

#Some places where you can browse thousands of color palettes are:
  
  #1. ColorBrewer (excellent for statistical graphics)
  #2. Coolors.co (palette generator)
  #3.Adobe Color
  #4.HTML Color Codes
  #5.RColorBrewer package in R

ggplot(abnormal_counts,
       aes(x = reorder(Parameter, -Count),
           y = Count,
           fill = Parameter)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = c(   
   
     "#C71585",  "#F7A8B8", "#FADADD",  "#EC5F8C"
   
  )) +
  labs(
    title = "Number of Persons with Abnormal Parameters",
    x = "Clinical Parameter",
    y = "Number of Persons"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold"),
    legend.position = "none"
  ) +theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18)
  ) + theme(
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12)
  ) +geom_text(aes(label = Count),
               vjust = -0.3,
               size = 5,
               fontface = "bold")+expand_limits(y = 380)
#final code

ggplot(abnormal_counts,
       aes(x = reorder(Parameter, -Count),
           y = Count,
           fill = Parameter)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = Count),
            vjust = -0.2,
            size = 4,
            fontface = "bold") +
  scale_fill_manual(values = c(
    "#C71585",
    "#E75480",
    "#F48FB1",
    "#FADADD"
  )) +
  labs(
    title = "Number of Persons with Abnormal Parameters",
    x = "Clinical Parameter",
    y = "Number of Persons"
  ) +
  expand_limits(y = 380) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 9),
    axis.text = element_text(face = "bold", size = 7),
    legend.position = "none"
  )

#10.Create a subset of the data set who are males and do not have blood in sputum.

grep("gender|blood|sputum", names(data), value = TRUE, ignore.case = TRUE)
male_no_blood <- subset(
  data,
  gender == "Male" &
    blood_in_sputum == 0
)

nrow(male_no_blood)
head(male_no_blood)

#11.	Anything else you want to report in this dataset. 
#This is the most important question but is optional.

#1. Risk category by district
table(data$district, data$risk_category)
ggplot(data, aes(x = district, fill = risk_category)) +
  geom_bar() +
  scale_fill_manual(values = c(
    "#C71585","#F48FB1","#FADADD"
   
  )) +
  labs(
    title = "Risk Category by District",
    x = "District",
    y = "Number of Patients",
    fill = "Risk Category"
  ) +
  theme_minimal()

#2.Assessment delay by district
delay_by_district <- aggregate(
  assessment_delay ~ district,
  data = data,
  mean,
  na.rm = TRUE
)

delay_by_district$assessment_delay <- round(delay_by_district$assessment_delay, 2)

delay_by_district
library(ggplot2)

ggplot(delay_by_district,
       aes(x = reorder(district, assessment_delay),
           y = assessment_delay,
           fill = district)) +
  
  geom_col(width = 0.7)+
  
  scale_fill_manual(values=c(
    "#C71585",
    "#E75480",
    "#F48FB1",
    "#FADADD"
  ))+
  
  geom_text(aes(label=assessment_delay),
            vjust=-0.4,
            fontface="bold",
            size=4)+
  
  labs(
    title="Average Assessment Delay by District",
    x="District",
    y="Average Delay (Days)"
  )+
  
  theme_minimal(base_size=14)+
  
  theme(
    legend.position="none",
    plot.title=element_text(face="bold",hjust=0.5),
    axis.title=element_text(face="bold"),
    axis.text=element_text(face="bold")
  )
#3.Distribution of Risk Categories
ggplot(data,
       aes(risk_category,
           fill=risk_category))+
  
  geom_bar()+
  
  scale_fill_manual(values=c(
    "#FADADD",
    "#F48FB1",
    "#C71585"
  ))+
  
  geom_text(
    stat="count",
    aes(label=after_stat(count)),
    vjust=-0.4,
    fontface="bold"
  )+
  
  labs(
    title="Distribution of Risk Categories",
    x="Risk Category",
    y="Number of Patients"
  )+
  
  theme_minimal(base_size=14)+
  
  theme(
    legend.position="none",
    plot.title=element_text(face="bold",hjust=0.5),
    axis.title=element_text(face="bold"),
    axis.text=element_text(face="bold")
  )
#4. Age Distribution by Risk Category
ggplot(data,
       aes(risk_category,
           age,
           fill=risk_category))+
  
  geom_boxplot()+
  
  scale_fill_manual(values=c(
    "#FADADD",
    "#F48FB1",
    "#C71585"
  ))+
  
  labs(
    title="Age Distribution across Risk Categories",
    x="Risk Category",
    y="Age"
  )+
  
  theme_minimal(base_size=14)+
  
  theme(
    legend.position="none",
    plot.title=element_text(face="bold",hjust=0.5),
    axis.title=element_text(face="bold"),
    axis.text=element_text(face="bold")
  )
#Moderate Risk patients had the highest median age among all risk categories.
#High Risk and Moderate Risk groups showed greater variability in age compared with the Low Risk group.
#A few older outliers were observed in the Low Risk and NA categories.
#The considerable overlap in age distributions suggests that age alone does not clearly differentiate patients across the risk categories.

#5. Correlation Heatmap of Clinical Parameters

library(corrplot)

vars <- data[,c(
  "age",
  "bmi",
  "respiratory_rate",
  "oxygen_saturation",
  "haemoglobin"
)]

corrplot(cor(vars,use="complete.obs"),
         method="color")

#interpretation 
#Age shows a very weak positive correlation with BMI.
#Age has almost no correlation with respiratory rate, oxygen saturation, or haemoglobin.
#BMI also shows very weak correlations with the remaining variables.
#Respiratory rate, oxygen saturation, and haemoglobin do not appear to have any strong relationships with one another.
#Overall, none of the variables exhibit a strong correlation 
#(no dark blue or dark red cells outside the diagonal).

library(rmarkdown)
library(knitr)
