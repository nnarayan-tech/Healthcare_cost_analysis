HospitalCosts <- read.csv(file.choose(),header = TRUE)
View(HospitalCosts)
head(HospitalCosts)
colnames(HospitalCosts)
str(HospitalCosts)
dim(HospitalCosts)
summary(HospitalCosts)

# To record the patient statistics, the agency wants to find the age category of people who frequent the hospital and has the maximum expenditure.
table(HospitalCosts$AGE)
hist(HospitalCosts$AGE)
library(ggplot2)
ggplot(data = HospitalCosts, aes(x= AGE))+
geom_histogram(binwidth = 2, fill="green", color="black")+
ggtitle("Patients Frequency")
max(table(HospitalCosts$AGE))
max(summary(as.factor(HospitalCosts$AGE)))
which.max(table(HospitalCosts$AGE))
age <- aggregate(TOTCHG ~ AGE,data = HospitalCosts, sum)
max(age)

#In order of severity of the diagnosis and treatments and to find out the expensive treatments, the agency wants to find the diagnosis related group that has maximum hospitalization and expenditure.
t <- table(HospitalCosts$APRDRG)
diagnoscost <- as.data.frame(t)
names(diagnoscost)[1] = 'New Diagnosis Group'
diagnoscost
which.max(table(HospitalCosts$APRDRG))
which.max(t)
diagnoscost2 <- aggregate(TOTCHG ~ APRDRG, data = HospitalCosts,sum)
diagnoscost2
which.max(diagnoscost2$TOTCHG)
diagnoscost2[which.max(diagnoscost2$TOTCHG),]

#To make sure that there is no malpractice, the agency needs to analyze if the race of the patient is related to the hospitalization costs.
table(HospitalCosts$RACE)
HospitalCosts <- na.omit(HospitalCosts)
HospitalCosts$RACE <- as.factor(HospitalCosts$RACE)
anova_model <- aov(TOTCHG ~ RACE, data = HospitalCosts)
anova_model 
summary(anova_model)
summary(HospitalCosts$RACE)

#To properly utilize the costs, the agency has to analyze the severity of the hospital costs by age and gender for proper allocation of resources.
table(HospitalCosts$FEMALE)
HospitalCosts$FEMALE <- as.factor(HospitalCosts$FEMALE)
lin_model <- lm(TOTCHG ~ AGE+FEMALE, data = HospitalCosts)
summary(lin_model)
summary(HospitalCosts$FEMALE)

#Since the length of stay is the crucial factor for inpatients, the agency wants to find if the length of stay can be predicted from age, gender, and race.
HospitalCosts$RACE <- as.factor(HospitalCosts$RACE)
lin_model2 <- lm(TOTCHG ~ AGE+FEMALE +RACE+LOS, data = HospitalCosts) 
summary(lin_model2)

#To perform a complete analysis, the agency wants to find the variable that mainly affects the hospital costs.
aov(TOTCHG ~., data = HospitalCosts)
Lin_model3<- lm(TOTCHG ~.,data = HospitalCosts)
summary(Lin_model3)



