setwd("/Users/burrisfaculty/Desktop/DSCode/DSCI605/Final Project")
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(tidyr)
library(forecast)
library(lubridate)
library(caTools)
library(caret)
library(jtools)
library(huxtable)

stu_data = read.csv("StudentsPerformance.csv")
stu_data$gender = as.factor(stu_data$gender)
stu_data$parental.level.of.education = as.factor(stu_data$parental.level.of.education)
stu_data$lunch = as.factor(stu_data$lunch)
stu_data$test.preparation.course = as.factor(stu_data$test.preparation.course)
stu_data$race.ethnicity = as.factor(stu_data$race.ethnicity)
#creating dummy variables
#gender 1 = female 0 = male
stu_data$female = ifelse(stu_data$gender == 'female',1,0)
# free/reduced lunch = 1 standard = 0
stu_data$lunch = ifelse(stu_data$lunch == 'standard', 0 , 1)
#test_prep yes = 1 no = 0
stu_data$testprep = ifelse(stu_data$test.preparation.course == 'completed', 1, 0)

#exploratory data analysis

stu_grp_lunch = stu_data %>%
  group_by(lunch) %>%
  summarise(ave_reading = mean(reading.score),
            ave_math = mean(math.score),
            ave_writing = mean(writing.score))
View(stu_grp_lunch) 

stu_grp_lunch2 = stu_data %>%
  group_by(lunch) %>%
  summarise(med_reading = median(reading.score),
            med_math = median(math.score),
            med_writing = median(writing.score))
View(stu_grp_lunch2) 
plot1 <- ggplot(stu_data, aes(x = math.score, y = reading.score)) +
  geom_point(aes(color = as.factor(lunch))) + 
  scale_color_manual( name = 'Lunch Status',
                      labels = c('Full Price','Free/Reduced'),values = c('goldenrod4','darkorchid4')) +
  ggtitle("Reading Score vs Math Score") + xlab("Math Score") + ylab("Reading Score") +
  theme(plot.background =  element_rect(fill = 'azure'),
        panel.background =element_rect(fill = 'white'),
        legend.position = 'bottom',
        legend.box = 'vertical',
        legend.key = element_rect(fill = 'white'),
        legend.background = element_rect(colour = 'black'),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = 'lightgrey', linetype = 'dotted'),
        axis.line = element_line(),
        axis.text = element_text(colour = 'black'))

plot1
plot2 <- ggplot(stu_data, aes(x = math.score, y = writing.score)) +
  geom_point(aes(color = as.factor(lunch))) + 
  scale_color_manual( name = 'Lunch Status',
                      labels = c('Full Price','Free/Reduced'),values = c('goldenrod4','darkorchid4')) +
  ggtitle("Writing Score vs Math Score") + xlab("Math Score") + ylab("Writing Score") +
  theme(plot.background =  element_rect(fill = 'azure'),
        panel.background =element_rect(fill = 'white'),
        legend.position = 'bottom',
        legend.box = 'vertical',
        legend.key = element_rect(fill = 'white'),
        legend.background = element_rect(colour = 'black'),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = 'lightgrey', linetype = 'dotted'),
        axis.line = element_line(),
        axis.text = element_text(colour = 'black'))
plot2

plot3 <- ggplot(stu_data, aes(x = as.factor(lunch), y = math.score)) + geom_boxplot() +
  ggtitle("Math Scores by Lunch Status") + xlab("Lunch Status") + ylab("Math Score") +
  theme(plot.background =  element_rect(fill = 'azure'),
        panel.background =element_rect(fill = 'white'),
        legend.position = 'bottom',
        legend.box = 'vertical',
        legend.key = element_rect(fill = 'white'),
        legend.background = element_rect(colour = 'black'),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = 'lightgrey', linetype = 'dotted'),
        axis.line = element_line(),
        axis.text = element_text(colour = 'black')) +
  scale_x_discrete(labels=c("0" = "Full Price", "1" = "Free/Reduced"))
plot3

plot4 <- ggplot(stu_data, aes(x = parental.level.of.education, y = math.score)) + geom_boxplot() +
  ggtitle("Math Scores by Parent Level of Education") + xlab("Parent Level of Education") + ylab("Math Score") +
  theme(plot.background =  element_rect(fill = 'azure'),
        panel.background =element_rect(fill = 'white'),
        legend.position = 'bottom',
        legend.box = 'vertical',
        legend.key = element_rect(fill = 'white'),
        legend.background = element_rect(colour = 'black'),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = 'lightgrey', linetype = 'dotted'),
        axis.line = element_line(),
        axis.text = element_text(colour = 'black'),
        axis.text.x = element_text(angle = -30)) +
  scale_x_discrete(name ="Level of Education", 
                   limits=c("some high school","high school","some college", 
                            "associate's degree", "bachelor's degree","master's degree"),
                   labels =c("some high school" = 'Some HS',"high school" = "HS Diploma",
                             "some college" = "Some College", 
                             "associate's degree" = "Associate's", "bachelor's degree" = "Bachelor's",
                             "master's degree" = "Master's  or Higher") )
  
plot4

plot5 <- ggplot(stu_data, aes(x= race.ethnicity, y = math.score)) + 
  geom_boxplot() +
  ggtitle("Math Scores by Race/Ethnicity") +
   xlab("Race/Ethnicity") + ylab("Math Score") +
  theme(plot.background =  element_rect(fill = 'azure'),
        panel.background =element_rect(fill = 'white'),
        legend.position = 'bottom',
        legend.box = 'vertical',
        legend.key = element_rect(fill = 'white'),
        legend.background = element_rect(colour = 'black'),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = 'lightgrey', linetype = 'dotted'),
        axis.line = element_line(),
        axis.text = element_text(colour = 'black'),
        axis.text.x = element_text(angle = -30)) 
plot5


plot6 <- ggplot(stu_data, aes(math.score,reading.score)) + geom_point(aes(color = as.factor(lunch)) )+
  facet_grid(rows = vars(race.ethnicity),cols = var(parental.level.of.education)) +
  theme(plot.background =  element_rect(fill = 'azure'),
        panel.background =element_rect(fill = 'white'),
        legend.position = 'bottom',
        legend.box = 'vertical',
        legend.key = element_rect(fill = 'white'),
        legend.background = element_rect(colour = 'black'),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = 'lightgrey', linetype = 'dotted'),
        axis.line = element_line(),
        axis.text = element_text(colour = 'black'),
        axis.text.x = element_text(angle = -30)) +
  ggtitle("Facet Grid Reading vs Math") + xlab("Math Score") + ylab("Reading Score") +
  scale_color_manual( name = 'Lunch Status',
                      labels = c('Full Price','Free/Reduced'),values = c('goldenrod4','darkorchid4'))
plot6

# Test-train split
set.seed(47303)
sample <- sample.split(stu_data$lunch, SplitRatio = 0.7)
train  <- subset(stu_data, sample == TRUE)
test   <- subset(stu_data, sample == FALSE)
#Create Logistic Model
model <- glm(lunch ~ math.score + reading.score + writing.score +as.factor(female),family=binomial(link='logit'),data=train)
summary(model)
summ(model)



model2 <- glm(lunch ~ math.score + reading.score + writing.score ,family=binomial(link='logit'),data=train)
summary(model2)
export_summs(model, model2)

#Analyze Predictions

fitted.results <- predict(model,newdata=test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

confusionMatrix(as.factor(test$lunch), as.factor(fitted.results))
misClasificError <- mean(fitted.results != test$lunch)
print(paste('Accuracy',1-misClasificError))


#Create plot
pred_plot <- ggplot(test, aes(x=math.score + writing.score +reading.score, y=lunch)) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se=TRUE, method.args = list(family=binomial), col = "cadetblue") +
  ggtitle("Logistic Regression Curve", subtitle = "Lunch: 0 = Full Price and 1 = Free/Reduced") + xlab("Factors: Math, Reading, and Writing Scores") +
  ylab("Lunch") +
  theme(plot.background =  element_rect(fill = 'azure'),
        panel.background =element_rect(fill = 'white'),
        legend.position = 'bottom',
        legend.box = 'vertical',
        legend.key = element_rect(fill = 'white'),
        legend.background = element_rect(colour = 'black'),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = 'lightgrey', linetype = 'dotted'),
        axis.line = element_line(),
        axis.text = element_text(colour = 'black')) 
pred_plot
