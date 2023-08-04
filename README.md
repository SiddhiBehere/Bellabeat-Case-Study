
# Google Data Analytics Capstone: Bellabeat Case Study

## Scenario
You are a junior data analyst working on the marketing analyst team at Bellabeat, a high-tech manufacturer of
health-focused products for women. Bellabeat is a successful small company, but they have the potential to become a larger
player in the global smart device market. Urška Sršen, cofounder and Chief Creative Officer of Bellabeat, believes that
analyzing smart device fitness data could help unlock new growth opportunities for the company. You have been asked to
focus on one of Bellabeat’s products and analyze smart device data to gain insight into how consumers are using their smart
devices. The insights you discover will then help guide marketing strategy for the company. You will present your analysis to
the Bellabeat executive team along with your high-level recommendations for Bellabeat’s marketing strategy.

## Ask
**Goal:** To gain insight into how consumers are using their smart devices so the insights that will be discovered will help guide marketing strategy for Bellabeat.

## Prepare
[Data source ](https://www.kaggle.com/datasets/arashnic/fitbit)

This Kaggle data set contains information of personal fitness tracker from thirty fitbit users.

The data is organized in long format.

The data is Reliable, Original, Comprehensive and Cited.

Data Limitations:
- Limited sample size.
- Collected in 2016 i.e. data is not current.
- Limited descriptive data(age, sex, lifestyle, etc.)

## Process
Examine the data for any NULL values and duplicate values from dailyActivity_merged, sleepDay_merged, weightLogInfo_merged.

Convert dates from dailyActivity_merged, sleepDay_merged, weightLogInfo_merged tables to mm-dd-yyyy and convert dates to weekdays.

The dataset has:
- 33 users from dailyActivity_merged.
- 24 users from sleepDay_merged.
- 8 users from weightLogInfo_merged.

## Analyze 
**Summary:** 

```r
daily_activity <- read.csv("dailyActivity_merged.csv")
weight_data <- read.csv("weightLogInfo_merged.csv")
sleep_data <- read.csv("sleepDay_merged.csv")
merge_weightAndSleep <- merge(sleep_data, weight_data, by = c("Id"), all=TRUE)
merge_data <- merge(daily_activity, merge_weightAndSleep, by = c("Id"), all=TRUE)
merge_data %>%
  summary()
```

On an avg total steps taken are 9373 (while max is 36019 which is almost 4 times the avg steps) and distance covered is 6.415km.

It can be observed that users spent on an avg 12hrs sedentary almost 4hrs lightly active. Almost half an hour fairly active and almost half an hour very active.

Avg weight is 63.32kgs with calories burn of 2103 and BMI of 24.42

Users get on an avg 7hrs of sleep.

**Findings:**

*Total steps vs calories:*
```r
ggplot(data = daily_activity, mapping = aes(x = TotalSteps, y = Calories, color=SedentaryMinutes)) +
  geom_point()+
  stat_smooth(method=lm)+
  labs(title="Total Steps vs Calories")
```
![alt text](https://github.com/[username]/[reponame]/blob/[branch]/image.jpg?raw=true)

It would be expected that if user spends most of their time sedentary then their calorie burned must be less but turns out its not true for this data. Here it can be seen even the users who spend most of their time sedentary had also burnt out around 1,500 to 3000 calories compare to users who are more active.

*Total steps vs Total distance:*
```r
ggplot(data = daily_activity, mapping = aes(x = TotalSteps, y = TotalDistance)) +
  geom_point()+
  stat_smooth(method=lm)+
  labs(title="Total Steps vs Total Distance")
```
![alt text](https://github.com/[username]/[reponame]/blob/[branch]/image.jpg?raw=true)

More the number of steps, more the distance covered.

*Hourly steps:*
```r
hourlySteps <- hourlySteps %>%
  group_by(Id, ActivityHour) %>%
  summarise(sum_steps = mean(StepTotal)) %>%
  arrange(ActivityHour)

ggplot(data = hourlySteps, aes(x=ActivityHour, y=sum_steps, fill=ActivityHour)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45))+
  labs(title = "Hourly Steps")
```
![alt text](https://github.com/[username]/[reponame]/blob/[branch]/image.jpg?raw=true)

It is clear that most steps are taken between 5PM to 7PM.

*Steps according to weekdays:*
```r
dailyActivity_week <- dailyActivity %>%
  select(Weekday, TotalSteps) %>%
  group_by(Weekday) %>%
  summarise(Total_steps = mean(TotalSteps)) %>%
  arrange(is.character(Weekday))

ggplot(data = dailyActivity_week, aes(x=Weekday, y=Total_steps, fill=Weekday)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45))+
  labs(title = "Steps according to weekdays")
```
![alt text](https://github.com/[username]/[reponame]/blob/[branch]/image.jpg?raw=true)

It is interesting to find out that Saturday is most active since its avg steps are more and Sunday is most lazy day due to its avg step count.

*Steps vs sleep:*
```r
mergeStepsSleep <- merge(dailyActivity, sleepData, by = c("Id", "ActivityDate"), all=TRUE)
View(mergeStepsSleep)

ggplot(data = mergeStepsSleep, aes(x=TotalSteps, y=TotalMinutesAsleep)) +
  geom_point() +
  stat_smooth(method=lm)+
  theme(axis.text.x = element_text(angle = 45))+
  labs(title = "Steps vs Sleep")
```
![alt text](https://github.com/[username]/[reponame]/blob/[branch]/image.jpg?raw=true)

One would expect a direct relation between sleep and total steps. But it can be seen that there is an indirect relation i.e. more steps, less sleep and this trend can be seen from the slope.

*Sleep vs Calories:*
```r
ggplot(data = mergeStepsSleep, aes(x=TotalMinutesAsleep, y=Calories)) +
  geom_point() +
  stat_smooth(method=lm)+
  theme(axis.text.x = element_text(angle = 45))+
  labs(title = "Sleep vs Calories")
```
![alt text](https://github.com/[username]/[reponame]/blob/[branch]/image.jpg?raw=true)

There is no relation between sleep and calories burned.

*Very active minutes vs Calories:*
```r
ggplot(data = dailyActivity, aes(x=VeryActiveMinutes, y=Calories)) +
  geom_point() +
  stat_smooth(method=lm)+
  theme(axis.text.x = element_text(angle = 45))+
  labs(title = "Very active minutes vs calories")
```
![alt text](https://github.com/[username]/[reponame]/blob/[branch]/image.jpg?raw=true)

Most of the calories are burnt in active minutes only.

*Time in Bed and Time asleep:*
```r
timeAwake <- mutate(sleep_data, awakeTime = TotalTimeInBed - TotalMinutesAsleep)

timeAwake <- timeAwake %>%
  select(Id, awakeTime) %>%
  filter(awakeTime >= 30) %>%
  group_by(Id)

timeAwake <- timeAwake %>%
  summarize(countTime = n_distinct(Id))

count(timeAwake)

timeAwake <- mutate(sleep_data, awakeTime = TotalTimeInBed - TotalMinutesAsleep)

timeAwake <- timeAwake %>%
  select(Id, awakeTime) %>%
  filter(awakeTime >= 45) %>%
  group_by(Id)

timeAwake <- timeAwake %>%
  summarize(countTime = n_distinct(Id))

count(timeAwake)
```
There are total 17 users who are awake in bed for 30 mins or more without sleeping.
Out of 17 users, 15 users spend 45 mins in bed not asleep.

## Share
[Dashboard](https://public.tableau.com/app/profile/siddhi.behere/viz/Bellabeatcasestudy_16896712052100/DashboardofBellabeatcasestudy)

![ppt](https://github.com/[username]/[reponame]/blob/[branch]/image.jpg?raw=true)

## Act
*Conclusion:*
- 62.5% users spend 45 mins in bed awake.
- On an avg, users spend most time sedentary(almost 991.2 minutes)
- Sunday can be considered a lazy day due to less step count.
- Users take more steps between 5PM to 7PM.

*Recommendations:*
- Bellabeat time product can vibrate after sensing prolonged Sendentary minutes.
- Bellabeat app can have some videos of yoga instructors or fitness trainers on short exercises that can keep the users active and fit.
- Bellabeat app can have a streak system. Whenever user completes her daily steps and short exercises, a notification like “streak points added” can pop up. This will be helpful for days with less step count like Sunday.
- These streak points can be later redeemed on bellabeat products/memberships.
- Along with the streak system, top three users with highest step count will receive some gift vouchers/gift cards from bellabeat. In this way most of the users will be encouraged to exercise.
- Bellabeat app can also pop up some positive notifications like “keep it up”  to keep the users motivated.
