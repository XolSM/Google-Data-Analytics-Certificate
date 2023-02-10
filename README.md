# Bellabeat - How Can a Wellness Technology Company Play It Smart?

This is my case study analysis for the Google Data Analysis Certificate Capstone and final course.

## 📋 [Executed Analysis Report](https://rpubs.com/XolSM/Google-Data-Analytics-Certificate-Bellabeat-Case-Study)

## 📊 [Presentation for Stakeholders](https://github.com/XolSM/Google-Data-Analytics-Certificate/blob/2673866dde65ab5f3737546ed7673cce85d6d8cb/Case%20Study%202-%20Bellabeat%20XSM.pdf)

Bellabeat is a high-tech manufacturer of health-focused products for women. For this case study, the business task was to identify tendencies in the consumers usage of non-Bellabeat fitness smart devices with relevance to applicable trends for Bellabeat customers in order to help influence Bellabeat marketing strategy, taking into account that these trends could also help improve Bellabeat products.

Using available non-Bellabeat smart device fitness data, the aim is to get knowledge about how consumers are using their smart devices in order to use these insights to help guide the marketing strategy for the company. 

Content:
* [Dataset](#dataset)
* [Data Exploration](#data-exploration)
     - [Loading the Dataset's CSV files](#loading-datasets)
     - [Exploring "n"](#exploring-n)
* [Data Cleaning](#data-cleaning)
     - [Sumarizing Data](#sumarizing-data)
     - [Transforming Dates](#transforming-dates)
     - [Dataframes horizontal merging](#horizontal-merge)
* [Analizing and sharing data - Looking for and visualizing correlations and trends](#analizing-data)
     - [Devices Usage](#devices-usage)
     - [User Types](#user-types)
     - [Active days trend exploration](#active-days)
     - [Active hours trend exploration](#active-hours)
     - [Activity (total minutes) vs Sleep (total minutes) relationship exploration](#activity-vs-sleep)
     - [Time Asleep vs Time in Bed](#asleep-vs-bed)
* [Business Recommendations](#recommendations)

<a id="dataset"></a>
## Dataset

DataSet source:
Furberg, R., Brinton, J., Keating, M., & Ortiz, A. (2016). Crowd-sourced Fitbit datasets 03.12.2016-05.12.2016 Zenodo. [Data set](https://doi.org/10.5281/zenodo.53894)
Obtained through FitBit Fitness Tracker Data (CC0: Public Domain, dataset made available through Mobius): This Kaggle data set contains personal fitness tracker from thirty fitbit users. Thirty eligible Fitbit users consented to the submission of personal tracker data, including minute-level output for physical activity, heart rate, and sleep monitoring. It includes information about daily activity, steps, and heart rate that can be used to explore users’ habits.

As a good practice, we need to check if the data is: Reliable, Original, Comprehensive, Current and Cited. 
We can see that the informartion is well cited and license is clearly open source, and we can also assume that the data is original as the autors published, however, we have other issues with our data:
* They state that the information was obtained from "thirty eligible Fitbit users", however, they don't clarify the eligibility criteria for these users nor the demography, so this group can include few or no women at all, which are our company's target consumer.
* It's definitely not current as this data is dated 2016 (we can assume for the sake of the Case Study that the data is current enough). 
This data itself doesn't seem to be "Good Data" to base an analysis to make data-driven business decisions, however, we will continue the analysis as per the Case Study as if these issues were discussed with the client we are using it to perform a preliminar analysis to get to know a dataset for this type of devices and where we can focus the data search or the methods to obtain data.

<a id="data-exploration"></a>
## Data exploration
Previewing the data as a spreadsheet we started to think about which data could be usefull for our bussiness task while also viewing data format and Columns, and selected the files that contain the most useful information to make broaden but handy conjectures, so we have excluded information taken by the minute or second as this would be useful to search for trends for a person but not so much for a general population, and also excluded from the analysis information like Calories or METs as these depends of metabolic differences for each individual. We also noticed that the information listed in the files dailyCalories, dailyIntensities and dailySteps is summarized in the dailyActivity Dataframe; lastly we can see that the weightLogInfo document only has 67 observations. Therefore, the following is the data that we will be using in this analysis:
* dailyActivity
* hourlySteps
* sleepDay

In a more detailed review of each DataFrame, we check for number of categories or labels, the different data types, if there is any that needs clarification and if there is any unformatted data or different formats, finding that is necessary to clarify the following in accordance to the [fitabase data dictionary](https://www.fitabase.com/media/1930/fitabasedatadictionary102320.pdf):

* Intensity has for value levels as follows: 0 = Sedentary 1 = Light 2 = Moderate 3 = Very Active.
* Sedentary behaviour is defined as time spent sitting or lying with low energy expenditure, while awake, in the context of occupational, educational, home and community settings and transportation [(WHO Definition)](https://www.who.int/docs/default-source/physical-activity/call-for-consultation/draft-guideline-on-physical-activity-and-sedentray-behaviour.pdf?sfvrsn=ddf523d5_4).
* Total Intensity: Value calculated by adding all the minute-level intensity values that occurred within the hour.
* Average Intensity: Average intensity state exhibited during that hour (TotalIntensity for that ActivityHour divided by 60).

We also observed that, except by the dailyActivity file, all other tables contain Date and Time concatenated in one column with a 12 hour format, so we know we will need to transform this later if we need to make some calculations or correlations taking date and/or time in consideration.

Based on the size of the DataSet and the need of a thorough documentation, and taken into consideration issues with the Date columns that have arised in BigQuery, we decided to use R for this task.

Now, lets load the needed libraries:
```{r Preparation for Bussiness Task, echo=TRUE}
install.packages("readr")
install.packages("tidyr")
install.packages("dplyr")
install.packages("skimr")
install.packages("janitor")
install.packages("ggplot2")
install.packages("ggpubr")

library(readr)
library(tidyr) 
library(dplyr) 
library(skimr)
library(janitor)
library(ggplot2)
library(ggpubr)
```

<a id="loading-datasets"></a>
### Loading the Dataset's CSV files

So we now proceed to load the data that we have previously selected:
```{r Load Data, echo=TRUE, include=FALSE}
dailyActivity <- read_csv("Fitabase Data 4.12.16-5.12.16/dailyActivity2.csv")
hourlySteps <- read_csv("Fitabase Data 4.12.16-5.12.16/hourlySteps2.csv")
dailySleep <- read_csv("Fitabase Data 4.12.16-5.12.16/sleepDay2.csv")
```
 
We can see that this data contains DataFrames with different sizes as the number of observations varies if the data is logged by the day or hour, as well as if is logged manually:
* dailyActivity have 940 rows
* hourlySteps have 22,099 rows
* dailySleep have 413 rows.

<a id="exploring-n"></a>
### Exploring "n"
The Data Source didn't clarify why do we have more observations for some items than for others, for which we will check for the amount of observations registered by dates as "n" for each Id, assuming that 1 Id= 1 specific fitbit user. I will only use the DataFrames that contain observations that are expected daily: dailyActivity, sleepDay.

```{r, echo=TRUE}
print(dailyActivity %>%
        select(ActivityDate, Id) %>%
        group_by(Id) %>%
        summarize(n = n()) %>%
        arrange(n), n=50)

print(dailySleep %>%
        select(SleepDay, Id) %>%
        group_by(Id) %>%
        summarize(n = n()) %>%
        arrange(n), n=50)
```

As we can see, there were 33 fitbit Ids for this data collection for 31 days from April 12th to May 12th of 2016, and more than half of this population sample used the devices for the whole month; however, for dailySleep we only have information registered by 24 users and only 15 of them registered at least 15 observations (half of the trial month), for which it doesn't seem to be information that the participants usually log.

<a id="data-cleaning"></a>
## Data Cleaning 
Checking for missing or null data:
```{r, echo=TRUE}
any(is.na(dailyActivity))
any(is.na(hourlySteps))
any(is.na(dailySleep))
```

Now, lets check for duplicated data to guarantee all rows corresponds to individual observations:
```{r, echo=TRUE}
sum(duplicated(dailyActivity))
sum(duplicated(hourlySteps))
sum(duplicated(dailySleep))
```

As we have some duplicated data, we need to clean the Dataframes to only keep unique data as follows:
```{r}
dailySleep <- dailySleep %>%
  distinct() %>%
  drop_na()


#To verify that there are no duplicates now:
sum(duplicated(dailySleep))
```
<a id="sumarizing-data"></a>
### Sumarizing data
We can now take a look at some basics statistics for our data:
```{r}
head(dailyActivity)
dailyActivity %>%  
  select(TotalSteps,
         TotalDistance,
         LoggedActivitiesDistance,
         VeryActiveMinutes,
         FairlyActiveMinutes,
         LightlyActiveMinutes,
         SedentaryMinutes) %>%
  summary()

head(dailySleep)
dailySleep %>%  
  select(TotalSleepRecords,
         TotalMinutesAsleep,
         TotalTimeInBed) %>%
  summary()
```

Now we have a more comprehensive panorama of the data and we can draw the following observations:

* For starters we can see that this sample in average reach the mean of the recommended 7,500 daily steps at least of [](https://www.medicinenet.com/how_many_steps_a_day_is_considered_active/article.htm).
* As per the Logged Activities Distance we can see that these users don't usually lodge many walkings.
* The Sedentary Minutes are higher than the rest of the activity levels minutes, as it includes the Total Minutes Asleep (for a total of 1440 minutes a Day).
* The mean of these users Total Minutes Asleeps gives a total of about 7h/day.
* There is a slightly higher amount of time expend in bed than the time asleep.

<a id="transforming-dates"></a>
### Tranforming dates
We can now also consider that the best way to search for trends is to pair and merge the daily records in one table, but we need our data to be consistent between datasets before merging or comparing, so we will transform the date format in the dailySleep dataframe that have MM/DD/YY HH:MM AM/PM format, and as all data has the same time stamp (12:00 AM) we will only keep the date as relevant information. We will also change the date column names to be consistent between dailyActivity and dailySleep.

```{r }
# Renaming and formatting date column from dailyActivity
dailyActivity <- dailyActivity %>%
  rename(Date = ActivityDate) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))
head(dailyActivity)

# dailySleep column SleepDay from a Date&Time format to Date
dailySleep <- dailySleep %>%
  rename(Date = SleepDay) %>%
  mutate(Date = as.Date(Date,format = "%m/%d/%Y %I:%M:%S %p"))
head(dailySleep)

# Formatting ActivityHour column from hourlySteps
hourlySteps <- hourlySteps%>%
  rename(DateTime = ActivityHour) %>%
  mutate(DateTime = as.POSIXct(DateTime,format ="%m/%d/%Y %I:%M:%S %p" , tz=Sys.timezone()))
hourlySteps$Date <- as.Date(hourlySteps$DateTime)
hourlySteps$Time <- format(hourlySteps$DateTime,"%H:%M:%S")
head(hourlySteps)
```
<a id="horizontal-merge"></a>
### Dataframes horizontal merging
Now, we can merge dailyActivity and dailySleep using id and date as their primary keys using a full join as we know that dailyActivity have observations about more users than dailySleep.
```{r }
dailyActivityAndSleep <- merge(dailyActivity, dailySleep, by=c ("Id", "Date"), all=TRUE)
glimpse(dailyActivityAndSleep)
```
<a id="analizing-data"></a>
## Analizing and sharing data - Looking for and visualizing correlations and trends

Now we can explore the data and find different types of users, and some different trends, like if there are more active days, or more active hours, relationships between activity and sleep and time asleep and time in bed. 

<a id="devices-usage"></a>
### Devices Usage
First, we would like to know or at least estimate how often or how much these users wore their devices. As we saw previously, more than half of the users in this sample used their devices for the whole month (31 days).
```{r}
#Usage
dailyActivityAndSleep$ActivityMinutesTotal <- dailyActivityAndSleep$VeryActiveMinutes + 
    dailyActivityAndSleep$FairlyActiveMinutes + 
    dailyActivityAndSleep$LightlyActiveMinutes + 
    dailyActivityAndSleep$SedentaryMinutes

#Check the distribution of ActivityMinutesTotal, confirm that max is 1440 minutes or less (total minutes in a day)
dailyActivityAndSleep %>%
  select(ActivityMinutesTotal) %>%
  summary()

#Check that TotalMinutesAsleep < SedentaryMinutes
dailyActivityAndSleep %>%
  select(TotalMinutesAsleep, SedentaryMinutes) %>%
  summary()
```

We can confirm now what the logics tells us: 
* The sum of Minutes in each activity intensity is equal to or less than the amount of minutes in a day (1440).
* Sedentary Minutes most include the minutes asleep and therefore it can't be less than the Minutes Asleep.

We can also see that there are many NA's in the TotalMinutesAsleep column, suggesting that more users wore the devices to track Daily Activity rather than to track Daily Sleep, we can confirm this with the following chart:
```{r, echo=FALSE}
dailyActivityAndSleepN <- dailyActivityAndSleep %>% 
    group_by(Date) %>% 
    summarise(TotalMinutesAsleepN = sum(!is.na(TotalMinutesAsleep)), TotalStepsN = sum(!is.na(TotalSteps)))

#With a general n of 33 as there were 33 users included in this dataset
dailyActivityAndSleepPercent <- dailyActivityAndSleepN %>% 
   group_by(Date) %>% 
    summarise(TotalMinutesAsleepPercent = (TotalMinutesAsleepN / 33)*100, TotalStepsNPercent = (TotalStepsN / 33)*100)
head(dailyActivityAndSleepPercent)

#Graphically
ggarrange(
    ggplot(dailyActivityAndSleepPercent) +
      geom_col(aes(Date, TotalMinutesAsleepPercent), fill = "#006699") +
      geom_hline(yintercept = 50) +
      labs(title = "Sleep Registers per Date", x= "", y = "") +
      theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust = 1)),
    ggplot(dailyActivityAndSleepPercent) +
      geom_col(aes(Date, TotalStepsNPercent), fill = "#85e0e0") +
      geom_hline(yintercept = 50) +
      labs(title = "Steps Registers per Date", x= "", y = "") +
      theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust = 1))
    )
```

From this table and graphics we can confirm that the majority of users tracks their Activity but not their Sleep, as we have about 75-100% of Activity registered per day vs about 30-50%  of Sleep, which seems to be pretty consistent through the month.

<a id="user-types"></a>
### User Types
Now, we can try to divide type of users per Steps given in a Day, according to the [literature](https://www.medicinenet.com/how_many_steps_a_day_is_considered_active/article.htm) most Pedometers classify activity as follows:

* Sedentary: Less than 5,000 steps daily; we will assign number "0"
* Low active: About 5,000 to 7,499 steps daily; we will assign number "1"
* Somewhat active: About 7,500 to 9,999 steps daily; we will assign number "2"
* Active: More than 10,000 steps daily; we will assign number "3"
* Highly active: More than 12,500 steps daily; we will assign number "4"

Having a recommended amount of steps between 7,500 to 10,000 steps (Somewhat active category)
```{r}
usersPerStepsADay <- dailyActivityAndSleep %>% 
    select(Id, TotalSteps) %>%
    group_by(Id) %>%
    summarise(MTotalSteps = mean(TotalSteps)) %>%
    arrange(MTotalSteps)

usersPerStepsADay$UserType <- ifelse(usersPerStepsADay$MTotalSteps<5000, 0,
       ifelse(usersPerStepsADay$MTotalSteps>=5000 & usersPerStepsADay$MTotalSteps<7500,1,
              ifelse(usersPerStepsADay$MTotalSteps>=7500 & usersPerStepsADay$MTotalSteps<10000,2,
                     ifelse(usersPerStepsADay$MTotalSteps>=10000 & usersPerStepsADay$MTotalSteps<12500,3,
              4))))
head(usersPerStepsADay)

#Graphically
usersPerStepsADay %>%
group_by(UserType) %>%
summarise(Total = n()) %>%
mutate(Totals = sum(Total)) %>%
group_by(UserType) %>%
summarise(TotalPercent = Total / Totals) %>%
ggplot(aes(UserType,y=TotalPercent, fill=UserType)) +
    geom_col()+
    scale_y_continuous(labels = scales::percent) +
    theme(legend.position="none") +
    labs(title="User Types According to Daily Steps", x="User Type") +
    theme(legend.position="none", text = element_text(size = 15),plot.title = element_text(hjust = 0.5))
```

So here we can see that more than half of the Fitbit users in this sample fit in the categories of "Low active" and "Somewhat active" with more than 25% of users in each category, followed by "Sedentary" users, "Active" and "Highly active" users. Due to the data bias discussed previously this is not a good indicator of a hard trend, however, this may indicate that users trying to be more active are using the devices more.

<a id="active-days"></a>
### Active days trend exploration
We can move on now to some trends exploration with this data, as it would be useful to know if there are days that can be more active than others.
```{r}
weekdayActivity <- dailyActivityAndSleep %>%
  mutate(Weekday = weekdays(Date))

weekdayActivity$Weekday <- ordered(weekdayActivity$Weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday",
"Friday", "Saturday", "Sunday"))

weekdayActivity <- weekdayActivity %>%
  group_by(Weekday) %>%
  summarize (MTotalSteps = mean(TotalSteps)) 
weekdayActivity %>%
ggplot(aes(Weekday,y=MTotalSteps, fill=Weekday)) +
    geom_col()+
    theme(legend.position="none") +
    labs(title="Daily Steps Means by Weekday", x="Weekday") +
    theme(legend.position="none", text = element_text(size = 15),plot.title = element_text(hjust = 0.5))
head(weekdayActivity)
```

We can see in the above graphic that although Tuesdays and Saturdays seems to have a slightly higher TotalSteps Mean or Average and Sundays are a bit lower, there is no significant different among days of the week. In this graphic we can also see how the mean of each day is around the minimum recommended 7,500 daily steps, except on Sundays.

<a id="active-hours"></a>
### Active hours trend exploration
In this same line of tren exploration, we can also look for a peak of activity per hours.
```{r}
hourlySteps %>%
  group_by(Time) %>%
  summarize (MStepTotal = mean(StepTotal)) %>%
    ggplot(aes(Time,y=MStepTotal, fill=Time)) +
        geom_col() +
        theme(legend.position="none") +
        labs(title="Daily Steps Means by Hour", x="") +
        theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust = 1), legend.position="none", text = element_text(size = 15),plot.title = element_text(hjust = 2))
```

According to this graphic there are times were we can see a slightly elevated Mean in the Steps/hour, which match the usual hours for lunch (12:00 - 14:00) and work exit times (17:00 - 19:00), dropping at 20:00 when users are expected to be home.

<a id="activity-vs-sleep"></a>
### Activity (total minutes) vs Sleep (total minutes) relationship exploration
Now, we can check if there is a relationship between activity and sleep.

First, we can explore if there is a User Type that clearly sleeps more than the rest, however, as the amount of data is limited (we only have half the data for dailySleep in regards to dailyActivity), if we can see a trend, it wouldn't be very conclusive.
```{r}
dailyActivityUserVsSleep <- merge(dailyActivity, dailySleep, by=c ("Id", "Date")) #Merging Databases excluding Nulls from dailySleep

dailyActivityUserVsSleep <- dailyActivityUserVsSleep %>% 
    select(Id, 
           TotalSteps, 
           TotalMinutesAsleep) %>%
    group_by(Id) %>%
    summarise(MTotalSteps = mean(TotalSteps), 
              MTotalMinutesAsleep = mean(TotalMinutesAsleep))

dailyActivityUserVsSleep$UserType <- ifelse(dailyActivityUserVsSleep$MTotalSteps<5000, 0,
       ifelse(dailyActivityUserVsSleep$MTotalSteps>=5000 & dailyActivityUserVsSleep$MTotalSteps<7500,1,
              ifelse(dailyActivityUserVsSleep$MTotalSteps>=7500 & dailyActivityUserVsSleep$MTotalSteps<10000,2,
                     ifelse(dailyActivityUserVsSleep$MTotalSteps>=10000 & dailyActivityUserVsSleep$MTotalSteps<12500,3,
              4))))
head(dailyActivityUserVsSleep)

#Graphically
dailyActivityUserVsSleep %>%
group_by(UserType) %>%
summarise(TMTotalMinutesAsleep = mean(MTotalMinutesAsleep)) %>%
ggplot(aes(UserType,y=TMTotalMinutesAsleep, fill=UserType)) +
    geom_col()+
    theme(legend.position="none") +
    labs(title="User Types Vs Mean Time Asleep", x="User Type") +
    theme(legend.position="none", text = element_text(size = 15),plot.title = element_text(hjust = 0.5))
```

It seems that Sedentary users have about 50 min more asleep than the rest of the groups however, according to this, in average no User Type have the recommended amount of sleep (8hours as per 8h x 60min = 480min), which can be caused for the lack of sleep registers, as we saw with previous data, users doesn't seem to wear the devices to sleep.

Now, we can check if we can see a relationship between the amount of time people expend active and the amount of time people sleep. It's important to discern that even if we can see a correlation (more activity time - more sleep time) we won't be looking at causation (more activity causes people to sleep more; or more sleep time causes people to have more active time).
```{r}
ggplot(dailyActivityAndSleep, aes(x=TotalMinutesAsleep, y=TotalSteps))+
  geom_jitter() +
  geom_smooth(color = "blue") + 
  labs(title = "Daily steps vs Minutes asleep", x = "Minutes Asleep", y= "Daily Steps") +
   theme(panel.background = element_blank(),
        plot.title = element_text( size=20))
```

As we can see by the previous graphic, there seems to be a slightly negative correlation between the amount of Daily Steps taken and Minutes Asleep, so it seems that less activity (measured as Daily Steps) equals less amount of sleep, however, data is very scattered and we lost a bit more than half of the data (530 out of 940 observations) due to the lack of Sleep registers per user, so this is inconclusive. 

<a id="asleep-vs-bed"></a>
### Time Asleep vs Time in Bed
To close this data exploration, we can explore if these data have a trend that suggest poor sleep quality and/or problems to sleep by looking at total sleeping hours and time in bed vs sleep hours.
```{r}
ggplot(dailySleep, aes(x=TotalMinutesAsleep, y=TotalTimeInBed))+
  geom_jitter() +
  geom_smooth(color = "blue") + 
  labs(title = "Time Asleep vs Time in Bed", x = "Minutes Asleep", y= "Time in Bed") +
   theme(panel.background = element_blank(),
        plot.title = element_text( size=20))
```

Here we can see a clearly linear positive correlation between these two variables, as expected. We can also note a slightly higher inclination towards Time in Bed, and according to the Fitabase dictionary this time is collected as the Total minutes spent in bed, including asleep, restless, and awake, that occurred during a defined sleep record, so people may expend some time in bed without sleeping and or have a bad quality of sleep. 

<a id="recommendations"></a>
## Business Recommendations

Bellabeat products are focussed on collecting data on and inform users about their activity, sleep, stress, and reproductive health, which have allowed the company to empower women with knowledge about their own health and habits. 

Bellabeat has a very strong active marketing strategy in several platforms, however they want to prepare for the future, and the company's Co-funders and CCOs also want to know about other devices usage and identifiable trends that can help them to improve Bellabeat products to become a larger player in the global smart device market.

1. For starters, the first and biggest suggestion would be to explore this information further obtaining more data with better parameters and a bigger sample from current Bellabeat users, focussed on women, and including information on menstrual cycle data and some targeted questions to obtain future more adapted suggestions.
2. Promote some motivation per user type helping users to stablish their initial user type and a target user type with a minimum target on "Somewhat active" for people to reach a minimum healthy amount of activity. Users could receive congratulatory notifications when goals are reached.
3. Stablish a special notification on Sundays to motivate all users to move and have a walk or some exercise.
4. Allow users to select their preferred time of activity and receive a reminder to exercise 0,5-1h after this set time if no activity has been registered before that, and again at 18:00, in the middle of the second activity peak identified according to this data.
5. Stablish reminders to active pause for 5min when sedentary time surpass a 1h threshold.
6. Make recommendations regarding sleep time when a low sleep quality is detected.
7. Make all goals sharable, specially as a game and competition to themselfs and other users, as the best marketing is the one made person to person, the more a person share about their great experiences with a device and the goals their are reaching because of it, the company will caught the attention of more possible consumers.


### Bellabeat *Empowering Women to Unlock Their Full Potential*

