---
title: "Student lifestyle dataset analysis"
author: "Vera A."
date: "2024-12-30"
output: 
  html_document:
    keep_md: true
bibliography: references.bib
---

### Project background

The data analyzed in this project contains different aspects of student lifestyle from 2000 students. This analysis aims to visualize patterns and understand any correlations between the students' daily habits and their academic performance.

The data set used was sourced from Kaggle [@sumit_kumar_2024].

### Setup




``` r
library(tidyverse)
library(ggplot2)
library(tidyr)
library(dplyr)
library(wesanderson)
library(knitr)

student_data <- read.csv("student_lifestyle_dataset.csv")
```

### Overview of the data set 

For this data set, the daily lifestyle attributes are hours allocated for

* studying 
* sleep 
* extracurricular activity 
* social activity
* physical activity

The students' academic performance and well being are represented by

* student GPA
* student stress level


``` r
head(student_data)
```

```
##   Student_ID Study_Hours_Per_Day Extracurricular_Hours_Per_Day
## 1          1                 6.9                           3.8
## 2          2                 5.3                           3.5
## 3          3                 5.1                           3.9
## 4          4                 6.5                           2.1
## 5          5                 8.1                           0.6
## 6          6                 6.0                           2.1
##   Sleep_Hours_Per_Day Social_Hours_Per_Day Physical_Activity_Hours_Per_Day  GPA
## 1                 8.7                  2.8                             1.8 2.99
## 2                 8.0                  4.2                             3.0 2.75
## 3                 9.2                  1.2                             4.6 2.67
## 4                 7.2                  1.7                             6.5 2.88
## 5                 6.5                  2.2                             6.6 3.51
## 6                 8.0                  0.3                             7.6 2.85
##   Stress_Level
## 1     Moderate
## 2          Low
## 3          Low
## 4     Moderate
## 5         High
## 6     Moderate
```

``` r
str(student_data)
```

```
## 'data.frame':	2000 obs. of  8 variables:
##  $ Student_ID                     : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ Study_Hours_Per_Day            : num  6.9 5.3 5.1 6.5 8.1 6 8 8.4 5.2 7.7 ...
##  $ Extracurricular_Hours_Per_Day  : num  3.8 3.5 3.9 2.1 0.6 2.1 0.7 1.8 3.6 0.7 ...
##  $ Sleep_Hours_Per_Day            : num  8.7 8 9.2 7.2 6.5 8 5.3 5.6 6.3 9.8 ...
##  $ Social_Hours_Per_Day           : num  2.8 4.2 1.2 1.7 2.2 0.3 5.7 3 4 4.5 ...
##  $ Physical_Activity_Hours_Per_Day: num  1.8 3 4.6 6.5 6.6 7.6 4.3 5.2 4.9 1.3 ...
##  $ GPA                            : num  2.99 2.75 2.67 2.88 3.51 2.85 3.08 3.2 2.82 2.76 ...
##  $ Stress_Level                   : chr  "Moderate" "Low" "Low" "Moderate" ...
```

### Descriptive statistics of student data variables


``` r
summary(student_data)
```

```
##    Student_ID     Study_Hours_Per_Day Extracurricular_Hours_Per_Day
##  Min.   :   1.0   Min.   : 5.000      Min.   :0.00                 
##  1st Qu.: 500.8   1st Qu.: 6.300      1st Qu.:1.00                 
##  Median :1000.5   Median : 7.400      Median :2.00                 
##  Mean   :1000.5   Mean   : 7.476      Mean   :1.99                 
##  3rd Qu.:1500.2   3rd Qu.: 8.700      3rd Qu.:3.00                 
##  Max.   :2000.0   Max.   :10.000      Max.   :4.00                 
##  Sleep_Hours_Per_Day Social_Hours_Per_Day Physical_Activity_Hours_Per_Day
##  Min.   : 5.000      Min.   :0.000        Min.   : 0.000                 
##  1st Qu.: 6.200      1st Qu.:1.200        1st Qu.: 2.400                 
##  Median : 7.500      Median :2.600        Median : 4.100                 
##  Mean   : 7.501      Mean   :2.705        Mean   : 4.328                 
##  3rd Qu.: 8.800      3rd Qu.:4.100        3rd Qu.: 6.100                 
##  Max.   :10.000      Max.   :6.000        Max.   :13.000                 
##       GPA        Stress_Level      
##  Min.   :2.240   Length:2000       
##  1st Qu.:2.900   Class :character  
##  Median :3.110   Mode  :character  
##  Mean   :3.116                     
##  3rd Qu.:3.330                     
##  Max.   :4.000
```

### Data set cleaning

No NA or missing values were observed on the data set.

The data table was also duplicated from wide to long format for ease of generating some of the succeeding plots.


``` r
student_data_long <- student_data %>% 
  
  select(Student_ID, 
         Studying = Study_Hours_Per_Day, 
         Extracurriculars = Extracurricular_Hours_Per_Day, 
         Sleep = Sleep_Hours_Per_Day,
         Socialization = Social_Hours_Per_Day,
         Physical_activity = Physical_Activity_Hours_Per_Day,
         GPA, 
         Stress_Level) %>% 
  
  pivot_longer(
    cols = -c(Student_ID, GPA, Stress_Level),
    names_to = "Activity",
    values_to = "Hours"
  )

head (student_data_long)
```

```
## # A tibble: 6 × 5
##   Student_ID   GPA Stress_Level Activity          Hours
##        <int> <dbl> <chr>        <chr>             <dbl>
## 1          1  2.99 Moderate     Studying            6.9
## 2          1  2.99 Moderate     Extracurriculars    3.8
## 3          1  2.99 Moderate     Sleep               8.7
## 4          1  2.99 Moderate     Socialization       2.8
## 5          1  2.99 Moderate     Physical_activity   1.8
## 6          2  2.75 Low          Studying            5.3
```

### Visualizations and EDA

#### Scatter plots 

Hours students spend per activity were plotted against GPA and stress levels on individual scatter plots to see if there are general patterns.

**Observations:**

* Most students with high GPAs allocate more hours for studying and less time for physical activity.  
* For extracurricular activities, social activities, and sleep hours, no general pattern was observed. 
* Most students with higher GPAs also have higher stress levels as observed across all the scatter plots, regardless of the activity hours measured against. 


``` r
ggplot(student_data, aes(x = Extracurricular_Hours_Per_Day, y = GPA, color = Stress_Level)) +
  geom_point() +
  scale_color_manual(breaks = c("High", "Moderate", "Low"),
                     values=c("#CC0000", "#FF9900", "#339999")) +
  labs(title = "Extracurricular activities vs GPA and Stress Level") + 
  xlim(0,10)
```

![](student_lifestyle_basicviz_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

``` r
ggplot(student_data, aes(x = Social_Hours_Per_Day, y = GPA, color = Stress_Level)) +
  geom_point() +
  scale_color_manual(breaks = c("High", "Moderate", "Low"),
                     values=c("#CC0000", "#FF9900", "#339999")) +
  labs(title = "Social activity vs GPA and Stress Level") +
  xlim(0,10)
```

![](student_lifestyle_basicviz_files/figure-html/unnamed-chunk-5-2.png)<!-- -->

``` r
ggplot(student_data, aes(x = Physical_Activity_Hours_Per_Day, y = GPA, color = Stress_Level)) +
  geom_point() +
  scale_color_manual(breaks = c("High", "Moderate", "Low"),
                     values=c("#CC0000", "#FF9900", "#339999")) +
  labs(title = "Physical activity vs GPA and Stress Level") +
  xlim(0,10)
```

![](student_lifestyle_basicviz_files/figure-html/unnamed-chunk-5-3.png)<!-- -->

``` r
ggplot(student_data, aes(x = Study_Hours_Per_Day, y = GPA, color = Stress_Level)) +
  geom_point() +
  scale_color_manual(breaks = c("High", "Moderate", "Low"),
                     values=c("#CC0000", "#FF9900", "#339999")) +
  labs(title = "Study hours vs GPA and Stress Level") +
  xlim(0,10)
```

![](student_lifestyle_basicviz_files/figure-html/unnamed-chunk-5-4.png)<!-- -->

``` r
ggplot(student_data, aes(x = Sleep_Hours_Per_Day, y = GPA, color = Stress_Level)) +
  geom_point() +
  scale_color_manual(breaks = c("High", "Moderate", "Low"),
                     values=c("#CC0000", "#FF9900", "#339999")) +
  labs(title = "Sleep hours vs GPA and Stress Level") +
  xlim(0,10)
```

![](student_lifestyle_basicviz_files/figure-html/unnamed-chunk-5-5.png)<!-- -->

#### Box plots

The total hours the students spent on their activities were plotted on box plots.

**Observations:**

* Majority of the students' time is allocated for studying and sleeping.
* Time for physical activity has the most variation across the students sampled.
* Hours spent for extracurricular and social activities are similar and quite lower than the other activities.


``` r
ggplot(student_data_long, aes(x = reorder(Activity, Hours), y = Hours, fill = Activity)) +
  geom_boxplot() +
  labs(title = "Distribution of Hours Spent on Student Activities",
       y = "Hours spent",
       x = "Daily activity") +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 5))
```

![](student_lifestyle_basicviz_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

The box plots were separated further versus stress levels and GPA as well as a faceted plot containing all given variables. 

**Observations**

* Hours spent for extracurricular and social activities have minimal variation across different student stress levels and GPAs.
* Students with higher GPAs (GPA = A) and higher stress (Stress level = High) spend the most time for studying and the least amount of time for physical activity.
* For the remaining samples (GPA = B, C), the amount of sleep is quite inversely correlated with stress level. 


``` r
# additional column for arranging stress levels in order
student_data_long$Stress_Levels = factor(student_data_long$Stress_Level, levels = c("Low", "Moderate", "High"))

ggplot(student_data_long, aes(x = reorder(Activity, Hours), y = Hours, fill = Activity)) +
  geom_boxplot() +
  labs(title = "Distribution of hours per stress level",
       y = "Hours spent",
       x = "") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  facet_wrap(~Stress_Levels) +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 5))
```

![](student_lifestyle_basicviz_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


``` r
# additional column for categorizing GPA/grades 
student_data_long <- student_data_long %>% 
  mutate(
    Grade = case_when(
      GPA >= 3.7 ~ "A",
      GPA >= 3.0 & GPA < 3.7 ~ "B",
      GPA >= 2.0 & GPA < 3.0 ~ "C",
      GPA >= 1.0 & GPA < 2.0 ~ "D",
      GPA < 1.0 ~ "F"
    )
  ) %>% 

  mutate(Grade = factor(Grade, levels = c("A", "B", "C", "D", "F")))


ggplot(student_data_long, aes(x = reorder(Activity, Hours), y = Hours, fill = Activity)) +
  geom_boxplot() +
  labs(title = "Distribution of hours per GPA level",
       y = "Hours spent",
       x = "") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  facet_wrap(~Grade) +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 5))
```

![](student_lifestyle_basicviz_files/figure-html/unnamed-chunk-8-1.png)<!-- -->



``` r
ggplot(student_data_long, aes(x = reorder(Activity, Hours), y = Hours, fill = Activity)) +
  geom_boxplot() +
  labs(title = "Faceted plot of GPA and stress levels",
       y = "Hours spent",
       x = "") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  facet_grid(Grade ~ Stress_Levels) +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 5))
```

![](student_lifestyle_basicviz_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

### Summary of findings

* The data set reinforces the usual saying "the more you study, the better grades you can get". 
* Extracurricular and social activities have minimal variation and thus may have minimal effect on student stress levels and GPA when kept within a range of hours per day.
* Physical activity and sleep are observed most likely to affect the students' performances. These variables can be further studied to understand the optimal amount students should allocate to each to improve their stress levels and GPAs.


### Recommendations

For future improvement of this project:

* Deep dive using statistical analysis
* Train models for predictive analytics
* Report formatting


### Notes

Hello! This is my first R project :) \
My initial objective was to deploy learnings from the Google Data Analytics certification course, specifically in R programming.
This is also my first experience coding in R and RStudio.

For this project, the focus is mainly on generating visualizations and basic visual analysis only.
Still got a lot to learn.

Cheers! \
Vera A.

### Data set reference
\

### End


