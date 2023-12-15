---
title: "The Impact of Crime Severity and Recidivism on Attitudes towards Felon Voting Rights"
subtitle: "602 (Fall 2023)"
author: Kenneth Bufford, Kai Holl, Esiquio Iglesias Guerra
format: 
  html:
    toc: true
    toc-depth: 2
    toc-title: Contents
    toc-location: left
    html-math-method: katex
    theme: flatly
    smooth-scroll: true
    link-external-icon: true
    link-external-newwindow: true
    citations-hover: true
    footnotes-hover: true
    font-size: 80%
---

<!--
ATTENTION: PLEASE SUBMIT both your qmd file and html file.
-->


<!--
Replace the placeholders (title and author) with yours in the yaml above.
-->


# Project Description
<!--
With the growing research into the punitive nature of the 13th Amendment, 
sociologists have begun to question the ways in which we mistreat felons 
and those incarcerated. One significant inquiry is who will advocate for 
those currently serving time. In 26 states, it is legal to disenfranchise 
felons while in jail, and prevent them from ever voting again in specific municipalities once freed. 
To understand how felon enfranchisement would impact our general population, 
we first examine the factors leading to continuous felon disenfranchisement.

In our survey, we investigate how the severity of the crime and the intention 
for recidivism affect people’s perceptions of whether voting rights should be 
restored to a felon in prison. We hypothesize that the more severe the crime, 
the less likely someone is to have their enfranchisement rights restored while 
serving time and that if someone intends to recidvise they will be less likely 
to have their enfranchisement restored in prison. We also hypothesis that felons 
of color will dispropotionatly not be enfranchised while in prison due to bias 
ideas of crime rate, recidivism and negative ideologies attributed to people 
of color. We hypothesis that perceptions of felons also playing a major role in 
indicated how likely someone is towards voting for enfranchisement legislation. 
Those who know someone in prison, or have better ideas of felons will be more 
likely to vote in favor of enfranchising felon as opposed to those with lower 
ideas of felons. Additionally, we explore how  a respondent’s general attitudes towards 
felons moderate this relationship. If people generally hold negative perceptions
of felons, we inquire about the impact of providing information about felons and 
their recidivism rates on the chances of enfranchisement.
-->


```{r}
#| label: setup
#| warning: false
#| message: false

# library(ggpubr)
# library(naniar)
library(tidyr)
library(haven)
library(tidyverse)
library(explore)
library(haven)
library(stringr)
library(ggpubr)
library(ltm)

knitr::opts_chunk$set(echo = TRUE, warning=FALSE, message=FALSE)
```

# Importing Dataset and Cleaning

```{r warning = FALSE, message = FALSE}
#survey_data <- read_csv('RD_Omnibus_FA23_final_dec7.csv')
survey_data <- read_csv('https://raw.githubusercontent.com/eiig26/public_data/main/RD_Omnibus_FA23_final_dec7.csv')

#Remove the headers
survey_data <- survey_data [-(1:2),]

#Select just our columns and Remove the preview responses
survey_data <- survey_data[survey_data$Status != "Survey Preview", c(
  "Duration (in seconds)",
  "DistributionChannel",
  "Consent",
  "g4q1_1", "g4q1_2", "g4q1_3", "g4q1_4", "g4q1_5", "g4q1_6", "g4q1_7", "g4q1_8", "g4q1_9",
  "g4q2", "g4q3", "g4q4", "g4q5", "g4q6", "g4q7", "g4q8", "g4q9", "g4q10", "g4q11", "g4q12", "g4q13", "g4q14",
  "age", "gender", "hhi", "ethnicity", "hispanic", "education", "political_party", "zip", "region",
  "g4_DO", "g4jail1", "g4jail2"
)]

#Remove Consent = I do not agree
survey_data <- survey_data %>% filter(Consent == "I agree")

#Rename columns for best understanding
survey_data <- survey_data %>% rename("(att)g4q1_1_honest living" = g4q1_1, "(att)g4q1_2_next door" = g4q1_2, "(att)g4q1_3_rehabilitate" = g4q1_3, "(att)g4q1_4_productivelives" = g4q1_4, "(att)g4q1_5_immoral" = g4q1_5, "(att)g4q1_6_circumstance" = g4q1_6, "(att)g4q1_7_trust" = g4q1_7, "(att)g4q1_8_guard" = g4q1_8, "(att)g4q1_9_parole" = g4q1_9, "(control)g4q2_felonsvote" = g4q2, "g4q3_white_lowc_noitd" = g4q3, "g4q4_nowhite_lowc_noitd" = g4q4, "g4q5_white_medc_noitd" = g4q5, "g4q6_nowhite_medc_noitd" = g4q6, "g4q7_white_highc_noitd" = g4q7, "g4q8_nowhite_highc_noitd" = g4q8, "g4q9_white_lowc_itd" = g4q9, "g4q10_nowhite_lowc_itd" = g4q10, "g4q11_white_medc_itd" = g4q11, "g4q12_nowhite_medc_itd" = g4q12, "g4q13_white_highc_itd" = g4q13, "g4q14_nowhite_highc_itd" = g4q14,  "g4jail1_youincar" = g4jail1, "g4jail2_famfrdincar" = g4jail2)

#Recode gender column
survey_data <- survey_data %>%
  mutate(gender = recode(as.character(gender),"1" =	"Male", "2" =	"Female"))

#Recode ethnicity column
survey_data <- survey_data %>%
  mutate(ethnicity = recode(as.character(ethnicity),
"1" =	"White", 
"2" =	"Black, or African American",
"3" =	"American Indian or Alaska Native",
"4" =	"Asian *** Asian Indian", 
"5"	= "Asian *** Chinese", 
"6" =	"Asian *** Filipino",
"7"	= "Asian *** Japanese", 
"8"	= "Asian *** Korean", 
"9" =	"Asian *** Vietnamese", 
"10" =	"Asian *** Other", 
"11" =	"Pacific Islander *** Native Hawaiian ",
"12" =	"Pacific Islander *** Guamanian", 
"13" =	"Pacific Islander *** Samoan", 
"14" =	"Pacific Islander *** Other Pacific Islander",
"15" =	"Some other race", 
"16" =	"Prefer not to answer")) 

#Recode political party columns 
survey_data <- survey_data %>%
  mutate(political_party = recode(as.character(political_party),
"1" =	"Strong Democrat",
"2" =	"Not very strong Democrat",
"3" =	"Independent Democrat",
"4" = "Independent - neither",
"5" =	"Independent Republican",
"6" =	"Other - leaning Democrat",
"7" =	"Other - neither",
"8"	= "Other - leaning Republican",
"9" =	"Not very strong Republican",
"10" = "Strong Republican"))

#Recode education columns
survey_data <- survey_data %>%
  mutate(education = recode(as.character(education),
"1" =	"Some high school or less",
"2" =	"High school graduate",
"3" =	"Other post high school vocational training",
"4" =	"Completed some college, but no degree",
"5" =	"Associate's degree",
"6" =	"Bachelor's degree",
"7" =	"Master's or professional degree",
"8" =	"Doctorate degree",
"-3105"	= "None of the above"))

#Recode hhi column (Annual household income)
survey_data <- survey_data %>%
  mutate(hhi = recode(as.character(hhi),
"1" =	"Less than $14,999",
"2" =	"$15,000 to $19,999",
"3" =	"$20,000 to $24,999",
"4" =	"$25,000 to $29,999",
"5" =	"$30,000 to $34,999",
"6" =	"$35,000 to $39,999",
"7" =	"$40,000 to $44,999",
"8" =	"$45,000 to $49,999",
"9" =	"$50,000 to $54,999",
"10" =	"$55,000 to $59,999",
"11" =	"$60,000 to $64,999",
"12" = "$65,000 to $69,999",
"13" =	"$70,000 to $74,999",
"14" =	"$75,000 to $79,999",
"15" =	"$80,000 to $84,999",
"16" =	"$85,000 to $89,999",
"17" =	"$90,000 to $94,999",
"18" =	"$95,000 to $99,999",
"19" =	"$100,000 to $124,999",
"20" =	"$125,000 to $149,999",
"21" =	"$150,000 to $174,999",
"22" =	"$175,000 to $199,999",
"23" =	"$200,000 to $249,999",
"24" =	"$250,000 and above",
"-3105" =	"Prefer not to answer"))

#Recode hispanic columns
survey_data <- survey_data %>%
  mutate(hispanic = recode(as.character(hispanic),
"1" =	"No , not of Hispanic, Latino, or Spanish origin",
"2" =	"Yes, Mexican, Mexican American, Chicano",
"3" =	"Yes, Cuban",
"4"	= "Yes, another Hispanic, Latino, or Spanish origin *** Argentina",
"5" =	"Yes, another Hispanic, Latino, or Spanish origin *** Colombia",
"6" =	"Yes, another Hispanic, Latino, or Spanish origin *** Ecuador",
"7" =	"Yes, another Hispanic, Latino, or Spanish origin *** El Salvadore", 
"8" =	"Yes, another Hispanic, Latino, or Spanish origin *** Guatemala", 
"9" =	"Yes, another Hispanic, Latino, or Spanish origin *** Nicaragua", 
"10" =	"Yes, another Hispanic, Latino, or Spanish origin *** Panama", 
"11" =	"Yes, another Hispanic, Latino, or Spanish origin *** Peru", 
"12" =	"Yes, another Hispanic, Latino, or Spanish origin *** Spain", 
"13" =	"Yes, another Hispanic, Latino, or Spanish origin *** Venezuela", 
"14" =	"Yes, another Hispanic, Latino, or Spanish origin *** Other Country",
"15" =	"Prefer not to answer"))

#Recode region columns
survey_data <- survey_data %>%
  mutate(region = recode(as.character(region),
"1" =	"Northeast",
"2" =	"Midwest",
"3" =	"South",
"4" =	"West"))

#Assign IV number

survey_data <- survey_data %>% 
  mutate(g4_IV = case_when(
   str_detect(g4_DO, "g4q3") ~ 1,
    str_detect(g4_DO, "g4q4") ~ 2,
    str_detect(g4_DO, "g4q5") ~ 3,
    str_detect(g4_DO, "g4q6") ~ 4,
     str_detect(g4_DO, "g4q7") ~ 5,
     str_detect(g4_DO, "g4q8") ~ 6,
     str_detect(g4_DO, "g4q9") ~ 7,
     str_detect(g4_DO, "g4q10") ~ 8,
     str_detect(g4_DO, "g4q11") ~ 9,
     str_detect(g4_DO, "g4q12") ~ 10,
     str_detect(g4_DO, "g4q13") ~ 11,
     str_detect(g4_DO, "g4q14") ~ 12,
    )) 

write.csv(survey_data, "survey_data_v2.csv", row.names=TRUE)

#The survey_data file was uploaded to GitHub for continue with the Data Cleaning process
```

# More Data Cleaning and Recoding

```{r}
survey.data <- read_csv("https://raw.githubusercontent.com/kholl28/dacss602final/main/survey_data_v2.csv?token=GHSAT0AAAAAACLHNEN2NCZ7WEDFG2CIKPZSZL2FBMQ")

# renaming columns to make referencing them easier
# --------------------------------
oldnames = c("...1","(att)g4q1_1_honest living","(att)g4q1_2_next door","(att)g4q1_3_rehabilitate","(att)g4q1_4_productivelives","(att)g4q1_5_immoral","(att)g4q1_6_circumstance","(att)g4q1_7_trust","(att)g4q1_8_guard","(att)g4q1_9_parole","(control)g4q2_felonsvote","g4q3_white_lowc_noitd","g4q4_nowhite_lowc_noitd","g4q5_white_medc_noitd","g4q6_nowhite_medc_noitd","g4q7_white_highc_noitd","g4q8_nowhite_highc_noitd","g4q9_white_lowc_itd","g4q10_nowhite_lowc_itd","g4q11_white_medc_itd","g4q12_nowhite_medc_itd","g4q13_white_highc_itd","g4q14_nowhite_highc_itd","g4jail1_youincar","g4jail2_famfrdincar")

newnames = c("respondent","honest.living","next.door","rehab","productive","immoral","circumstance","trust","guard","parole","vote.info","white.low.nor","non.low.nor","white.med.nor","non.med.nor","white.high.nor","non.high.nor","white.low.r","non.low.r","white.med.r","non.med.r","white.high.r","non.high.r","you.jail","famfrd.jail")

# Info on Column Names
# --------------------------------
# ATTITUDES SCALE - full Questions
# honest.living = Most felons are too lazy to earn an honest living. **(REVERSE SCORED)**
# next.door = I wouldn’t mind living next door to an ex-felon.
# rehab = Trying to rehabilitate felons is a waste of time and money. **(REVERSE SCORED)**
# productive = Given the right conditions, felons can lead productive lives and become law-abiding citizens.
# immoral = Felons are just plain immoral. **(REVERSE SCORED)**
# circumstance = Felons are victims of circumstance and deserve to be helped.
# trust = It is wise to trust a felon.
# guard = You have to be constantly on your guard with felons. **(REVERSE SCORED)**
# parole = If a person does well in prison, he/she should be let out on parole.

# VIGNETTES
# white = white felon
# non = non-white felon
# low = low crime severity, tax fraud
# med = medium crime severity, armed robbery
# high = high crime severity, sexual offense against a child
# nor = no intention to recidivate, ie. do not intend to recommit crimes once out of prison
# r = intention to recidiviate, ie. do intend to recommit crimes once out of prison


# Recoding Attitude Responses to Numeric
# --------------------------------
survey.data <- survey.data %>%
  dplyr::select(-c(DistributionChannel, Consent)) %>% # removing some unnecessary columns
  rename_at(vars(oldnames), ~ newnames) %>%  
  mutate(across(c("next.door","productive","circumstance","trust","parole","vote.info"), # normal worded positive scale items plus vote.info variable
            ~case_when(
              grepl("Strongly disagree", .) ~ 1,
              grepl("Disagree", .) ~ 2,
              grepl("Agree", .) ~ 3,
              grepl("Strongly agree", .) ~ 4))) %>%
    mutate(across(c("honest.living","rehab","immoral","guard"), # recoding reverse worded survey items
            ~case_when(
              grepl("Strongly disagree", .) ~ 4,
              grepl("Disagree", .) ~ 3,
              grepl("Agree", .) ~ 2,
              grepl("Strongly agree", .) ~ 1)))

# Tabulating a Total Attitudes Score for Each Respondent
# --------------------------------
survey.data <- survey.data %>%
  mutate(attitudes = rowSums(.[3:11]) - 9, .after = "parole")

# 0 = most negative attitudes towards felons possible
# 27 = most positive attitudes towards felons possible

# Creating Summary df
# --------------------------------
vignettes <- survey.data %>%
  dplyr::select(14:25) 

respondent.df <- survey.data %>%
  mutate(v.condition = names(vignettes)[max.col(!is.na(vignettes))], .after = "non.high.r") %>% 
  mutate(felon.race = case_when(
    str_detect(v.condition, "white") ~ "white",
    str_detect(v.condition, "non") ~ "non-white"), .after = "v.condition") %>% 
  mutate(crime.sev = case_when(
    str_detect(v.condition, "low") ~ "low",
    str_detect(v.condition, "med") ~ "medium",
    str_detect(v.condition, "high") ~ "high"), .after = "felon.race") %>%
   mutate(recidivism = case_when(
    str_detect(v.condition, "nor") ~ "no intent",
    str_detect(v.condition, "r") ~ "intent"), .after = "crime.sev") %>% 
  mutate(v.response = coalesce(white.low.nor,non.low.nor,white.med.nor,non.med.nor,white.high.nor,non.high.nor, # v.response = survey participant's response to vignette
                               white.low.r,non.low.r,white.med.r,non.med.r,white.high.r,non.high.r), .after = "recidivism") %>% 
  dplyr::select(-14:-25, -v.condition)

# Turning variables into factor variable
# --------------------------------
respondent.df$crime.sev <- factor(respondent.df$crime.sev, levels = c("low", "medium", "high"))
respondent.df$v.response <- factor(respondent.df$v.response, levels = c("No", "Yes"))

# Creating new column for respondent history of incarceration
# --------------------------------
# you.jail = "Have you ever been incarcerated (sentenced to prison)" Yes/No
# famfrd.jail = "Has a friend or family members of yours ever been incarcerated (sentenced to prison)?" Yes/No

respondent.df <- respondent.df %>% 
  mutate(jail = case_when(
    you.jail == "Yes" | famfrd.jail == "Yes" ~ "Yes",
    you.jail == "No" | famfrd.jail == "No" ~ "No"), .after = "famfrd.jail")
```

# Descriptive Summary
<!--
One descriptive table and/or plot: Provide an overview of all the variables used in the statistical test. For example, the mean, standard deviation, range (min/max), contingency table, etc.
-->

```{r}
# race
# crime severity
# recidivism intention
# vignette response (DV)

# Vignette Response + Race of Felon
# --------------------------------
table(respondent.df$felon.race, respondent.df$v.response)
# non-white: no = 86, yes = 48
# white: no = 86, yes = 43

# Vignette Response + Crime Severity
# --------------------------------
table(respondent.df$crime.sev, respondent.df$v.response)
# No:
# low = 58
# medium = 53
# high = 61

# Yes:
# low = 31
# medium = 35
# high = 25

# Vignette Response + Recidivism Intent
# --------------------------------
table(respondent.df$recidivism, respondent.df$v.response)
# intent:
# no = 96
# yes = 35

# no intent:
# no = 76
# yes = 56

# Overall Vignette Responses
# --------------------------------
table(respondent.df$v.response)
# no = 172
# yes = 91

# Summary Table of Attitudes per each IV
# --------------------------------
summary_table <- respondent.df %>%
  group_by(felon.race, crime.sev, recidivism) %>%
  summarize(
    Mean_Attitudes = mean(attitudes, na.rm = TRUE),
    Response_Yes = sum(v.response == "1", na.rm = TRUE),
    Response_No = sum(v.response == "0", na.rm = TRUE),
    .groups = 'drop'  # This drops the grouping after summarization
  )

summary_table

# most positive attitudes towards felons: 27
# most negative attitudes towards felons: 0
```

# Findings
<!--
Findings: Report the statistic you obtained along with the information to decide whether the null hypothesis can be rejected or not. Use a plot and/or table with a proper label.
-->

```{r}
# What variables do we want to compare to each other to see if there is a relationship?

# Attitudes vs. Vignette Response = point-biserial correlation test
# Race of felon vs. Vignette Response = chi-squared test
# Crime Severity vs. Vignette Response = chi-squared test
# Recidivism Intention vs. Vignette Response = chi-squared test

# Attitudes vs. Vignette Response
# --------------------------------
respondent.df <- respondent.df %>%
  mutate(v.response = case_when(
              v.response == "No" ~ "0",
              v.response == "Yes" ~ "1"))

biserial.cor(respondent.df$attitudes, respondent.df$v.response, use = c("all.obs"), level = 1)
biserial.cor(respondent.df$attitudes, respondent.df$v.response, use = c("all.obs"), level = 2)
# level 1 = no response to vignette
# -0.2203021
# level 2 = yes response to vignette
# 0.2203021
# normal relationship, not very strong

# https://medium.com/@RaharditoDP/point-biserial-correlation-in-r-70e713caa7a0


# Race of felon vs. Vignette Response
# --------------------------------
chisq.test(respondent.df$felon.race, respondent.df$v.response)
# p-value = 0.7685
# so, fail to reject the null hypothesis (p > 0.05)
# x-squared = 0.086612 
# df = 1


# Crime Severity vs. Vignette Response
# --------------------------------
chisq.test(respondent.df$crime.sev, respondent.df$v.response)
# p-value = 0.332
# so, fail to reject the null hypothesis (p > 0.05)
# x-squared = 2.205
# df = 2


# Recidivism Intention vs. Vignette Response
# --------------------------------
chisq.test(respondent.df$recidivism, respondent.df$v.response)
# p-value = 0.01084
# so, reject the null hypothesis (p < 0.05), yay!
# x-squared = 6.4907
# df = 1


# Incarceration History vs. Vignette Response
# --------------------------------
chisq.test(respondent.df$jail, respondent.df$v.response)
# p-value = 0.4388
# so, fail to reject the null hypothesis (p > 0.05)
# x-squared = 0.59935
# df = 1
```

```{r}
# https://www.r-bloggers.com/2021/08/how-to-plot-categorical-data-in-r-quick-guide/#:~:text=A%20useful%20technique%20to%20show,is%20to%20use%20grouped%20boxplots.&text=The%20teams%20are%20represented%20on,represented%20on%20the%20y%2Daxis

# https://rkabacoff.github.io/datavis/Bivariate.html

# Attitudes vs. Vignette Response
# Race of felon vs. Vignette Response = chi-squared test
# Crime Severity vs. Vignette Response = chi-squared test
# Recidivism Intention vs. Vignette Response = chi-squared test


# Relationship between Attitudes and Vignette Response
# --------------------------------
respondent.df %>%
  ggplot(aes(v.response, attitudes)) +
  geom_boxplot(fill='lightskyblue') +
  theme_minimal() +
  labs(x = "Vignette Response", y="Total Attitudes Score", title = "Relationship between Attitudes and Vignette Response")

respondent.df %>%
  group_by(v.response) %>%
  count(attitudes) %>%
  ggplot(aes(attitudes, n, colour = v.response, group = v.response)) +
  geom_line(lwd = 1) +
  theme_minimal() +
  theme(legend.position = 'bottom') +
  scale_colour_hue(labels=c("Voting rights restored", "voting rights NOT restored")) +
  labs(x = "Total Attitudes Score", y = "Count", colour = "Vignette Response", title = "Relationship btw/ attitudes towards felons and decision to restore felon voting rights")


# Relationship between Race of Felon and Vignette Response 
# --------------------------------
respondent.df %>%
  ggplot(aes(felon.race, fill = factor(v.response, 
                                     levels = c("0", "1"),
                                     labels = c("no", "yes")))) +
  geom_bar(position = "fill", width = 0.7) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1", name = "Voting Rights Restored?", labels = c("No", "Yes")) +
  scale_y_continuous(labels = scales::percent_format(accurary = 1)) +
  labs(x = "Race of Felon", y="Proportion (%)", title = "Effect of Felon Race on Voting Rights Restoration")


# Relationship between Crime Severity and Vignette Respone 
# --------------------------------
respondent.df %>%
  ggplot(aes(v.response, fill = crime.sev)) +
  geom_bar(position = position_dodge(preserve = "single"), width = 0.7) +
  scale_fill_manual(name = "Crime severity", values = c("lightgreen", "khaki", "indianred")) +
  scale_x_discrete(name = "Voting Rights Restored?", labels = c("No", "Yes")) +
  theme_minimal() +
  labs(x = "Vignette Response", y="Count", title = "Effect of Crime Severity on Voting Rights Restoration")

# The majority of respondents said No to restoring voting rights to the felon.
# Respondents were marginally more likely to restore voting rights to a felon with a medium crime severity than a low or high crime severity **NEED TO CHECK SIGNIFICANCE**


# Relationship between Recidivism Intention and Vignette Response
# --------------------------------
respondent.df %>%
  ggplot(aes(v.response, fill=recidivism)) +
  geom_bar(position="fill", width = 0.7) +
  theme_minimal() +
   scale_x_discrete(name = "Voting Rights Restored?", labels = c("No", "Yes")) +
  scale_y_continuous(labels = scales::percent_format(accurary = 1)) +
  labs(x = "Vignette Response", y="Proportion (%)", title = "Effect of Felon Recidivism Intention on Voting Rights Restoration", fill = "Recidivism Intention")

# plot shows that when shown a vignette of a felon who intends to recommit the crime once released from prison, independent of the severity of the crime committed or the race of the felon, respondents are more likely to not restore rights to the felon compared to respondents who received vignettes of felons who did NOT intend to recidivise. 
```


# Interpretation
<!--

Discussion of Chi-Square Test:
We conducted Chi-Square tests on four distinct groups: Race of Felon vs. Vignette Response, 
Crime Severity vs. Vignette Response, Recidivism Intention vs. Vignette Response, 
and Incarceration History vs. Vignette Response. Among these, Race of Felon, Crime Severity, 
and Incarceration History yielded non-significant p-values. This indicates that we cannot reject 
the null hypothesis, suggesting no significant mean variance between the groups in these cases.
On the contrary, Recidivism Intention and Response displayed a P-Value of 0.01084, allowing us 
to reject the null hypothesis. This implies a discernible difference between the groups. 
Specifically, the intention of recidivism appears to influence the likelihood of voting in favor 
of enfranchisement. The significance of this finding underscores the impact that respondents' 
perceptions of an individual's likelihood to reoffend have on their stance toward enfranchisement.


Visualizations and Communication:
Examining bar graphs where results are segmented by variables and color-coded for interpretation 
reveals intriguing insights. The initial graph illustrates the connection between attitudes and 
vignette responses. Higher attitudes correlate with felons being more inclined to vote for enfranchisement. 
Divergent outcomes emerge across varying levels of crime severity. Notably, perceived non-violent 
crimes show little distinction from high-severity crimes. Conversely, crimes like robbery are 
perceived as more conducive to enfranchisement. Another finding indicates that race does not 
influence who is enfranchised. Non-white individuals are only marginally more likely to be 
re-enfranchised, with no significant difference. Turning our attention to recidivism intention, it 
plays a role in determining someone's likelihood of being re-enfranchised. Just over half of survey 
respondents, when informed about a defendant likely to recommit the crime post-release, voted against
enfranchising felons. This suggests that when negative information about felons is presented before 
voting, there's an almost 50-50 chance of a 'no' vote. Conversely, the intention not to recommit the 
crime resulted in 62% of respondents voting to enfranchise felons. When individuals have a genuine 
support system and are committed to rehabilitation, there's a higher likelihood of voting for enfranchisement, 
even while the individual is still in prison.

Social and Economic Considerations:
While we controlled for race and income, we believe another factor may explain the perception of 
armed robbery. Traditionally, crimes like robbery don't occur in a vacuum; they often stem from 
socioeconomic factors, where individuals, having limited options, resort to crime, unlike offenses 
such as tax evasion and pedophilia.

Suggestions for Future Research:
In future research, we aim to delve into specific points to gain a deeper understanding. First, 
we want to explore why armed robbery, considered a middle-severity crime, was perceived more 
favorably than tax evasion. Were our categorizations of non-violent crimes accurate, or do 
certain non-violent crime saliences predispose people to vote in a certain way? Now that we 
know race plays no significant role, further investigation into the three main groups of low, 
middle, and high severity crimes and how perception influences them is warranted.

Limitations of the Study:
Our study surveyed only 263 people, a sample size insufficient for extrapolating inferences about
the broader U.S. community. Furthermore, with 12 different groups, each consisting of fewer than
30 people, the sample distribution poses limitations. Additionally, the perception of crime severity
may vary; what we consider less severe might be perceived differently by others online.
-->

