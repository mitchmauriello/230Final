# 230Final
 
name: "Mauriello"
title: "My Website"
description: |
  My Website
output_dir: "docs"
navbar:
  right:
    - text: "Home"
      href: index.html
    - text: "About"
      href: about.html
    - text: "MLB"
      href: MLB.html
    - text: "Inforaphic"
      href: Infographic.html
output: distill::distill_website


---
title: "My Website"
description: |
  Welcome to the website. I hope you enjoy it!
site: distill::distill_website
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)

# Learn more about creating websites with Distill at:
# https://rstudio.github.io/distill/website.html

# Learn more about publishing to GitHub Pages at:
# https://rstudio.github.io/distill/publish_website.html#github-pages

```

Bio: 
  My name is Mitch Mauriello, I am a statistics major at Bucknell University currently finishing my fourth semester and taking MATH230. I have always had a love for baseball as it was my favorite sport to play growing up. However, as I got older I realized how interesting the finances within the MLB are. There's no salary cap in the MLB meaning that teams can spend whatever they want on whoever they want. This dynamic is unlike any other sport and intrigued me to evaluate how much of an advantage this gives to teams in a big market over those that don't have alot of money to spend. 
  
  
---
title: "Past Course Visualizations"
description: |
  Showcasing Previous Visualization
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```

```{r}
library(aRtsy)
library(distill)
library(Lock5Data)
library(Lock5Data)
library(hexbin)
library(Hmisc)
library(ggplot2)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library("ggplot2")
library("ggside")
library("ggTimeSeries")
library("devtools")
library("carData")
data("OBrienKaiser")
data("EuStockMarkets")
data(StudentSurvey)
```

Bucknell Phyllotaxis
```{r}
artwork1 = canvas_phyllotaxis(colors = c("blue", "orange"))
print(artwork1)
```

Ying & Yang
```{r}
artwork2 = canvas_stripes(colors = c("black", "white"))
print(artwork2)
```

American Pixels
```{r}
artwork3 = canvas_diamonds(colors = c("red", "white","blue"))
print(artwork3)
```

Title: First 730 Values for DAX
Data Set Description: 
  Our data set is compiled of European Stock market data where the indices included are Germany (DAX), UK (FTSE), Belgium (BEL20), Switzerland (SSMI), Denmark (OMX), France (CAC), Italy (MIB30), and the Netherlands (AEX). 
Visualization Description: 
  Our visualization shows a fraction of the DAX stock market data-specifically the first 730 values-and the market fluctuations on each given day. The color scale indicates red for extreme fluctuations and green for passive fluctuation. 


```{r}
StudentSurvey = StudentSurvey[complete.cases(StudentSurvey), ]
StudentSurvey$Year = factor(StudentSurvey$Year, 
                          levels = c("FirstYear", "Sophomore", "Junior", "Senior"),
                          ordered = TRUE)

# load the EuStockMarkets dataset
data(EuStockMarkets)

##GGPLOT EXTENSION
OBrienKaiser = OBrienKaiser[complete.cases(OBrienKaiser)]

minimalTheme = theme_set(theme_bw(12))
minimalTheme = theme_update(
  axis.ticks = element_blank(),
  legend.position = "none",
  strip.background = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank()
)

dfData1 = data.frame(
  DateCol = seq(
    as.Date("1/01/2014", "%d/%m/%Y"),
    as.Date("31/12/2015", "%d/%m/%Y"),
    "days"
  ),
  ValueCol = EuStockMarkets[1:730, 1]
)

ggplot_calendar_heatmap(dfData1, "DateCol", "ValueCol") + labs(title = "First 730 Values for DAX") +
  xlab(NULL) +
  ylab(NULL) +
  scale_fill_continuous(low = "green", high = "red") + facet_wrap(~Year, ncol = 1)


```

Title: GPA Among Award Preferences in Student Survey
Data Set Description: 
  Our data set, StudentSurvey, is compiled of a multitude of studentinformation. In this plot, we are using the variables GPA and Awards.
Visualization Description: 
  Our Visualization shows the GPA distributions among students’ award preferences. 
```{r}
attach(StudentSurvey)
ggplot(StudentSurvey, aes(x = GPA, y = Award, fill = Award)) + geom_boxplot()+
  guides(fill="none")
```


```{r}
ggplot(StudentSurvey, aes(x=GPA, color = Award ))+ 
  geom_density()
```

Title: GPA Distributions of Award Preferences
Description of Dataset: 
  Our data set, StudentSurvey, is compiled of a multitude of studentinformation. In this plot we are using the variables GPA and Awards.
Description of Visualization: 
  Our Visualization shows the distribution and density of GPA for each of the three award preferences, measuring each category’s density at each GPA interval
  
  
Confidence Interval Shiny Application:
Title: Intro Stats Confidence Interval Lesson
Data Set Description: 
  Our data set is simulated from normal distributions with mean mu and standard deviation sigma. We use bootstrapping to simulate many outputs from the desired distributions.
Visualization Description: 
  Our Visualizations show distributions of sample means in the simulated data, the interval width of the same data with different confidence levels (the higher the level, the higher the width), and the coverage rate of our confidence intervals over our simulated samples. 

<iframe src=“https://rconnect.bucknell.edu/content/369ef67a-9abf-45e0-9dc0-10745cc77a69” class=“l-screen-inset shaded” height=“1000px”></iframe>
---
title: "MLB Data"
description: |
  Here, we explore MLB Payroll and Win total data to evaluate the extent of the relationship that spending money allows you to accumulate more wins in the MLB.
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```


```{r}
#read.csv(file.choose("DATA"), header=TRUE)
mydata = read.csv("DATA.csv", header=TRUE)
attach(mydata)
library(ggplot2)
library(dplyr)
library(ggrepel)
```

Data set Description: 
  
  We will be referencing a parent dataset referred to as Standard MLB Data. The Standard MLB data contains all 30 MLB teams’ win totals, Payrolls, and playoff outcomes for every season from 2000 to 2021*. In this dataset, however, we omit 2020 and 2022 due to the shortened season and deference of team payroll in 2020 and due to the extended playoff format of both years. Wins are displayed as a whole number between 0 and 162, the number of games in a typical season (occasionally a 163 must be played), Payroll is displayed as a number between 0 and 300 and is expressed in millions of dollars (81.45 means 81,450,000 dollars), the year is represented as numbers 2000-2019 & 2021, Team is the name of the team (ex. Yankees, Mets, etc.), and Playoffs is a binary operator of values 0-5 indicating the playoff success as follows and printing the expressions in quotation marks: 0 = “Missed” (Missed Playoffs), 1 = “Lost WC” (lost the wildcard game), 2 = “Lost DS” (Lost League Divisional Series), 3 = “Lost CS” (Lost League Championship Series), 4 = “Lost WS” (Lost World Series), and 5 = “Won WS” (Won World Series). 
	Additionally, we will address two other notable adaptations of this parent data which are referred to as WS winners and Averaged MLB Data respectively. WS winners is simply just a subset of the data that contains only teams that have won at least one world series in the years we are observing; the variables all remain the same in this dataset. Averaged MLB data refers to only the average number of wins and average annual payroll for all 30 MLB teams and introduces two new variables avg_wins & avg_payroll. 
	
```{r}
# Create a new variable called PlayoffOutcome that recodes Playoff values
mydata <- mydata %>%
  mutate(PlayoffOutcome = recode(Playoffs, 
                                 `0` = "Missed", 
                                 `1` = "Lost WC", 
                                 `2` = "Lost DS", 
                                 `3` = "Lost CS", 
                                 `4` = "Lost WS", 
                                 `5` = "Won WS"))
```

```{r}
ggplot(mydata, aes(x = Payroll, y = Wins, shape = PlayoffOutcome)) + 
  geom_point() +
  scale_shape_manual(values=c(19,1,15,5,17,3)) +  # specify the shape values manually
  labs(x = "Payroll", y = "Wins", title = "Wins and Payroll Scatterplot Shaped Playoff success") +
  theme(legend.position = "right",  # position the legend on the right
        legend.title = element_blank(),  # remove the legend title
        legend.spacing.y = unit(0.2, "cm"),  # increase vertical spacing between legend items
        legend.text = element_text(size = 10),  # set legend text size
        legend.key.size = unit(0.5, "cm"))  # set legend key size
```

Here, we see a very raw visualization of the Payroll and Wins of all MLB Teams from every season in the years given in the dataset. There are 600 data points here, 30 teams for all 20 seasons, and we can see what appears to be a generally positive correlation between payroll spending and regular season wins. Additionally, as one can see in the legend, the points are shaped to reflect different postseason successes, a trend we seek to explore further by looking more closely at some subsets of this massive dataset.


Let's Filter This Plot A Bit!

<iframe src=“https://rconnect.bucknell.edu/content/17902463-0f42-4d9d-b0e7-94bcbd2e8670” class=“l-screen-inset shaded” height=“1000px”></iframe>


```{r}
#World Series Winners
WS_Winners = c("Yankees", "WhiteSox", "Royals", "RedSox", "Phillies", "Nationals", "Marlins", "Giants", "Diamondbacks", "Cubs", "Cardinals", "Braves", "Astros", "Angels")
WS_winners <- mydata %>% 
  filter(Team %in% c("Yankees", "WhiteSox", "Royals", "RedSox", "Phillies", "Nationals", "Marlins", "Giants", "Diamondbacks", "Cubs", "Cardinals", "Braves", "Astros", "Angels")) %>% 
  select(Team, Year, Payroll, Playoffs, Wins)
all_teams <- c("Yankees", "BlueJays", "RedSox", "Tampa", "Orioles", "WhiteSox", "Royals", "Twins","Tigers", "Guardians", "Astros", "Angels", "Athletics","Mariners", "Rangers", "Mets", "Phillies", "Nationals", "Marlins","Braves,", "Brewers", "Cubs", "Cardinals","Pirates", "Reds", "Giants", "Diamondbacks", "Dodgers", "Rockies", "Padres")
# Subset the data for the specified teams
mydata_sub <- mydata[mydata$Team %in% all_teams,]
# Create side-by-side boxplots using ggplot2
ggplot(mydata_sub, aes(x = Payroll, y = Team, fill = Team)) + 
  geom_boxplot() +
  scale_fill_discrete(name = "Team") +
  labs(x = "Payroll", y = "Team", title = "Money Spent in 2000's Among ALL MLB teams")

WS_Winners = c("Yankees", "WhiteSox", "Royals", "RedSox", "Phillies", "Nationals", "Marlins", "Giants", "Diamondbacks", "Cubs", "Cardinals", "Braves", "Astros", "Angels")
# Create a vector of team names
teams <- c("Yankees", "WhiteSox", "Royals", "RedSox", "Phillies", "Nationals", "Marlins", "Giants", "Diamondbacks", "Cubs", "Cardinals", "Braves", "Astros", "Angels")
# Subset the data for the specified teams
mydata_sub <- mydata[mydata$Team %in% teams,]
# Create side-by-side boxplots using ggplot2
ggplot(mydata_sub, aes(x = Payroll, y = Team, fill = Team)) + 
  geom_boxplot() +
  scale_fill_discrete(name = "Team") +
  labs(x = "Payroll", y = "Team", title = "Money Spent Among 2000's WS Winners")
```

It is here where we subset the data into Teams that have won the World Series (WS winners) to juxtapose the overall payroll distributions between them. Who are the big spenders? Who are the teams that won on a budget? These boxplots show total payroll over the 20 seasons in the dataset and show the caliber of spending it took all teams to eventually find themselves winning a world series. Looking back up to the first plot, which teams have spent the most without a world series win? We can evaluate every teams payroll distribution in relation to others.

```{r}
team_summary <- mydata %>%
  group_by(Team) %>%
  summarize(avg_payroll = mean(Payroll), avg_wins = mean(Wins))

subset_data <- subset(mydata, Playoffs == 5)
subset_data <- subset_data[, c("Year", "Team", "Payroll", "Wins","Team")]
team_summary <- mydata %>%
  group_by(Team) %>%
  summarize(avg_payroll = mean(Payroll), avg_wins = mean(Wins))

modelregSznAvg <- lm(avg_wins ~ avg_payroll, data = team_summary)
modelWSwinners = lm(Wins ~ Payroll, data = subset_data)
modelregszn <- lm(Wins ~ Payroll, data = mydata)

AVGCorr = cor(team_summary$avg_payroll,team_summary$avg_wins)
#print(AVGCorr)


WS_winners <- subset(mydata, PlayoffOutcome == "Won WS")
WSCorr <- cor(WS_winners$Payroll, WS_winners$Wins)
#print(WSCorr)

STDCorr = cor(Payroll, Wins)
```

  In the aforementioned data set description, we talked about evaluating the relationship between MLB Payroll and Wins for not only the standard MLB data, but a normalized average MLB data set, and the World series winner subset depicted above in the box plots. 

Let's take a look at these relationships: <iframe src=“https://rconnect.bucknell.edu/content/d3a26d78-92d5-406a-a2d1-2e5aa281ec53” class=“l-screen-inset shaded” height=“1000px”></iframe>


```{r}
# Define variables for legend labels
# Define variables for legend labels
WSwinners_label <- "World Series Winners"
regszn_label <- "Standard MLB Data"
regSznAvg_label <- "Averaged MLB Data"

ggplot() +
  geom_line(data = mydata, aes(x = Payroll, y = predict(modelregszn), color = "red"), size = 1.2) +
  geom_line(data = team_summary, aes(x = avg_payroll, y = predict(modelregSznAvg), color = "green"), size = 1.2) +
  geom_line(data = WS_winners, aes(x = Payroll, y = predict(modelWSwinners), color = "blue"), size = 1.2) +
  scale_color_manual(values = c("blue", "green", "red"), name = "Data", labels = c(paste0(WSwinners_label, ": "), paste0(regSznAvg_label, ": "), paste0(regszn_label, ": "))) +
  labs(x = "Payroll", y = "Wins") +
  ggtitle("Multi-Regression Line Comparison of Payroll vs. Wins in MLB") +
  theme_bw() +
  theme(legend.position = "right", legend.title = element_blank(), legend.text = element_text(size = 10), legend.box.spacing = unit(0.1, "cm"), legend.margin = margin(0, 5, 0, 5))
```
  Here, we condense the three regression lines condensed from the previous plot to see how different they truly are. We see that there is a shockingly similar slope coefficient between the World Series Winners and the Standard MLB data. Both are raw data and both display the subjective nature and intense variability that sports employ. However, we see that the normalized or Averaged MLB Data employs a much steeper linear relationship. It is in this plot, that we can visualize the differences and similarities detailed in the descriptions of the individual plots.
  
Conclusion About Spending and Wins in The MLB:

  There's a 30 million dollar difference between the average payroll of the Standard MLB Data and the average payroll of the World Series Winners (90 million compared to 120 million). Reiterating, the correlations between Payroll and Wins for the three categories of data used are as follows: Standard: ~.3, World Series Winners: ~.5, and Averaged: ~.7. Given this information, it seems that spending money on signing big free agents does equate to wins. Although the extent may not be as highly correlated through the standard data set, one cannot ignore the steep correlation value that the averaged data boasts. Additionally, the 30 million dollar difference in spending between all teams in all seasons and just those that won the World Series in those seasons, indicates that winning teams tend to spend more money than others. The relationship between Payroll and Wins is a positive one, and owners wishing for their teams to compete must come to terms with this regardless of the size of the market they're playing in; if you don't want to spend money in an effort to truly put your best foot forward in competing for a World Series, sell the team to someone who will.


  Lastly, we have the ultimate MLB data experience for any fan wishing to evaluate the trajectory of their team's success since the turn of the century. It is here where we can observe individual team trends over time observing win total and payroll in chronological order.
  
Let's take a look: <iframe src=“https://rconnect.bucknell.edu/content/e2c721f4-1b89-4f74-917b-84fc2404ab6b” class=“l-screen-inset shaded” height=“1000px”></iframe>

---
title: "Infographic"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


```{r}
library(ggplot2)
library(dplyr)
library(ggrepel)
```


```{r}
mydata = read.csv("DATA.csv", header=TRUE)
attach(mydata)
subset_data <- subset(mydata, Playoffs == 5)
subset_data <- subset_data[, c("Year", "Team", "Payroll", "Wins","Team")]
all_teams <- c("Yankees", "BlueJays", "RedSox", "Tampa", "Orioles", "WhiteSox", "Royals", "Twins","Tigers", "Guardians", "Astros", "Angels", "Athletics","Mariners", "Rangers", "Mets", "Phillies", "Nationals", "Marlins","Braves,", "Brewers", "Cubs", "Cardinals","Pirates", "Reds", "Giants", "Diamondbacks", "Dodgers", "Rockies", "Padres")
# Subset the data for the specified teams
team_summary <- mydata %>%
  group_by(Team) %>%
  summarize(avg_payroll = mean(Payroll), avg_wins = mean(Wins))
mydata_sub <- mydata[mydata$Team %in% all_teams,]
team_summary <- mydata %>%
  group_by(Team) %>%
  summarize(avg_payroll = mean(Payroll), avg_wins = mean(Wins))
#print(team_summary)
```







```{r, echo = FALSE}
###INFOGRAPGHIC
 
my_font <- "Arial"

mydata_sub <- mydata[mydata$Team %in% all_teams,]

# Create a vector of team names
teams <- c("Yankees", "WhiteSox", "Royals", "RedSox", "Phillies", "Nationals", "Marlins", "Giants", "Diamondbacks", "Cubs", "Cardinals", "Braves", "Astros", "Angels")

# Subset data for selected teams
team_payroll <- team_summary$Payroll[team_summary$Team %in% teams]

# Create histogram
ggplot(data.frame(Payroll = team_payroll), aes(x = Payroll)) +
  geom_histogram(fill = "green", color = "white", bins = 15) +
  ggtitle("Payroll Distribution of World Series Winners") +
  xlab("Payroll (in millions)") +
  ylab("Frequency")


ggplot(subset_data, aes(x = Payroll, y = Wins, label = Team)) +
  geom_point() +
  geom_text_repel() +
  xlab("Payroll") +
  ylab("Wins") +
  ggtitle("Scatterplot of Average Wins vs. Average Payroll for Each Team") +
  theme_gray() + geom_smooth(method = "lm", se = FALSE, color = "red") + # Add the linear regression line
  xlab("Payroll") +
  ylab("Wins") +
  ggtitle("Scatterplot of Wins vs. Payroll for World Series Winners")




