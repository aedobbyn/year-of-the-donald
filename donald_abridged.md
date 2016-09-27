# Primary Analysis, Abridged
Amanda Dobbyn  
`r format(Sys.Date())`  



***

## Overview
* About:
    + This is a shortened version of `year_of_the_donald.Rmd` and `year_of_the_donald.R` that omits most of the code to focus on the models and graphs
    + Data from [Kaggle](https://www.kaggle.com/datasets) can be found in `county_facts_abr.csv` and `primary_results.csv`
* Load data:
    + With `readr::read_csv()` rather than from a local Postgres database (as in `year_of_the_donald.Rmd`) to switch things up 
* Join the two datasets by county code
* Analyze:
    + Group counties into states and from there get vote totals/averages for candidates of interest
    + Pretend that this is a head to head in the general: look at total number of votes cast for Clinton and Trump
    + Using an "all-or-nothing" scheme, calculate the "winner" of each county and each state
* Model
    + Use demographic variables to train a random forest algorithm that predicts which general election candidate will "win" each county. Calculate the importance of each of these variables to the predictive power of the model
    + Train a K-nearest neighbors algorithm to do the same
* Plot:
    + Plot Clinton's "lead" by state
    + Plot how various demographic variables affect vote outcomes for the candidates

***

1. Load in and prepare the two datasets


2. Join the datasets


3. Examine the first few rows of our main dataframe before doing any analysis on it

```r
kable(head(election), format="markdown")
```



| fips_county_code|state   |state_abbr |party    |candidate       | votes| fraction_votes|state_abbreviation | population_2014| female| white| black| hispanic| college| inc_percap| inc_household|
|----------------:|:-------|:----------|:--------|:---------------|-----:|--------------:|:------------------|---------------:|------:|-----:|-----:|--------:|-------:|----------:|-------------:|
|             1001|Alabama |AL         |Democrat |Bernie Sanders  |   544|          0.182|AL                 |           55395|   51.4|  77.9|  18.7|      2.7|    20.9|      24571|         53682|
|             1001|Alabama |AL         |Democrat |Hillary Clinton |  2387|          0.800|AL                 |           55395|   51.4|  77.9|  18.7|      2.7|    20.9|      24571|         53682|
|             1003|Alabama |AL         |Democrat |Bernie Sanders  |  2694|          0.329|AL                 |          200111|   51.2|  87.1|   9.6|      4.6|    27.7|      26766|         50221|
|             1003|Alabama |AL         |Democrat |Hillary Clinton |  5290|          0.647|AL                 |          200111|   51.2|  87.1|   9.6|      4.6|    27.7|      26766|         50221|
|             1005|Alabama |AL         |Democrat |Bernie Sanders  |   222|          0.078|AL                 |           26887|   46.6|  50.2|  47.6|      4.5|    13.4|      16829|         32911|
|             1005|Alabama |AL         |Democrat |Hillary Clinton |  2567|          0.906|AL                 |           26887|   46.6|  50.2|  47.6|      4.5|    13.4|      16829|         32911|

***

For exploratory analysis, see `year_of_the_donald.Rmd`

***
## Table of total votes in the primaries per candidate





Summarise various metrics by state

```r
election.by.state <- election %>%
  group_by(state_abbreviation, candidate) %>%
  summarise(                 
    w.b_gap = mean(white - black),           # w.b_gap for white-black gap
    fr.votes = mean(fraction_votes),
    tot.votes = sum(votes),
    percap = mean(inc_percap)
  ) %>%
  ungroup %>%
  arrange(
    percap
  )
```

Spread this table to wider format, making each candidate a variable whose value is the total number of votes he or she received

```r
election.by.state.spread <- election.by.state %>% 
  select (
    state_abbreviation, candidate, tot.votes
  ) %>%
  rename(
    State = state_abbreviation
  ) %>% 
  group_by(candidate) %>%
  spread (                  
    key = candidate,
    value = tot.votes
  ) 
```


```r
kable(election.by.state.spread, format = "markdown", 
      caption = "Total votes per candidate per state")
```



|State | Bernie Sanders| Donald Trump| Hillary Clinton|
|:-----|--------------:|------------:|---------------:|
|AL    |          76399|       371735|          309928|
|AR    |          64868|       133144|          144580|
|AZ    |         163400|       249916|          235697|
|CA    |        1502043|      1174829|         1940580|
|CO    |          71928|           NA|           49256|
|DE    |          36659|        42472|           55950|
|FL    |         566603|      1077221|         1097400|
|GA    |         214332|       501707|          543008|
|HI    |          23531|         5677|           10127|
|IA    |          69452|        45419|           69733|
|ID    |          18640|        62478|            5065|
|IL    |         452006|       427086|          396004|
|IN    |         335256|       590460|          303382|
|KY    |         210626|        82493|          212550|
|LA    |          72240|       124818|          221615|
|MD    |         281275|       236623|          533247|
|MI    |         595222|       483751|          576795|
|MO    |         309071|       382093|          310602|
|MS    |          36348|       191755|          182447|
|MT    |          63168|       114056|           55194|
|NC    |         460316|       458151|          616383|
|NE    |          19120|       121287|           14340|
|NJ    |         323259|       356697|          554237|
|NM    |         103856|        73530|          110451|
|NV    |           5641|        34531|            6296|
|NY    |         763469|       524932|         1054083|
|OH    |         513549|       727585|          679266|
|OK    |         174054|       130141|          139338|
|OR    |         320746|       240804|          251739|
|PA    |         719955|       892702|          918689|
|SC    |          95977|       239851|          271514|
|SD    |          25958|        44866|           27046|
|TN    |         120333|       332702|          245304|
|TX    |         475561|       757618|          935080|
|UT    |          61333|        24864|           15666|
|VA    |         275507|       355960|          503358|
|WA    |          19159|       403003|            7140|
|WI    |         567936|       386370|          432767|
|WV    |         123860|       156245|           86354|
|WY    |            156|           NA|             124|

***

<br><br>

## Models


#### Regression models


First a linear regression with only fixed effects.  
Percent female, percent college, and per capita income
predicting whether the winner of that county is Trump or Clinton.  

<br>

Clinton coded as 1, Trump as 0.

```r
simp_reg <- glm(winner ~ female + college + inc_percap,
                data=winner.winner, family=binomial())
kable(tidy(simp_reg), format="markdown")
```



|term        |   estimate| std.error|  statistic|  p.value|
|:-----------|----------:|---------:|----------:|--------:|
|(Intercept) |  4.9841552| 1.1697130|   4.261007| 2.04e-05|
|female      | -0.1060329| 0.0232929|  -4.552149| 5.30e-06|
|college     | -0.0901755| 0.0084775| -10.637067| 0.00e+00|
|inc_percap  |  0.0001298| 0.0000138|   9.375148| 0.00e+00|
This model suggests that women and college-educated people prefer Clinton, whereas Trump does better in wealthier counties.

<br>

Now a mixed model with percent female, percent college, and percent black
predicting winner.
Random intercept for state.

```r
mixed.mod <- winner.winner %>% 
  do(tidy(glmer(winner ~ female + college + black +
                  (1 | state_abbreviation),
                data=., family=binomial())))
kable(mixed.mod, format="markdown")
```



|term                              |   estimate| std.error|   statistic|   p.value|group              |
|:---------------------------------|----------:|---------:|-----------:|---------:|:------------------|
|(Intercept)                       |  5.5511598| 1.4328801|   3.8741272| 0.0001070|fixed              |
|female                            | -0.0204312| 0.0282795|  -0.7224733| 0.4700036|fixed              |
|college                           | -0.0733402| 0.0074884|  -9.7939035| 0.0000000|fixed              |
|black                             | -0.1550231| 0.0084682| -18.3065474| 0.0000000|fixed              |
|sd_(Intercept).state_abbreviation |  1.9742313|        NA|          NA|        NA|state_abbreviation |
Percent black is an important factor with counties with higher black populations more likely to support Clinton.

<br><br>

#### Random Forest


How important are each of the demographic variables?

```r
round(importance(win.rf), 2)
```

```
##                 Clinton Trump MeanDecreaseAccuracy MeanDecreaseGini
## population_2014   25.21 18.92                30.77           185.72
## female             0.97 16.60                14.97           128.53
## black             86.49 83.43               115.44           311.16
## hispanic          32.17 15.78                31.35           157.65
## college           26.35 27.46                41.75           161.50
## inc_percap        -6.20 45.93                41.98           170.10
```
Percent black seems to be particularly important  


<br><br>


#### K Nearest Neighbors


See how well the model did

```r
library(gmodels)
CrossTable(nov.test.labels, nov.pred, prop.chisq = F)
```

```
## 
##  
##    Cell Contents
## |-------------------------|
## |                       N |
## |           N / Row Total |
## |           N / Col Total |
## |         N / Table Total |
## |-------------------------|
## 
##  
## Total Observations in Table:  1798 
## 
##  
##                 | nov.pred 
## nov.test.labels |   Clinton |     Trump | Row Total | 
## ----------------|-----------|-----------|-----------|
##         Clinton |       262 |       264 |       526 | 
##                 |     0.498 |     0.502 |     0.293 | 
##                 |     0.642 |     0.190 |           | 
##                 |     0.146 |     0.147 |           | 
## ----------------|-----------|-----------|-----------|
##           Trump |       146 |      1126 |      1272 | 
##                 |     0.115 |     0.885 |     0.707 | 
##                 |     0.358 |     0.810 |           | 
##                 |     0.081 |     0.626 |           | 
## ----------------|-----------|-----------|-----------|
##    Column Total |       408 |      1390 |      1798 | 
##                 |     0.227 |     0.773 |           | 
## ----------------|-----------|-----------|-----------|
## 
## 
```
The algorithm classifies Trump better than Clinton, maybe because he won more overall counties(more data -> better prediction)


***

<br><br>


# Plots



Bar graphs for each state plotting whether Clinton won or lost in a fake head-to-head with Trump

```r
clinton.lead.plot <- ggplot(winner.winner) +
  geom_bar(aes(x=state_abbreviation, y=clinton.lead), stat='identity') +
  xlab("State") +
  ylab("Clinton's Lead")

clinton.lead.plot
```

![](donald_abridged_files/figure-html/hil_lead_plot-1.png)<!-- -->

<br>

### Integrate demographic variables
For each county, plot fraction of votes received against percent white for each candidate

```r
white.plot <- ggplot(election, aes(white, fraction_votes))
white.plot + geom_point(aes(colour = candidate, size=population_2014)) +
  labs(title = "Primary Votes of Whites") +
  xlab("Percent of county that is white") +
  ylab("Fraction of votes received") +
  labs(colour = "Candidate", size = "Population in 2014")
```

![](donald_abridged_files/figure-html/white_plot-1.png)<!-- -->
So at low percent white (left half of graph), Hillary does best and Bernie does worst at high percent white it's more of a jumble  

Size of point as fraction of vote

```r
college.plot <- ggplot(election, aes(college, votes))
college.plot + geom_point(aes(colour = candidate, size=fraction_votes)) +
  labs(title = "Primary Votes of College Educated") +
  xlab("Percent with college degrees") +
  ylab("Number of votes") +
  labs(colour = "Candidate", size = "Fraction of votes")
```

![](donald_abridged_files/figure-html/college_plot-1.png)<!-- -->

What's up with outlier county with very high population?  
Plot population per county against votes cast per county

```r
qplot(population_2014, votes, data = election)
```

![](donald_abridged_files/figure-html/qplot_pop-1.png)<!-- -->

Looks like there's also a county that cast a lot of votes  
Find county with highest population

```r
election[which.max(election$population_2014), 
         c("fips_county_code", "state", "population_2014")]
```

```
## # A tibble: 1 × 3
##   fips_county_code      state population_2014
##              <int>     <fctr>           <dbl>
## 1             6037 California        10116705
```
So the outlier is FIPS code 6037 (LA county).  
According to Google, they are the county with the highest population in the US (9,818,605).
Second is Cook County, IL at 5,194,675 (woot)  

<br>




Graph per capita income, the white-black gap, and total votes by state
for all three candidates

```r
by.state.plot <- ggplot(election.by.state, aes(percap, w.b_gap))
by.state.plot + geom_point(aes(colour = candidate, size=tot.votes)) +
  facet_grid(. ~ candidate) +
  xlab("Per capita income") +
  ylab("White-black gap per county") +
  labs(size="Total Votes", colour = "Candidate")
```

![](donald_abridged_files/figure-html/by_state_plot-1.png)<!-- -->





