# Amanda Dobbyn
# summer 2016

# short exploration of data from 2016 primaries combod with demographic county data 
# both datasets from https://www.kaggle.com/datasets stored on local Postgres database

#### ===================================== for reference ======================================== ####
### describe primary_2016 data ####
# (from command line)

# \d+ primary_2016

# Table "public.primary_2016"
# Column      |       Type        |                         Modifiers                         | Storage  | Stats target | Description 
# ------------------+-------------------+-----------------------------------------------------------+----------+--------------+-------------
#   id               | integer           | not null default nextval('primary_2016_id_seq'::regclass) | plain    |              | 
#   state            | character varying |                                                           | extended |              | 
#   state_abbr       | character varying |                                                           | extended |              | 
#   county           | character varying |                                                           | extended |              | 
#   fips_county_code | real              |                                                           | plain    |              | 
#   party            | character varying |                                                           | extended |              | 
#   candidate        | character varying |                                                           | extended |              | 
#   votes            | integer           |                                                           | plain    |              | 
#   fraction_votes   | real              |                                                           | plain    |              | 
#   Indexes:
#   "primary_2016_pkey" PRIMARY KEY, btree (id)



### describe county_facts ###

# \d+ county_facts

# Table "public.county_facts"
# Column       |       Type        |                         Modifiers                         | Storage  | Stats target | Description 
# --------------------+-------------------+-----------------------------------------------------------+----------+--------------+-------------
#   id                 | integer           | not null default nextval('county_facts_id_seq'::regclass) | plain    |              | 
#   county_code        | integer           |                                                           | plain    |              | 
#   area_name          | character varying |                                                           | extended |              | 
#   state_abbreviation | character varying |                                                           | extended |              | 
#   population_2014    | integer           |                                                           | plain    |              | 
#   female             | real              |                                                           | plain    |              | 
#   white              | real              |                                                           | plain    |              | 
#   black              | real              |                                                           | plain    |              | 
#   hispanic           | real              |                                                           | plain    |              | 
#   college            | real              |                                                           | plain    |              | 
#   inc_percap         | real              |                                                           | plain    |              | 
#   inc_household      | real              |                                                           | plain    |              | 
#   Indexes:
# "county_facts_pkey" PRIMARY KEY, btree (id)


#### ========================================================================================== ####



# load in packages
library(RPostgreSQL)
library(tibble)
library(tidyr)
library(dplyr)   # remember to detach(package:MASS) so that select works (or use dplyr::select)


# ------------------------------------------------------------------------------------
# read in 2016 presidential primary data from SQL database

# set up driver as postgres
drv <- dbDriver("PostgreSQL")

# set connection to our db
con <- dbConnect(drv, dbname="pullplay_db", host='localhost', port=5432, user="amanda")

# select everything from table
primary_2016 <- dbGetQuery(con, "SELECT * FROM primary_2016
                           WHERE candidate in ('Bernie Sanders', 'Donald Trump', 'Hillary Clinton')") 

# stick variables we want to make into factors in a vector
to.factor <- c('state', 'state_abbr', 'county', 'party',
               'candidate')

# use apply to make those variables factors
primary_2016[, to.factor] <- data.frame(apply(primary_2016[, to.factor], 2, as.factor))

# make votes numeric so we can do math on them
primary_2016$votes <- as.numeric(primary_2016$votes)

# print structure and first few rows of the dataframe
str(primary_2016)
head(primary_2016)

# make primary df into a tibble
primary_2016 <- as_tibble(primary_2016)





# ------------------------------------------------------------------------------------
# read in county_facts data from database
county_facts <- dbGetQuery(con, "SELECT * FROM county_facts")

# prepare like primary data
to.fact <- c('area_name', 'state_abbreviation')
county_facts[, to.fact] <- data.frame(apply(county_facts[, to.fact], 2, as.factor))
county_facts <- as_tibble(county_facts)

# take a look
str(county_facts)
head(county_facts)


# take id out of county_facts so join() doesn't join on id
county <- county_facts %>%
  select (
    county_code,
    state_abbreviation, population_2014,               
    female, white, black, hispanic, college,
    inc_percap, inc_household
  )

# inner join with primary data on county code to get our main dataset
election <- primary_2016 %>%
  select(
    fips_county_code,           
    state, state_abbr, 
    party, candidate,
    votes, fraction_votes
  ) %>%
  inner_join(county, by = c("fips_county_code" = "county_code"))


# make election a tibble
election <- as_tibble(election)

# examine our main dataframe
head(election)
str(election)

# make population and votes numeric 
election$population_2014 <- as.numeric(election$population_2014)
election$votes <- as.numeric(election$votes)

# 51 levels of state_abbreviation (from county_facts data) 
# and 49 levels state_abbr (from primary_2016 data)

# see what the 2 level difference is
setdiff(levels(election$state_abbreviation), levels(election$state_abbr))
# so we don't have primary_2016 data from DC and MN




# ------ manipulate election tibble to get a sense of the data ------ #


# ------------------------------------------------------------------------------------
# calculate window function to calculate state-wide averages per candidate
# while keeping county-wide counts as well
e.window <- election %>%
  filter (
    candidate %in% c('Hillary Clinton', 'Donald Trump')
  ) %>%
  group_by(state_abbreviation, candidate) %>%
  mutate(                 
    n.counties = n(),
    w.b_gap = (white - black),        
    state.fr.votes = mean(fraction_votes),
    state.percap = mean(inc_percap)
  ) %>%
  select (candidate,
    state_abbreviation, votes, fraction_votes, w.b_gap,       # these are county-wide
    n.counties, state.fr.votes, state.percap                  # these are state-wide
  ) %>%
  print(n = 10)


# ------------------------------------------------------------------------------------
# get average votes per county per state (i.e., collapse across county)
# only look at Bernie and Hillary
dems <- election %>%
  na.omit() %>%
  group_by(state, candidate) %>%
  filter(candidate %in% c('Bernie Sanders', 'Hillary Clinton')) %>%
  select(candidate, state, votes, fraction_votes) %>%
  summarise(
    avg_votes = mean(votes)
  ) %>%
  print(n=15)

# unstack the candidates
dems.spread <- dems %>%
  spread (
    key = candidate,
    value = avg_votes
  )
dems.spread


# -----------------------------------------
# get avg and total  votes per candidate per state
# limit to general election candidates
general.by.state <- election %>%
  filter (
    candidate %in% c('Hillary Clinton', 'Donald Trump')
  ) %>%
  group_by(state_abbreviation, candidate) %>%
  select (
    state_abbreviation, votes,
    candidate, population_2014
  ) %>%
  summarise (
    tot.votes = sum(votes),
    mean.by.state = mean(votes),
    pop = sum(population_2014)
    # weight.votes = (mean.by.state*pop)
  ) %>%
  # ungroup %>%
  arrange(desc(
    pop), desc(tot.votes)
  ) 
general.by.state


# spread total votes per candidate per state
general.by.state.spread <- general.by.state %>%
  select(
    state_abbreviation, candidate, tot.votes
  ) %>%
  spread (                  
    key = candidate,
    value = tot.votes     # so the value under each candidate's name is the total number of votes they received
  ) 
general.by.state.spread

# rename columns to candidates' last names
names(general.by.state.spread)[names(general.by.state.spread)=='Donald Trump'] <- 'Trump'
names(general.by.state.spread)[names(general.by.state.spread)=='Hillary Clinton'] <- 'Clinton'

# # can also use
# general.by.state.spread <- general.by.state.spread %>%
#   rename(
#     Trump = `Donald Trump`,
#     Clinton = `Hillary Clinton`
#   )




# ------------------------------------------------------------------------------------
# pretending that this is a head-to-head (and not totally different primaries),
# see how much Clinton is winning by in total votes cast per state
clinton.lead <- general.by.state.spread %>%
  mutate(
    clinton.lead = Clinton - Trump
  ) 
clinton.lead


# winner take all
# give the loser a 0 in total votes
all.or.nothing <- clinton.lead %>%
  na.omit() %>%          # take out rows that contain NAs
  mutate (
    winner = ifelse(clinton.lead > 0, 'Clinton', 'Trump'),
    all.nothing.donald = ifelse(Trump > Clinton, Trump, 0),
    all.nothing.hillary = ifelse(Clinton > Trump, Clinton, 0)
  ) 
all.or.nothing

# how many states did each win?
length(which(all.or.nothing$winner=='Hil')) # 22
length(which(all.or.nothing$winner=='Donald')) # 16

# how many overall votes did Clinton win by?
all.or.nothing.sums <- all.or.nothing %>%
  ungroup %>%
  summarise (
    sum.d = sum(all.nothing.donald),
    sum.hil = sum(all.nothing.hillary),
    diff = sum.hil - sum.d
  )
all.or.nothing.sums







# ----------------------------------------------------------------------------- #
# ---------------------------------- the meat --------------------------------- #
# ----------------------------------------------------------------------------- #

# make our main tibble that has both our winner column and demographic data

# limit to general election candidates
combo <- election %>%
  dplyr::filter (
    candidate %in% c('Hillary Clinton', 'Donald Trump')
  ) %>%
  droplevels() %>%
  select (
    state_abbreviation, votes, fips_county_code,
    candidate, population_2014,
    female, white, black, hispanic, college, inc_percap
  ) 
combo

# check that our only levels are Hillary and Donald
levels(combo$candidate)

# spread out by candidate
combo.spread <- combo %>%
  spread (                  
    key = candidate,
    value = votes
  ) 
combo.spread

# rename columns
combo.spread <- combo.spread %>%
  rename(
    Trump = `Donald Trump`,
    Clinton = `Hillary Clinton`
  )

# check out Trump and Clinton columns
combo.spread[, c(1:2, 10:11)]


# add clinton.lead all or nothing and winner columns
winner.winner <- combo.spread %>%
  na.omit() %>%                         # take out all NAs
  mutate(
    clinton.lead = Clinton - Trump,
    winner = ifelse(clinton.lead > 0, 'Clinton', 'Trump'),
    all.nothing.donald = ifelse(Trump > Clinton, Trump, 0),
    all.nothing.hillary = ifelse(Clinton > Trump, Clinton, 0)
  ) 
winner.winner

# make the winner column a factor
winner.winner$winner <- factor(winner.winner$winner)


# see who got the chicken dinner for each state
head(winner.winner[, c('state_abbreviation', 'fips_county_code', 'winner')])



# -----------------------------------
# make a combo for each state
combo.by.state <- combo %>%
  group_by(state_abbreviation, candidate) %>%
  summarise (
    tot.votes = sum(votes),
    mean.votes = mean(votes),
    pop = sum(population_2014),
    mean.female = mean(female),
    mean.white = mean(white),
    mean.black = mean(black),
    mean.hisp = mean(hispanic),
    mean.inc = mean(inc_percap)
    #   mean.Trump = mean(Trump),      # consider making nice.Trump
    #   mean.Clinton = mean(Clinton)
  ) %>%
  arrange(desc(
    pop), desc(tot.votes)
  ) %>%
  print(n = 20)
combo.by.state

str(combo.by.state)

# spread by total votes (can also do by mean.votes)
combo.by.state.spread <- combo.by.state %>%
  select (
    state_abbreviation, candidate,
    tot.votes, # note that have to take out mean.votes or tot.votes so that don't get NAs in the Trump and Clinton columns
    pop, mean.female, mean.white, mean.black, mean.hisp, mean.inc
  ) %>%
  na.omit() %>%
  spread (                  
    key = candidate,
    value = tot.votes
  ) 
combo.by.state.spread

# check the end of the column
combo.by.state.spread[, c(1, 6:ncol(combo.by.state.spread))]


# rename columns
combo.by.state.spread <- combo.by.state.spread %>%
  rename(
    Trump = `Donald Trump`,
    Clinton = `Hillary Clinton`
  )


# add winner stuff
combo.by.state.spread <- combo.by.state.spread %>% 
  na.omit() %>%                         # take out all NAs
  mutate (
    `Clinton's lead` = Clinton - Trump,
    Winner = ifelse(`Clinton's lead` > 0, 'Clinton', 'Trump'),
    `All or nothing: Trump` = ifelse(Trump > Clinton, Trump, 0),
    `All or nothing: Clinton` = ifelse(Clinton > Trump, Clinton, 0)
  )  


# make the winner column a factor
combo.by.state.spread$Winner <- factor(combo.by.state.spread$Winner)

# move winner info to the left of the tibble
combo.by.state.spread <- 
  combo.by.state.spread[, c(1, 8:ncol(combo.by.state.spread), 2:7)]

# see who got the chicken dinner for each county
head(combo.by.state.spread[, c('state_abbreviation', 'Winner')])












# ----------------------------------- models --------------------------------- #

# a couple sample regressions
library(lme4)
library(broom)

# linear regression with percent female, percent college, and per capita income
# predicting whether the winner of that county is Trump or Clinton
simp_reg <- glm(winner ~ female + college + inc_percap,
                data=winner.winner, family=binomial())
tidy(simp_reg)

# mixed model with percent female, percent college, and percent black
# predicting winner
# random intercept for state
mixed.mod <- winner.winner %>% 
  do(tidy(glmer(winner ~ female + college + black +
                  (1 | state_abbreviation),
                data=., family=binomial())))
mixed.mod


# random forest --------------------
library(randomForest)
library(MASS)     # note that this masks dplyr::select

set.seed(23) # start random num generator at 23 (just for testing purposes)

# classify winner (Donald or Hillary) from county demographic variables
# DV = winner
# IVs = population, percent female, percent black, percent hispanic, percent college educated, per capita income
# 6 IVs so set mtry to sqrt(6) or 2
win.rf <- randomForest(winner ~ population_2014 + female + black +
                         hispanic + college + inc_percap,
                       data = winner.winner,
                       mtry = 2,    # number variables randomly sampled at each split
                       replace = TRUE,
                       proximitiy = TRUE, # get matrix of proximity measures
                       importance = TRUE) # we want to know how important each variable is
# do.trace = 100) # print output for every 100 trees

# print confusion matrix
print(win.rf)

# plot result
plot(win.rf)


# how important are each of the demographic variables?
round(importance(win.rf), 2)
# percent black seems to be particularly important


# detach MASS so we can get dplyr::select back
detach(package:MASS)





# K Nearest Neighbors ---------------------------

# adapted from https://www.datacamp.com/community/tutorials/machine-learning-in-r#gs.JIcctJU

# load in class package for knn()
library(class)

# function for normalizing predictor variables
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

# variables to keep
want.cols <- c('population_2014', 'female', 'white', 'black', 
               'hispanic', 'college', 'inc_percap',
               # 'fips_county_code',
               # 'Trump', 'Clinton', 'state_abbreviation', 
               'winner')

# make a new dataframe with only these cols
november <- winner.winner[, want.cols]
november <- as_tibble(november)

# make population numeric
november$population_2014 <- as.numeric(november$population_2014)

# take a look
head(november)

# norm our predictor variables (population_2014:inc_percap)
november.normed <- as.data.frame(lapply(november[ , 1:7], normalize))
head(november.normed)

# stick the winner names back on to our dataframe
november.normed <- cbind(november.normed, november[, 8])

# check that our predictors are numeric and winner is a factor
str(november.normed)

# check dimensions
dim(november.normed) # 2711 x 8


# assign rows to training or test at random (i.e., make it so not just first ~1000 are training and last ~2000 are test)
# make a vector the same length as our dataset with a random 1 or 2 assigned to each position based on the split we want
# 1 = training, 2 = test. Train with 1/3 of data, test with 2/3
rand.num <- sample(c(1, 2), 
                   nrow(november.normed), replace=T, 
                   prob=c((1/3), (2/3)))

# split dataset into training and test, and take out the target varialbe
# take the row numbers of all the 1s in the rand.num vector (vector that is separate and apart from our dataframe)
# use all columns except the target
nov.train <- november.normed[rand.num==1, 1:7]
nov.test <- november.normed[rand.num==2, 1:7]

# store the target variable (winner) in a vector
nov.train.labels <- november.normed[rand.num==1, 8]
nov.test.labels <- november.normed[rand.num==2, 8]


# make sure that we have the right split
# should be about 33%-67% split
nrow(nov.train)   # number of rows in training and test dataframes
nrow(nov.test)
length(nov.train.labels)    # length of target label vector
length(nov.test.labels)


# our model
# uses data frame nov.train and actual answer in vector nov.train.labels
# to classify data frame nov.test into new vector nov.pred
nov.pred <- knn(train=nov.train, test=nov.test, cl=nov.train.labels, k=3, prob=T)

# look at some of the precited vector
nov.pred[1:10]


# see how well the model did
library(gmodels)
CrossTable(nov.test.labels, nov.pred, prop.chisq = F)

# seems to classify Trump much better than Clinton
# maybe because he won more overall counties so more data -> better prediction














# plots ------------------------------

# would love to integrate a map of the US with this data

library(ggplot2)

# bar graphs for each state plotting whether Clinton won or lost
# in fake head-to-head with Trump
clinton.lead.plot <- ggplot(clinton.lead) +
  geom_bar(aes(x=state_abbreviation, y=clinton.lead), stat='identity') 
# here there be labs
clinton.lead.plot


# for each county, 
# plot fraction of votes received against percent white for each candidate
white.plot <- ggplot(election, aes(white, fraction_votes))
white.plot + geom_point(aes(colour = candidate, size=population_2014)) +
  # geom_jitter(position = position_jitter()) +    # figure out how to make this not look like it got the bubonic plague
  labs(title = "Primary Votes of Whites") +
  xlab("Percent of county that is white") +
  ylab("Fraction of votes received") +
  labs(colour = "Candidate", size = "Population in 2014")
# so at low percent white (left half of graph), Hillary does best and Bernie does worst
# at high percent white it's more of a jumble

# size of point as fraction of vote
college.plot <- ggplot(election, aes(college, votes))
college.plot + geom_point(aes(colour = candidate, size=fraction_votes)) +
  # geom_jitter() +
  labs(title = "Primary Votes of College Educated") +
  xlab("Percent with college degrees") +
  ylab("Number of votes") +
  labs(colour = "Candidate", size = "Fraction of votes")


# what's up with outlier county with very high population?
# looks like there's also a county that cast a lot of votes
qplot(population_2014, votes, data = election)

election[which.max(election$population_2014), 
         c("fips_county_code", "state", "population_2014")]
# so outlier is fips code 6037 (LA county) according to Google
# they are the county with the highest population in the US (9,818,605)
# second is Cook County, IL at 5,194,675!

# take a look at other counties with high populations
find.outliers <- election %>%
  dplyr::select (
    fips_county_code, population_2014,
    state
  ) %>%
  nest() %>%  # squish three rows (for the three candidates) with same data into one
  select(-data) %>%  # take out column with nested data
  arrange(desc(
    population_2014
  )) %>%
  print(n = 10)




# -------------- by state -------------
# summarise things by state
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
  ) %>%
  print(n=15)

# spread election by state
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
election.by.state.spread


# percap, w.b_gap, tot.votes by state
# all three candidates
by.state.plot <- ggplot(election.by.state, aes(percap, w.b_gap))
by.state.plot + geom_point(aes(colour = candidate, size=tot.votes)) +
  facet_grid(. ~ candidate) +
  xlab("Per capita income") +
  ylab("White-black gap per county") +
  labs(size="Total Votes", colour = "Candidate")




