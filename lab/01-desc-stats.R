# R script for '01-desc-stats'
# Notes: 
# Commented lines begin with # symbol
# Any changes you make here will be lost once you exit
# Some codes have been slightly modified
# Data from from tidytuesday

# ---------------------------------------------------
# FIFA World Cup Matches 
# ---------------------------------------------------

# Import data
wcmatches <- readr::read_csv("./data/wcmatches.csv")

# get a snapshot of the data
head(wcmatches)   # prints first few lines

glimpse(wcmatches)

# create new data object
wcm2018 <- wcmatches %>%
  filter(year == 2018) %>%          # filter rows based on some condition
  select(home_team, away_team,
         home_score, away_score, outcome)  # choose columns to keep

# create a new variable
wcm2018 <- wcm2018 %>%
  mutate(total_score = home_score + away_score)   # creates a new variable

# plot data as a bar chart
ggplot(data = wcm2018, aes(total_score)) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Distribution of total goals scored in FIFA matches",
    subtitle = "Year = 2018",
    x = "Goals Scored per match",
    y = "Count"
  )

# horizontal bar chart
ggplot(data = wcm2018, aes(total_score)) +
  geom_bar(fill = "steelblue") +
  coord_flip() +      # produces horizontal bars
  labs(
    title = "Distribution of total goals scored in FIFA matches",
    subtitle = "2018",
    x = "Goals Scored per match",
    y = "Count"
  )

# create new data object
wcm <- wcmatches %>%
  mutate(total_score = home_score + away_score) %>%     # create new variable
  group_by(year) %>%                                    # group by year
  summarise(total_score = sum(total_score))             # add up goals scored

head(wcm)

# plot total score by year as a line chart
ggplot(data = wcm, aes(x = year, y = total_score)) +
  geom_point() +                                # plot the points
  geom_line(color = "steelblue", linewidth = 1) +    # connect the points
  labs(
    title = "Goals scored in FIFA matches by year",
    x = "Year",
    y = "Count"
  )

# scatter plot of home score and away score
wcmatches %>% filter(stage == "Final") %>%
  ggplot(aes(x = home_score, y = away_score)) +
  geom_point(size = 3) +
  labs(
    title = "Home scores vs. away scores",
    x = "Home Score",
    y = "Away Score"
  ) +
  theme_classic()

# pie chart
wcmatches %>%
  mutate(total_score = home_score + away_score) %>%     # create new variable
  filter(stage == "Final" | stage == "Semifinals" | stage == "Quarterfinals") %>%
  ggplot(aes(x = "", y = total_score, fill = stage)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  theme_void()

# ---------------------------------------------------
# Penn World Tables for the year 2018
# Graphical Description
# ---------------------------------------------------

pwt2018 <- readr::read_csv("./data/pwt2018.csv")

# histogram of per capita GDP
ggplot(data = pwt2018, aes(pcgdp)) +
  geom_histogram(fill = "steelblue") +
  labs(
    title = "Histogram of per capita GDP in 2018",
    x = "GDP per capita"
  )

# manually specify number of bins
ggplot(data = pwt2018, aes(pcgdp)) +
  geom_histogram(bins = 10, fill = "steelblue") +
  labs(
    title = "Histogram of per capita GDP in 2018, (10 bins)",
    x = "GDP per capita"
  )

# manually specify break points
ggplot(data = pwt2018, aes(pcgdp)) +
  geom_histogram(
    breaks = c(2000, 5000, 15000, 25000, 35000, 50000), 
    fill = "steelblue") +
  labs(
    title = "Histogram of per capita GDP in 2018, (specified breaks)",
    x = "GDP per capita"
  )

# density plot
ggplot(data = pwt2018, aes(pcgdp)) +
  geom_density(fill = "steelblue", colour = "white") +
  labs(
    title = "Density plot of per capita GDP in 2018, (specified breaks)",
    x = "GDP per capita"
  )

# ---------------------------------------------------
# Penn World Tables
# Numerical Summaries
# ---------------------------------------------------

pwt_2000_2019 <- readr::read_csv("./data/pwt_2000_2019.csv") 

# average per capita gdp
pwt_2000_2019 %>%                           # PWT dataset
  filter(year == 2018) %>%                  # subset rows by filtering
  summarise(
    avg_pcgdp = mean(pcgdp, na.rm = TRUE)   # get average
  )

# summarise pcgdp by year
pwt_2000_2019 %>%                         # PWT dataset
  filter(year >= 2010) %>%                # subset by keeping years>=2000
  group_by(year) %>%                      # group data by variable year
  summarise(
    avg_pcgdp = mean(pcgdp, na.rm = TRUE) # get average by year
  ) %>%
  ungroup()                               # ungroup
  
# weighted average
pwt_2000_2019 %>%                         # PWT dataset
  filter(year >= 2010) %>%                # subset by keeping years>=2000
  group_by(year) %>%                      # group data by variable year
  summarise(
    wt_avg_rgdpo = weighted.mean(rgdpo, pop, na.rm = TRUE),  # get pop weighted average
    avg_rgdpo = mean(rgdpo, na.rm = TRUE)   # simple average
  ) %>%
  ungroup()                               # ungroup

# median
pwt2018 %>%
  summarise(med_pcgdp = median(pcgdp, na.rm = TRUE))

# standard deviation
pwt2018 %>%                           
  summarise(
    sd_pcgdp = sd(pcgdp, na.rm = TRUE)  # get standard deviation
  )

# Compute standardized score
pwt2018 %>%
  select(country, pcgdp) %>%                           
  mutate(
    avg_pcgdp = mean(pcgdp, na.rm = TRUE),  # average pc GDP
    sd_pcgdp = sd(pcgdp, na.rm = TRUE),     # standard deviation pc GDP
    pcgdp_dev = pcgdp - avg_pcgdp,          # deviation from mean
    z_pcgdp = pcgdp_dev / sd_pcgdp          # z-score = scaling (x - mean(x)) by sd(x)
  )

# IQR
pwt2018 %>%                           
  summarise(
    iqr_pcgdp = IQR(pcgdp, na.rm = TRUE)  # get standard deviation
  )


# Skewness

# function to compute skewness
skew <- function(x){
  n <- length(x)                # number of elements in x
  z <- (x - mean(x)) / sd(x)    # z score
  sum(z^3) / n                  # skewness
}

# apply function to pcgdp series
skew(pwt2018$pcgdp)             # apply skew function on pcgdp


# Excess Kurtosis
kurtosis <- function(x){
  n <- length(x)                # number of elements in x
  z <- (x - mean(x)) / sd(x)    # z score
  sum(z^4)/n - 3                # excess kurtosis
}
kurtosis(pwt2018$pcgdp)         # apply kurtosis function on pcgdp

