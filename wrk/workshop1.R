# R script for data exercises in Workshop 1
# Notes: 
# Commented lines begin with # symbol
# Any changes you make here will be lost once you exit

# ---------------------------------------------------
# Question 2
# Data: simulated_heights
# ---------------------------------------------------

# import data
height <- readr::read_csv("../data/sim_heights.csv")

x <- height$x   # x is a vector of heights

# ---- 2(a) ----
# mean
m <- mean(x)
m

# variance
v <- var(x)
v

# standard deviation
s <- sd(x)
s

# ---- 2(b) ----
# z score
z <- (x - m)/s
head(z)

# --- 2(c) ---
round(mean(z), 2)
var(z)

# --- 2(d) ---

# plot ECDF of x
ggplot(tibble(x), aes(x)) +
  stat_ecdf(geom = "step", col = "steelblue", linewidth = 1) +
  labs(
    x = "Height (X)",
    y = "ECDF (X)"
  ) +
  theme_bw()

# ---------------------------------------------------
# Question 3
# Data: simulated_heights
# ---------------------------------------------------

# get data
freedom <- readr::read_csv("../data/freedom.csv")

# prepare data
freedom_2020 <- freedom %>%
  dplyr::select(-Region_Code) %>%           # drop Region_Code
  mutate(Status = factor(Status),
         Region_Name = factor(Region_Name),
         is_ldc = factor(is_ldc)) %>%       # convert to factor
  filter(year == 2020)                      # keep data for the year 2020


head(freedom_2020)                          # print first few lines of the data

# ---- 3(a) ----
freedom_2020 %>% 
  tabyl(is_ldc, Status) %>%             # create table
  adorn_totals(c("row", "col"))         # add row and col totals

# ---- 3(b) ----
mean(freedom_2020$CL)

# ---- 3(c) ----
fh_ranking <- freedom_2020 %>%          # use 2020 FH data
  group_by(Status) %>%                  # group data by Status
  summarise(mean_cl = mean(CL, na.rm = TRUE))   # calculate the mean by group var.
fh_ranking

# ---- 3(d) ----
ggplot(data = fh_ranking, aes(x = Status, y = mean_cl)) +
  geom_bar(stat = 'identity',
           fill = 'steelblue',
           alpha = 0.6) +
  geom_text(
    aes(label = round(mean_cl, 2)),
    colour = "black",
    size = 3,
    vjust = 1.5,
    position = position_dodge(.9)
  ) +
  labs(
    title = "Civil Liberty Rating by Status",
    subtitle = "Note: Smaller CL values indicate fewer restrictions",
    x = "Status",
    y = "CL Rating"
  ) +
  theme_bw()

# ---- 3(e) ----
fh_ranking_m <- freedom_2020 %>%
  group_by(Status, is_ldc) %>%          # group data
  summarise(mean_cl = mean(CL, na.rm = TRUE)) %>%   # get averages
  mutate(label_y = cumsum(mean_cl))     # cumulative sum
fh_ranking_m

# multibarplot
ggplot(data = fh_ranking_m, aes(x = Status, y = mean_cl, fill = is_ldc)) +
  geom_bar(stat = 'identity', alpha = 0.6, position = position_dodge()) +
  scale_fill_brewer(palette = "Blues") +
  geom_text(
    aes(label = round(mean_cl, 2)),
    colour = "black",
    size = 3,
    vjust = 1.5,
    position = position_dodge(.9)
  ) +
  labs(
    title = "Civil Liberty Rating by LDC and Status",
    subtitle = "Note: Smaller values indicate fewer constraints",
    x = "Status",
    y = "CL Rating"
  ) +
  theme_bw()














