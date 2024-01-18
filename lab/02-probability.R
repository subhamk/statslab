# R script for data exercises in '02-probability'
# Notes: 
# Commented lines begin with # symbol
# Any changes you make here will be lost once you exit

# get data
lcf <- readr::read_csv("../data/lcf.csv")

attach(lcf)

# total number of respondents with *earned income* as their main income source
sum(inc_src_earned)

# proportion in the sample
mean(inc_src_earned)

# proportion reporting other income source
mean(inc_src_other)

# check if [1 - P(other)] = P(earned)
identical(1 - mean(inc_src_other), mean(inc_src_earned))

# Mutually exclusive and exhaustive events
mean(inc_src_earned) + mean(inc_src_other)

# Let $A$ be the event that earned income is the main income source and $B$ the event that the respondent is female.

# Pr(A)
mean(inc_src_earned)

# Pr(B)
mean(gender_female)

# Pr(A and B)
mean(inc_src_earned & gender_female)

# Addition Rule:
# Pr(A or B) = P(A) + P(B) - P(A and B)
mean(inc_src_earned) + mean(gender_female) - mean(inc_src_earned & gender_female)

# Independent?

# Pr(A and B)
mean(inc_src_earned & gender_female)

# P(A)xP(B)
mean(inc_src_earned) * mean(gender_female)

# Mutually exclusive events
mean(gender_female & gender_male)

# Conditional probability

# P(A|B)
mean(inc_src_earned[gender_female == 1])

# P(A|B) = P(A and B) / P(B)
mean(inc_src_earned & gender_female) / mean(gender_female)

# Conditional probability is not commutative

# P(B|A)
mean(gender_female[inc_src_earned == 1])

# P(A|B)
mean(inc_src_earned[gender_female == 1])


# Law of total probability

# P(A)
mean(inc_src_earned)

# P(A|B). P(B) + P(A|¬B) . P(¬B)
mean(inc_src_earned[gender_female == 1]) * mean(gender_female) + 
  mean(inc_src_earned[gender_male == 1]) * mean(gender_male)

# Bayes theorem

# P(B|A) = [P(A|B) * P(B)] / P(A)
(mean(inc_src_earned[gender_female == 1]) * mean(gender_female)) / mean(inc_src_earned)


detach(lcf)