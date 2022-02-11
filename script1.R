dir.create("data")
dir.create("data_output")
dir.create("fig_output")

download.file("https://ndownloader.figshare.com/files/11492171",
              "data/SAFI_clean.csv", mode = "wb")

area_hectares <- 1.0


# Alt + - = <-

# Ctrl + l = delete the row

# Commenting lines = CTRL + SHIFT + C

# args(round) = information about function

# class() = returns the type of element

# Length() = Returns lenght of element 

# str() = Provides structure of the element

# with [] you can extract values from vectors, mutiple? do [C()]

# You can combine multiple tests using & (both conditions are true, AND) or | (at least one of the conditions is true: >/<

# The function %in% allows you to test if any of the elements of a search vector (on the left hand side) are found in the target vector (on the right hand side)

# na.rm=TRUE to calculate the result while ignoring the missing values

#  The ! character is also called the NOT operator

# read_csv2 for different deciaml sperators . or , ? read_csv

# interviews[, -1] # The whole tibble, except the first column

# %>% (THAN, we this)  Pipes let you take the output of one function and send it directly to the next, which is useful when you need to do many things to the same dataset. 

?round
args(signif)



hh_members <- c(3, 7, 10, 6)
hh_members

respondent_wall_type <- c("muddaub", "burntbricks", "sunbricks")
respondent_wall_type


possessions <- c("bicycle", "radio", "television")
possessions <- c(possessions, "mobile_phone") # add to the end of the vector
possessions <- c("car", possessions) # add to the beginning of the vector
possessions


num_char <- c(1, 2, 3, "a")
num_logical <- c(1, 2, 3, FALSE)
char_logical <- c("a", "b", "c", TRUE)
tricky <- c(1, 2, 3, "4")

class(tricky)


num_logical <- c(1, 2, 3, TRUE)
char_logical <- c("a", "b", "c", TRUE)
combined_logical <- c(num_logical, char_logical)



rooms <- c(1, 2, 1, 1, NA, 3, 1, 3, 2, 1, 1, 8, 3, 1, NA, 1)

rooms2 <- na.omit(rooms)

median[na.omit(rooms)]

rooms_above_2 <- rooms2[rooms2 > 2]

library(tidyverse)


install.packages("here")
library(tidyverse)
library(here)
interviews <- read_csv(
  here("data", "SAFI_clean.csv"), 
  na = "NULL")

class(interviews)

dim(interviews)

summary(interviews)

glimpse(interviews)

head_interviews <- interviews[c(1:6), ]

interviews[-(7:131), ] 

interviews["village"] 

interviews$village

interview_100 <- interviews[100, ]

nrow(interviews)

interview_last <- interviews[131, ]
tail(interviews)

n_rows <- nrow(interviews)
interviews_last <- interviews[n_rows, ]

interviews_middle <- interviews[median(1:n_rows), ]

nrow(interviews)

respondent_floor_type <- factor(c("earth", "cement", "cement", "earth"))



respondent_floor_type <- factor(respondent_floor_type, levels = c("earth", "cement"))

respondent_floor_type_ordered <- factor(respondent_floor_type, 
                                        ordered = TRUE)

year_fct <- factor(c(1990, 1983, 1977, 1998, 1990))

as.numeric(as.character(year_fct))

memb_assoc <- interviews$memb_assoc

## replace the missing data with "undetermined"
memb_assoc[is.na(memb_assoc)] <- "undetermined"

## convert it into a factor
memb_assoc <- as.factor(memb_assoc)

## let's see what it looks like
memb_assoc

memb_assoc <- factor(memb_assoc, 
                     levels = c("No", "Undetermined", "Yes"))

levels(memb_assoc) <- c("No", "Undetermined", "Yes")


memb_assoc <- factor(memb_assoc, 
                     levels = c("No", "Yes", "Undetermined"))

plot(memb_assoc)


library(lubridate)
dates <- interviews$interview_date
str(dates)

interviews$day <- day(dates)
interviews$month <- month(dates)
interviews$year <- year(dates)
interviews



## load the tidyverse
library(tidyverse)
library(here)

interviews <- read_csv(here("data", "SAFI_clean.csv"), na = "NULL")

## inspect the data
interviews

## preview the data
# view(interviews)

fitler(interviews, village == "Chirodzo")

interviews %>% filter(memb_assoc == "yes") %>%
  select(affect_conflicts:liv_count, no_meals)

interviews %>%
  mutate(people_per_room = no_membrs / rooms)

interviews_total_meals <- interviews %>%
  mutate(total_meals = no_membrs * no_meals) %>%
  filter(total_meals > 20) %>%
  select(village, total_meals)


interviews %>%
   group_by(village) %>%
  summarize(Mean_H = mean(no_membrs),
                          Min_h = min(no_membrs),
                                      Max_h = max(no_membrs),
            n = n())

?n


interviews_wide <- interviews %>%
  mutate(wall_type_logical = TRUE) %>%
  pivot_wider(names_from = respondent_wall_type,
              values_from = wall_type_logical,
              values_fill = list(wall_type_logical = FALSE))

args(filter)
interviews_wide <- interviews %>%
  mutate(wall_type_logical = TRUE)

interviews_wide <- interviews %>%
  mutate(wall_type_logical = TRUE) %>%
  pivot_wider(names_from = respondent_wall_type,
              values_from = wall_type_logical,
              values_fill = list(wall_type_logical = FALSE))

dim(interviews_wide)
dim(interviews)


interviews_long <- interviews_wide %>%
  pivot_longer(cols = burntbricks:sunbricks,
               names_to = "respondent_wall_type",
               values_to = "wall_type_logical")



interviews_items_owned <- interviews %>%
  separate_rows(items_owned, sep = ";") %>%
  replace_na(list(items_owned = "no_listed_items")) %>%
  mutate(items_owned_logical = TRUE) %>%
  pivot_wider(names_from = items_owned,
              values_from = items_owned_logical,
              values_fill = list(items_owned_logical = FALSE))

nrow(interviews_items_owned)

interviews_items_owned <- interviews %>%
  separate_rows(items_owned, sep = ";") %>%
replace_na(list(items_owned = "no_listed_items")) %>%
  mutate(items_owned_logical = TRUE)



  