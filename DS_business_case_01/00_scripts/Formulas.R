#Tidying data with pivot_longer():
diamonds2 <- readRDS("diamonds2.rds")

diamonds2 %>% head(n = 5)
#Solution:
diamonds2 %>% 
  pivot_longer(cols      = c("2008", "2009"), 
               names_to  = 'year', 
               values_to = 'price') %>% 
  head(n = 5)

# Tidying data with pivot_wider():
diamonds3 <- readRDS("diamonds3.rds")

diamonds3 %>% head(n = 5)
#Solution:
diamonds3 %>% 
  pivot_wider(names_from  = "dimension",
              values_from = "measurement") %>% 
  head(n = 5)

#Tidying data with separate():
diamonds4 <- readRDS("diamonds4.rds")

diamonds4
#Solution:
diamonds4 %>% 
  separate(col = dim,
           into = c("x", "y", "z"),
           sep = "/",
           convert = T)

#Tidying data with unite(): 
diamonds5 <- readRDS("diamonds5.rds")

diamonds5
#Solution:
diamonds5 %>% 
  unite(clarity, clarity_prefix, clarity_suffix, sep = '')

#filter() picks cases based on their values (formula based filtering). 
#Use slice() for filtering with row numbers.
library(ggplot2) # To load the diamonds dataset
library(dplyr)
diamonds %>% 
  filter(cut == 'Ideal' | cut == 'Premium', carat >= 0.23) %>% 
  head(5)

diamonds %>% 
  filter(cut == 'Ideal' | cut == 'Premium', carat >= 0.23) %>% 
  slice(3:4)

#LOGICAL OPERATORS SUPPORTED BY R:

#  <	less than
#  <=	less than or equal to
#  >	greater than
#  >=	greater than or equal to
#  ==	exactly equal to
#  !=	not equal to
#  !x	Not x. Gives the opposite logical value.
#  x | y	x OR y
#  x & y	x AND y

#arrange() changes the ordering of the rows
diamonds %>% 
  arrange(cut, carat, desc(price))

#select() picks (or removes) variables based on their names
diamonds %>% 
  select(color, clarity, x:z) %>% 
  head(n = 5)
#exclusive select:
diamonds %>% 
  select(-(x:z)) %>% 
  head(n = 5)

#starts_with() / ends_with(): helper that selects every column tht starts with a prefix or ends with a suffix
#contains(): A select helper that selects any column containing a string of text.

#everything(): a select() helper that selects every column that has not already been selected. Good for reordering.
diamonds %>% 
  select(x:z, everything()) %>% 
  head(n = 5)

#rename() changes the name of a column.
diamonds %>% 
  rename(var_x = x) %>% 
  head(n = 5)

#mutate() adds new variables that are functions of existing variables and preserves existing ones.
diamonds %>% 
  mutate(p = x + z, q = p + y) %>% 
  select(-(depth:price)) %>% 
  head(n = 5)

#transmute() adds new variables and drops existing ones.
diamonds %>% 
  transmute(carat, cut, sum = x + y + z) %>% 
  head(n = 5)

#bind_cols() and bind_rows(): binds two tibbles column-wise or row-wise.

#group_by() and summarize() reduces multiple values down to a single summary
diamonds %>% 
  group_by(cut) %>% 
  summarize(max_price  = max(price),
            mean_price = mean(price),
            min_price  = min(price))
#glimpse() You can apply this function to get a glimpse of your dataset.

#DATA TYPES:
#character:  "a”, “dice"
#numeric	1, 10.23
#integer	1L, 10L
#logical	TRUE, FALSE

#DATA STRUCTURES / DATASETS:
#atomic vector
#list
#matrix
#data frame
#factors

#ATTRIBUTES
#names
#dimnames
#dim
#class
#attributes (contain metadata)

#Easy and fast parsing of date-times: ymd(), ymd_hms(), dmy(), dmy_hms, mdy(), 
#Simple functions to get and set components of a date-time, such as:
#year(), month(), mday(), hour(), minute() and second()






