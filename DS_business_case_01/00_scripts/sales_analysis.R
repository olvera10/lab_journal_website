# datascience at NIT ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----
library(tidyverse)
library(lubridate)

# 2.0 Importing Files ----
# A good convention is to use the csv file name and suffix it with tbl for the data structure tibble
order_items_tbl <- read_csv(file = "DS_business_case_01/00_data/01_e-commerce/01_raw_data/olist_order_items_dataset.csv") 
products_tbl    <- read_csv(file = "DS_business_case_01/00_data/01_e-commerce/01_raw_data/olist_products_dataset.csv")
orders_tbl      <- read_csv(file = "DS_business_case_01/00_data/01_e-commerce/01_raw_data/olist_orders_dataset.csv")

# 3.0 Examining Data ----
# Method 1: Print it in the console
order_items_tbl
products_tbl
orders_tbl
# Method 2: Click on the file in the environment tab
# Method 3: glimpse() function
glimpse(order_items_tbl) 

# 4.0 Joining Data ----
# by automatically detecting a common column
left_join(order_items_tbl, products_tbl)
# Chaining commands with the pipe and assigning it to order_items_joined_tbl
order_items_joined_tbl  <- order_items_tbl %>%
  left_join(orders_tbl) %>%
  left_join(products_tbl)
# Examine the results with glimpse()
order_items_joined_tbl %>% glimpse()
# 5.0 Wrangling Data ----
#take a look at 1 column
order_items_joined_tbl$product.category.name

# All actions are chained with the pipe already. You can perform each step separately and use glimpse() or View() to validate your code. Store the result in a variable at the end of the steps.
order_items_wrangled_tbl <- order_items_joined_tbl %>%
  
  # 5.1 Separate product category name in main and sub
  separate(col    = product.category.name,
           into   = c("main.category.name", "sub.category.name"),
           sep    = " - ",
           # Setting remove to FALSE to keep the original column
           remove = FALSE) %>%
  
  # 5.2 Add the total price (price + freight value) 
  # Add a column to a tibble that uses a formula-style calculation of other columns
  mutate(total.price = price + freight.value) %>% 
  
  # 5.3 Optional: Reorganize. Using select to grab or remove unnecessary columns
  # 5.3.1 by exact column name
  select(-shipping.limit.date, -order.approved.at) %>%
  
  # 5.3.2 by a pattern (we don't need columns that start with "product." or end with ".date")
  # You can use the select_helpers to define patterns. 
  # Type ?ends_with and click on Select helpers in the documentation
  select(-starts_with("product.")) %>%
  select(-ends_with(".date")) %>%
  
  # 5.3.3 Actually we need the column "product.id". Let's bind it back to the data
  bind_cols(order_items_joined_tbl %>% select(product.id)) %>% 
  
  # 5.3.4 You can reorder the data by selecting the columns in your desired order.
  # You can use select_helpers like contains() or everything()
  select(contains("timestamp"), contains(".id"),
         main.category.name, sub.category.name, price, freight.value, total.price,
         everything()) %>% 
  
  # 5.4 Rename columns because we actually wanted underscores instead of the dots
  # (one at the time vs. multiple at once)
  rename(order_date = order.purchase.timestamp) %>% 
  set_names(names(.) %>% str_replace_all("\\.", "_"))

# 6.0 Business Insights ----
# 6.1 Sales by Year ----

# Step 1 - Manipulate
revenue_by_year_tbl <- order_items_wrangled_tbl %>%
  
  # Select columns
  select(order_date, total_price) %>%
  
  # Add year column
  mutate(year = year(order_date)) %>%
  
  # Grouping by year and summarizing sales
  group_by(year) %>% 
  summarize(revenue = sum(total_price)) %>%
  
  # Optional: Add a column that turns the numbers into a currency format (makes it in the plot optically more appealing)
  mutate(revenue_text = scales::dollar(revenue))

revenue_by_year_tbl

# Step 2 - Visualize
revenue_by_year_tbl %>%
  
  # Setup canvas with the columns year (x-axis) and revenue (y-axis)
  ggplot(aes(x = year, y = revenue)) +
  
  # Geometries
  geom_col(fill = "#2DC6D6") + # Use geom_col for a bar plot
  geom_label(aes(label = revenue_text)) + # Adding labels to the bars
  geom_smooth(method = "lm", se = FALSE) + # Adding a trendline
  
  # Formatting
  scale_y_continuous(labels = scales::dollar) + # Change the y-axis
  labs(
    title    = "Revenue by year",
    subtitle = "Upward Trend",
    x = "", # Override defaults for x and y
    y = "Revenue"
  )

# 6.2 Sales by Year and Category 2 ----

# Step 1 - Manipulate
revenue_by_year_cat_main_tbl <- order_items_wrangled_tbl %>%
  
  # Select columns and add a year
  select(order_date, total_price, main_category_name) %>% 
  mutate(year = year(order_date)) %>%
  
  # Filter  > 1.000.000
  group_by(main_category_name) %>% 
  filter(sum(total_price) > 1000000) %>% # If you run the code up here, R will tell you that we have 6 groups
  ungroup() %>%
  
  # Group by and summarize year and main catgegory
  group_by(year, main_category_name) %>% 
  summarise(revenue = sum(total_price)) %>% 
  ungroup() %>%
  
  # Format $ Text
  mutate(revenue_text = scales::dollar(revenue))

revenue_by_year_cat_main_tbl  

# Step 2 - Visualize
revenue_by_year_cat_main_tbl %>%
  
  # Set up x, y, fill
  ggplot(aes(x = year, y = revenue, fill = main_category_name)) +
  
  # Geometries
  geom_col() + # Run up to here to get a stacked bar plot
  
  # Facet
  facet_wrap(~ main_category_name) +
  
  # Formatting
  scale_y_continuous(labels = scales::dollar) +
  labs(
    title = "Revenue by year and main category",
    subtitle = "Each product category has an upward trend",
    fill = "Main category" # Changes the legend name
  )


# 7.0 Writing Files ----

# If you want to interact with the filesystem use the fs package
install.packages("fs")
library(fs)
fs::dir_create("DS_business_case_01/00_data/01_e-commerce/04_wrangled_data_student")

# 7.1 Excel ----
install.packages("writexl")
library("writexl")
order_items_wrangled_tbl %>% 
  write_xlsx("DS_business_case_01/00_data/01_e-commerce/04_wrangled_data_student/order_items.xlsx")

# 7.2 CSV ----
order_items_wrangled_tbl %>% 
  write_csv("DS_business_case_01/00_data/01_e-commerce/04_wrangled_data_student/order_items.csv")

# 7.3 RDS ----
order_items_wrangled_tbl %>% 
  write_rds("DS_business_case_01/00_data/01_e-commerce/04_wrangled_data_student/order_items.rds")
