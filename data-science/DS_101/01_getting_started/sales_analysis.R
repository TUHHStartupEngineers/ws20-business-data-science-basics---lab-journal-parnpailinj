# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----
library(tidyverse)
library(readxl)
library(lubridate)
library("writexl")

# 2.0 Importing Files ----
bikes_tbl      <- read_excel(path = "data-science/DS_101/00_data/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel("data-science/DS_101/00_data/01_bike_sales/01_raw_data/orderlines.xlsx")
bikeshops_tbl  <- read_excel("data-science/DS_101/00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")

# 3.0 Examining Data ----

# 4.0 Joining Data ---

bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

# # 5.0 Wrangling Data ----
bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
  separate(col    = category,
           into   = c("category.1", "category.2", "category.3"),
           sep    = " - ") %>%
  mutate(total.price = price * quantity) %>%

  select(-...1, -gender) %>%

  select(-ends_with(".id")) %>%

  bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>%


  select(order.id, contains("order"), contains("model"), contains("category"),
         price, quantity, total.price,
         everything()) %>%

  rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))
# 
# # 6.0 Business Insights ----
# # 6.1 Sales by Year ----
# 
# # Step 1 - Manipulate
# sales_by_year_tbl <- bike_orderlines_wrangled_tbl %>%
#   
#   # Select columns
#   select(order_date, total_price) %>%
#   
#   # Add year column
#   mutate(year = year(order_date)) %>%
#   
#   # Grouping by year and summarizing sales
#   group_by(year) %>% 
#   summarize(sales = sum(total_price)) %>%
#   
#   # Optional: Add a column that turns the numbers into a currency format 
#   # (makes it in the plot optically more appealing)
#   # mutate(sales_text = scales::dollar(sales)) <- Works for dollar values
#   mutate(sales_text = scales::dollar(sales, big.mark = ".", 
#                                      decimal.mark = ",", 
#                                      prefix = "", 
#                                      suffix = " €"))
# 
# sales_by_year_tbl
# 
# # Step 2 - Visualize
# sales_by_year_tbl %>%
#   ggplot(aes(x = year, y = sales)) +
#   geom_col(fill = "#2DC6D6") + # Use geom_col for a bar plot
#   geom_label(aes(label = sales_text)) + # Adding labels to the bars
#   geom_smooth(method = "lm", se = FALSE) + # Adding a trendline
#   
#   scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
#                                                     decimal.mark = ",", 
#                                                     prefix = "", 
#                                                     suffix = " €")) +
#   labs(
#     title    = "Revenue by year",
#     subtitle = "Upward Trend",
#     x = "Year", # Override defaults for x and y
#     y = "Revenue"
#   )
# 
# # 6.2 Sales by Year and Category  ----
# 
# # Step 1 - Manipulate
# sales_by_year_cat_1_tbl <- bike_orderlines_wrangled_tbl %>%
#   
#   # Select columns and add a year
#   select(order_date, total_price, category_1) %>%
#   mutate(year = year(order_date)) %>%
#   
#   # Group by and summarize year and main catgegory
#   group_by(year, category_1) %>%
#   summarise(sales = sum(total_price)) %>%
#   ungroup() %>%
#   
#   # Format $ Text
#   mutate(sales_text = scales::dollar(sales, big.mark = ".", 
#                                      decimal.mark = ",", 
#                                      prefix = "", 
#                                      suffix = " €"))
# # Step 2 - Visualize
# sales_by_year_cat_1_tbl %>%
# 
#   ggplot(aes(x = year, y = sales, fill = category_1)) +
#   geom_col() + # Run up to here to get a stacked bar plot
# 
#   facet_wrap(~ category_1) +
# 
#   scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
#                                                     decimal.mark = ",", 
#                                                     prefix = "", 
#                                                     suffix = " €")) +
#   labs(
#     title = "Revenue by year and main category",
#     subtitle = "Each product category has an upward trend",
#     fill = "Main category" # Changes the legend name
#   )
# 
# 
# # 7.0 Writing Files ----
# bike_orderlines_wrangled_tbl %>%
#   write_xlsx("data-science/DS_101/00_data/01_bike_sales/02_wrangled_data/bike_orderlines.xlsx")
# 
# bike_orderlines_wrangled_tbl %>% 
#   write_csv("data-science/DS_101/00_data/01_bike_sales/02_wrangled_data/bike_orderlines.csv")
# 
# bike_orderlines_wrangled_tbl %>% 
#   write_rds("data-science/DS_101/00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")
# 


#Challenge 1

bike_orderlines_wrangled_city_state_tbl <- bike_orderlines_wrangled_tbl %>%
  separate(col    = location,
           into   = c("city", "state"),
           sep    = ",") 

sales_by_city_state_tbl <- bike_orderlines_wrangled_city_state_tbl %>%
  select(order_date, state, city, total_price) %>%
  mutate(year = year(order_date)) %>%
  group_by(state, city) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  
  # Format $ Text
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))  

sales_by_city_state_tbl %>%
  ggplot(aes(x = state, y = sales)) +
  geom_col(fill = "#2DC6D6") + # Use geom_col for a bar plot
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title    = "Revenue by state",
    subtitle = "",
    x = "State", # Override defaults for x and y
    y = "Revenue"
  )

#Challenge 2
sales_by_location_year_tbl <- bike_orderlines_wrangled_city_state_tbl %>%
  select(order_date, state, city, total_price) %>%
  mutate(year = year(order_date)) %>%
  group_by(state, year) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  
  # Format $ Text
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))  
# Step 2 - Visualize
sales_by_location_year_tbl %>%
  
  ggplot(aes(x = year, y = sales, fill = state)) +
  geom_col() + # Run up to here to get a stacked bar plot
  
  facet_wrap(~ state) +
  
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  
  labs(
    title = "Revenue by year and state",
    subtitle = "",
    fill = "State" # Changes the legend name
  )

