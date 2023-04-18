library(dplyr)
library(ggplot2)
library(lubridate)

store <- read_excel("Sample - Superstore Sales.xls")

store$`Order Date` <- as.Date(store$`Order Date`, format = "%Y-%m-%d")

#find total profit & total sales grouped by order date
sales_data_agg <- store %>%
  group_by(`Order Date`) %>% # Group by month
  summarize(total_profit = sum(Profit), # Calculate total profit
            total_sales = sum(Sales))

sales_data_agg
#find the Profit and Sales Trend 
ggplot(data = sales_data_agg, aes(x = `Order Date`)) + # Set x-axis to month
  geom_line(aes(y = total_profit, color = "Profit")) + # Plot profit as line
  geom_line(aes(y = total_sales, color = "Sales")) + # Plot sales as line
  labs(title = "Profit and Sales Trend by Month", # Set chart title
       x = "Month", # Set x-axis label
       y = "Amount", # Set y-axis label
       color = "Metric") + # Set color legend label
  scale_color_manual(values = c("Profit" = "red", "Sales" = "blue")) # Set custom colors for lines

correlation <- cor(store$Sales, store$Profit)
correlation

model <- lm(Profit ~ Sales, data = store)

model

# Create a scatter plot of sales vs profit
plot(store$Sales, store$Profit, main = "Scatter Plot of Sales vs Profit",
     xlab = "Sales", ylab = "Profit")

# Add the regression line to the scatter plot
abline(model, col = "red")


model222 <- lm(Discount ~ Profit, data = store)
plot(store$Profit, store$Discount, main = "Scatter Plot of Sales vs Discount",
     xlab = "Profit", ylab = "Discount")

# Add the regression line to the scatter plot
abline(model222, col = "blue")

correlation2 <- cor(store$Discount, store$Profit)
correlation2


# Create a boxplot for sales_data_agg
ggplot(data = sales_data_agg, aes(y = total_profit)) + 
  geom_boxplot()



#Find summary of Sales Data Aggregation
summary(sales_data_agg)

# Save modified Boston data frame as a CSV file
write.csv(Superstore, "sales data agg", row.names = FALSE)
