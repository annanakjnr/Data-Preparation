# Advanced Dimensional Modeling and Data Quality Assessment

# Loading the necessary libraries
library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(DBI)
library(RSQLite)

# Loading the dataset
raw_data <- read_csv("retail_sales_raw.csv")

glimpse(raw_data)
colSums(is.na(raw_data))

# Transforming the dataset 
# Handling missing values 

clean_data <- raw_data %>% 
  
  mutate( 

    # Replacing missing categorical values 
    
    City = ifelse(is.na(City), "Unknown", City), 
    
    Segment = ifelse(is.na(Segment), "Unknown", Segment), 

    # Recovering missing numerical values where possible 
    
    Quantity = ifelse( 
      
      is.na(Quantity) & !is.na(Revenue) & !is.na(UnitPrice) & !is.na(Discount), 
      
      round(Revenue / (UnitPrice * (1 - Discount))), 
      
      Quantity 
      
    ), 
    
    UnitPrice = ifelse( 
      
      is.na(UnitPrice) & !is.na(Revenue) & !is.na(Quantity) & !is.na(Discount), 
      
      Revenue / (Quantity * (1 - Discount)), 
      
      UnitPrice 
      
    ), 
    
    
    Discount = ifelse( 
      
      is.na(Discount) & !is.na(Revenue) & !is.na(Quantity) & !is.na(UnitPrice), 
      
      1 - (Revenue / (Quantity * UnitPrice)), 
      
      Discount 
      
    ), 
    
    # Recalculating revenue where the components are available 
    
    Revenue = ifelse( 
      
      is.na(Revenue) & !is.na(Quantity) & !is.na(UnitPrice) & !is.na(Discount), 
      
      round(Quantity * UnitPrice * (1 - Discount), 2), 
      
      Revenue 
      
    ) 
    
  ) %>% 
  
  # Removing transactions that still lack values needed 
  
  # to calculate revenue reliably 
  
  filter( 
    
    !is.na(Quantity), 
    
    !is.na(UnitPrice), 
    
    !is.na(Discount) 
    
  ) %>% 
 
  # Standardizing formats 
  
  mutate(
    Segment         = str_to_title(Segment),
    PaymentMethod   = str_to_title(PaymentMethod),
    TransactionDate = as_date(parse_date_time(TransactionDate, orders = c("ymd", "mdy")))
  ) %>%
  
  
  # Filtering unnecessary columns 
  
  select(-Notes, -InternalFlag) 

# Inspecting the cleaned file 
glimpse(clean_data)

# Adding a profit metric using the custom function
set.seed(7)

profit_margin <- function(revenue, cost) {
  round((revenue - cost) / revenue, 4)
}

clean_data <- clean_data %>%
  mutate(
    Cost         = round(UnitPrice * runif(n(), 0.55, 0.75), 2),  # assumed cost-of-goods ratio
    TotalCost    = round(Cost * Quantity, 2),
    ProfitMargin = profit_margin(Revenue, TotalCost)
  )

# Data Quality Assessment
# Outlier Detection & Treatment
# Using the IQR Method

iqr_bounds <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  c(lower = q1 - 1.5 * iqr, upper = q3 + 1.5 * iqr)
}

revenue_bounds  <- iqr_bounds(clean_data$Revenue)
quantity_bounds <- iqr_bounds(clean_data$Quantity)

sum(clean_data$Revenue  < revenue_bounds["lower"]  | clean_data$Revenue  > revenue_bounds["upper"])
sum(clean_data$Quantity < quantity_bounds["lower"] | clean_data$Quantity > quantity_bounds["upper"])

# cap (winsorize) outliers at the fence instead of dropping the rows
clean_data <- clean_data %>%
  mutate(
    Revenue  = pmin(pmax(Revenue,  revenue_bounds["lower"]),  revenue_bounds["upper"]),
    Quantity = pmin(pmax(Quantity, quantity_bounds["lower"]), quantity_bounds["upper"])
  )

print(clean_data)


# Advanced Dimensional Modeling

# Creating an in-memory SQLite database 
con <- dbConnect(RSQLite::SQLite(), ":memory:")

# Creating the junk dimension frame, which combines low-cardinality attributes --
clean_data <- clean_data %>%
  mutate(
    TransactionDate = as.Date(TransactionDate),
    DiscountFlag = ifelse(Discount > 0, "Discounted", "Full Price"),
    Season = case_when(
      month(TransactionDate) %in% c(12, 1, 2) ~ "Winter",
      month(TransactionDate) %in% c(3, 4, 5)  ~ "Spring",
      month(TransactionDate) %in% c(6, 7, 8)  ~ "Summer",
      TRUE ~ "Fall"
    )
  )

# Transaction attributes 
dim_transaction_attributes <- clean_data %>%
  distinct(PaymentMethod, DiscountFlag, Season) %>%
  mutate(TransactionAttrKey = row_number()) %>%
  select(TransactionAttrKey, everything())

clean_data <- clean_data %>%
  left_join(dim_transaction_attributes, by = c("PaymentMethod", "DiscountFlag", "Season"))


# Preparing dimension data frames with surrogate keys 
# dim_date 
dim_date <- clean_data %>% 
  
  distinct(Date = as.Date(TransactionDate)) %>% 
  
  arrange(Date) %>% 
  
  mutate( 
    
    DateKey = row_number(), 
    
    Day = day(Date), 
    
    Month = month(Date), 
    
    MonthName = month(Date, label = TRUE, abbr = FALSE), 
    
    Quarter = quarter(Date), 
    
    Year = year(Date), 
    
    DayOfWeek = wday(Date, label = TRUE, abbr = FALSE), 
    
    Weekend = ifelse(wday(Date) %in% c(1, 7), "Yes", "No"), 
    
  ) 



#dim_customer 
dim_customer <- clean_data %>% 
  
  distinct(CustomerID, CustomerName, Email, City, State, Region, Segment) %>% 
  
  mutate(CustomerKey = row_number()) 



#dim_ product 
dim_product <- clean_data %>% 
  
  distinct(ProductID, ProductName, Category, Subcategory, Brand) %>% 
  
  mutate(ProductKey = row_number()) 


# dim_store 
dim_store <- clean_data %>% 
  
  distinct(StoreID, StoreName, StoreCity, StoreState, StoreRegion) %>% 
  
  mutate(StoreKey = row_number()) 


# fact_sales, which now carries TransactionAttrKey + the profit measures 
fact_sales <- clean_data %>%
  left_join(dim_date,     by = c("TransactionDate" = "Date")) %>%
  left_join(dim_customer, by = "CustomerID", relationship = "many-to-many") %>%
  left_join(dim_product,  by = "ProductID") %>%
  left_join(dim_store,    by = "StoreID") %>%
  transmute(
    SalesKey = row_number(),
    TransactionID, DateKey, CustomerKey, ProductKey, StoreKey, TransactionAttrKey,
    Quantity, UnitPrice, Discount, Revenue, Cost, TotalCost, ProfitMargin
  )

# Creating the bridge table frame
customer_product_bridge <- clean_data %>%
  left_join(dim_customer, by = "CustomerID", relationship = "many-to-many") %>%
  left_join(dim_product,  by = "ProductID") %>%
  group_by(CustomerKey, ProductKey) %>%
  summarise(
    TimesPurchased    = n(),
    FirstPurchaseDate = min(TransactionDate),
    LastPurchaseDate  = max(TransactionDate),
    .groups = "drop"
  ) %>%
  mutate(BridgeKey = row_number()) %>%
  select(BridgeKey, CustomerKey, ProductKey, TimesPurchased, FirstPurchaseDate, LastPurchaseDate)

nrow(fact_sales); nrow(dim_transaction_attributes); nrow(customer_product_bridge)
head(dim_transaction_attributes)
head(customer_product_bridge)

# Creating the SQL tables
# Creating the dim_transaction table 
dbExecute(con, "
  CREATE TABLE dim_transaction_attributes (
    TransactionAttrKey INT PRIMARY KEY,
    PaymentMethod VARCHAR(30),
    DiscountFlag VARCHAR(20),
    Season VARCHAR(10)
  );
")

# Creating the bridge table 
dbExecute(con, "
  CREATE TABLE customer_product_bridge (
    BridgeKey INT PRIMARY KEY,
    CustomerKey INT,
    ProductKey INT,
    TimesPurchased INT,
    FirstPurchaseDate DATE,
    LastPurchaseDate DATE,
    FOREIGN KEY (CustomerKey) REFERENCES dim_customer(CustomerKey),
    FOREIGN KEY (ProductKey) REFERENCES dim_product(ProductKey)
  );
")

# Creating the dim_date table 
dbExecute(con, " 

CREATE TABLE DIM_DATE ( 

    DateKey INTEGER PRIMARY KEY, 

    Date TEXT, 

    Day INTEGER, 

    Month INTEGER, 

    MonthName TEXT, 

    Quarter INTEGER, 

    Year INTEGER, 

    DayOfWeek TEXT, 

    Weekend TEXT 

);") 



# Creating the dim_customer table 
dbExecute(con, " 

CREATE TABLE DIM_CUSTOMER ( 

    CustomerKey INTEGER PRIMARY KEY, 

    CustomerID TEXT, 

    CustomerName TEXT, 

    Email TEXT, 

    City TEXT, 

    State TEXT, 

    Region TEXT, 

    Segment TEXT 

);") 



# Creating the dim_product table 
dbExecute(con, " 

CREATE TABLE DIM_PRODUCT ( 

    ProductKey INTEGER PRIMARY KEY, 

    ProductID TEXT, 

    ProductName TEXT, 

    Category TEXT, 

    Subcategory TEXT, 

    Brand TEXT 

);") 



# Creating the dim_store table 
dbExecute(con, " 

CREATE TABLE DIM_STORE ( 

    StoreKey INTEGER PRIMARY KEY, 

    StoreID TEXT, 

    StoreName TEXT, 

    StoreCity TEXT, 

    StoreState TEXT, 

    StoreRegion TEXT 

);") 



# Creating the fact table 
dbExecute(con, " 

CREATE TABLE FACT_SALES (
    SalesKey INTEGER PRIMARY KEY,
    TransactionID VARCHAR(20),
    DateKey INTEGER,
    CustomerKey INTEGER,
    ProductKey INTEGER,
    StoreKey INTEGER,
    TransactionAttrKey INTEGER,
    Quantity INTEGER,
    UnitPrice DECIMAL(10,2),
    Discount DECIMAL(4,2),
    Revenue DECIMAL(10,2),
    Cost DECIMAL(10,2),
    TotalCost DECIMAL(10,2),
    ProfitMargin DECIMAL(6,4),
    FOREIGN KEY (DateKey) REFERENCES dim_date(DateKey),
    FOREIGN KEY (CustomerKey) REFERENCES dim_customer(CustomerKey),
    FOREIGN KEY (ProductKey) REFERENCES dim_product(ProductKey),
    FOREIGN KEY (StoreKey) REFERENCES dim_store(StoreKey),
    FOREIGN KEY (TransactionAttrKey) REFERENCES dim_transaction_attributes(TransactionAttrKey)
  );") 

# Inserting the R Data Frames into the SQLite Tables 
dbWriteTable(con, "DIM_DATE", dim_date, append = TRUE, row.names = FALSE) 
dbWriteTable(con, "DIM_CUSTOMER", dim_customer, append = TRUE, row.names = FALSE) 
dbWriteTable(con, "DIM_PRODUCT", dim_product, append = TRUE, row.names = FALSE) 
dbWriteTable(con, "DIM_STORE", dim_store, append = TRUE, row.names = FALSE) 
dbWriteTable(con, "FACT_SALES", fact_sales, append = TRUE, row.names = FALSE)
dbWriteTable(con, "dim_transaction_attributes", dim_transaction_attributes, append = TRUE)
dbWriteTable(con, "customer_product_bridge",
             customer_product_bridge %>%
               mutate(FirstPurchaseDate = as.character(FirstPurchaseDate),
                      LastPurchaseDate  = as.character(LastPurchaseDate)),
             append = TRUE)


# Verifying data was inserted successfully 
query_output <- dbGetQuery(con, "SELECT * FROM FACT_SALES LIMIT 5;") 

print(query_output) 

# Disconnect from database when done 

dbDisconnect(con)
