# Initial ETL Process

# Loading the necessary libraries
library(readr)
library(dplyr)
library(DBI)
library(RSQLite)
library(stringr)
library(lubridate)

# Loading the dataset
raw_data <- read_csv("retail_sales_raw.csv", show_col_types = TRUE)

# Inspecting the loaded file
glimpse(raw_data)
summary(raw_data)

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
    Segment       = str_to_title(Segment),
    PaymentMethod = str_to_title(PaymentMethod)
  ) %>%

# Filtering unnecessary columns
select(-Notes, -InternalFlag)

# Inspecting the cleaned file
glimpse(clean_data)
colSums(is.na(clean_data))


# Building the Dimension Tables Based on 
# The Designed Star Schema

# Creating an in-memory SQLite database
con <- dbConnect(RSQLite::SQLite(), ":memory:")

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
    DateKey INTEGER,
    CustomerKey INTEGER,
    ProductKey INTEGER,
    StoreKey INTEGER,
    TransactionID TEXT,
    Quantity INTEGER,
    UnitPrice REAL,
    Discount REAL,
    Revenue REAL,
    PaymentMethod TEXT,
    FOREIGN KEY (DateKey) REFERENCES DIM_DATE(DateKey),
    FOREIGN KEY (CustomerKey) REFERENCES DIM_CUSTOMER(CustomerKey),
    FOREIGN KEY (ProductKey) REFERENCES DIM_PRODUCT(ProductKey),
    FOREIGN KEY (StoreKey) REFERENCES DIM_STORE(StoreKey)
);")

# Preparing dimension data frames with surrogate keys
# dim_date
dim_date_df <- clean_data %>%
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
dim_customer_df <- clean_data %>%
  distinct(CustomerID, CustomerName, Email, City, State, Region, Segment) %>%
  mutate(CustomerKey = row_number())

#dim_ product
dim_product_df <- clean_data %>%
  distinct(ProductID, ProductName, Category, Subcategory, Brand) %>%
  mutate(ProductKey = row_number())

# dim_store
dim_store_df <- clean_data %>%
  distinct(StoreID, StoreName, StoreCity, StoreState, StoreRegion) %>%
  mutate(StoreKey = row_number())

#fact_sales
fact_sales_df <- clean_data %>%
  mutate(Date = as.Date(TransactionDate)) %>%
  left_join(dim_date_df, by = "Date") %>%
  left_join(dim_customer_df, by = c("CustomerID", "CustomerName", "Email", "City", "State", "Region", "Segment")) %>%
  left_join(dim_product_df, by = c("ProductID", "ProductName", "Category", "Subcategory", "Brand")) %>%
  left_join(dim_store_df, by = c("StoreID", "StoreName", "StoreCity", "StoreState", "StoreRegion")) %>%
  mutate(
    SalesKey = row_number(),
    Revenue = round(Quantity * UnitPrice * (1 - Discount), 2)
  ) %>%
  select(
    SalesKey, DateKey, CustomerKey, ProductKey, StoreKey,
    TransactionID, Quantity, UnitPrice, Discount, Revenue, PaymentMethod
  )

# Inserting the R Data Frames into the SQLite Tables
dbWriteTable(con, "DIM_DATE", dim_date_df, append = TRUE, row.names = FALSE)
dbWriteTable(con, "DIM_CUSTOMER", dim_customer_df, append = TRUE, row.names = FALSE)
dbWriteTable(con, "DIM_PRODUCT", dim_product_df, append = TRUE, row.names = FALSE)
dbWriteTable(con, "DIM_STORE", dim_store_df, append = TRUE, row.names = FALSE)
dbWriteTable(con, "FACT_SALES", fact_sales_df, append = TRUE, row.names = FALSE)

# Verifying data was inserted successfully
query_output <- dbGetQuery(con, "SELECT * FROM FACT_SALES LIMIT 5;")
print(query_output)

# Disconnect from database when done
dbDisconnect(con)
