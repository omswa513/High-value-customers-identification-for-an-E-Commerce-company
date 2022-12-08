#High value customers identification for an E-Commerce company

# Install Necessary libraries

library(scales)
install.packages("NbClust")
library(dplyr)
library(ggplot2)

ecom_file <- Ecommerce
View(ecom_file)

str(ecom_file)

#Invoice No data type shows as character so changing it to integer

ecom_file$InvoiceNo <- as.integer(ecom_file$InvoiceNo)

#exploratory data analysis
str(ecom_file)
summary(ecom_file)
head(ecom_file)
tail(ecom_file)
dim(ecom_file)
class(ecom_file)

# Removing unnecessary column called x which have NA values

ecom_data_subset <- subset(ecom_file, select = -X)
View(ecom_data_subset)
str(ecom_data_subset)

# Verify null and missing values
length(unique(ecom_data_subset$CustomerID)) # Total 4373 unique values
sum(is.na(ecom_data_subset$CustomerID)) # Customer Id 135080


ecom_data_subset <- subset(ecom_data_subset, Country == "United Kingdom")

mean(is.na(ecom_data_subset)) # There are 0.035% missing values are there which can be ignore by consider the huge data set
View(ecom_data_subset)

pMiss <- function(x)
{
  sum(is.na(x))/length(x)*100
}

apply(ecom_data_subset,2,pMiss)

# Customer id have 26.96% missing values which is high number

# Mice package will give us better understanding to to understand for missing values

library(mice)
md.pattern(ecom_data_subset)

# unique invoices and unique customers.
length(unique(ecom_data_subset$InvoiceNo)) # 20120 unique invoices
length(unique(ecom_data_subset$CustomerID)) # 3951 Unique customer id's

# Quantity column consist negative values so removing them
pos_quant <- ecom_data_subset[ecom_data_subset$Quantity > 0,]
nrow(pos_quant) # 486286

# Date column data type shows as character so let's change it to Date
# changing date format
ecom_data_subset$InvoiceDate <- as.Date(ecom_data_subset$InvoiceDate,format = "%d-%b-%y")
str(ecom_data_subset$InvoiceDate)

str(ecom_data_subset) # Date format changed as Date now

# Add the column - expense
ecom_data_subset['expense'] = ecom_data_subset['Quantity'] * ecom_data_subset['UnitPrice']

#Data set contain various country information
#So here we’ll restrict the data to UK only.
table(ecom_data_subset$Country)

#Let's see the number of unique invoices and unique customers.
length(unique(ecom_data_subset$InvoiceNo)) # Unique Invoice No = 20120
length(unique(ecom_data_subset$CustomerID)) # Unique Customer ID = 3951

##Identify returns with Invoice details
ecom_data_subset$item.return <- grepl("C", ecom_data_subset$InvoiceNo, fixed=TRUE)
ecom_data_subset$purchase.invoice <- ifelse(ecom_data_subset$item.return=="TRUE", 0, 1)
View(ecom_data_subset)

# SO added new columns item.return and purchase.invoice to explore it more.

#K- Means Cluster

# kmeans(scaled dataset,centers=no of clusters,iter.max = max no of iterations)

library('cluster')

kmeans_model <- kmeans(ecom_data_subset,centers=3,iter.max=30)

#cluster metrics
kmeans_model






