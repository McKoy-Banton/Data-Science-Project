# Clear work space and set options
rm(list = ls())
options(scipen = 9999)
library(dplyr)

# Load the sales data set
sale<-read.csv("sales_data01.csv")

# Review data
View(sale)
nrow(sale) ## get the number of rows in data frame
str(sale) ## review structure of data frame

## summary for each feature
summary(sale)
summary(sale$X.1)
summary(sale$Region)
summary(sale$Country)
summary(sale$X)
summary(sale$Sales.Channel)
summary(sale$Order.Priority)
summary(sale$Order.Date)
summary(sale$Order.ID)
summary(sale$Ship.Date)
summary(sale$Units.Sold)
summary(sale$Unit.Price)
summary(sale$Unit.Cost)
summary(sale$Total.Revenue)
summary(sale$Total.Cost)
summary(sale$Total.Profit)


#Convert the Order.Date and Ship.Date to Date data type
sale$Order.Date <- as.Date(sale$Order.Date, format = "%m/%d/%Y")
sale$Ship.Date <- as.Date(sale$Ship.Date, format = "%m/%d/%Y")
str(sale$Order.Date)
str(sale$Ship.Date)

#Convert Order.ID, Units.Sold to int
sale$Order.ID<- as.integer(sale$Order.ID)
sale$Units.Sold<-as.integer(sale$Units.Sold)
str(sale$Order.ID)
str(sale$Units.Sold)

#Convert Unit.Price, Total.Revenue, Total.Cost, Total.Profit to float
sale$Unit.Price<-as.numeric(sale$Unit.Price)
str(sale$Unit.Price)

sale$Unit.Cost<-as.numeric(sale$Unit.Cost)
str(sale$Unit.Cost)

sale$Total.Revenue<-as.numeric(sale$Total.Revenue)
str(sale$Total.Revenue)

sale$Total.Cost<-as.numeric(sale$Total.Cost)
str(sale$Total.Cost)

sale$Total.Profit<-as.numeric(sale$Total.Profit)
str(sale$Total.Profit)

#Checking for empty fields
apply(sale, 2, function(c) sum(is.na(c)))

# Check for missing values by rows
apply(sale, 1, function(c) sum(is.na(c)))

#Calculate Total.Profit if its value is missing
sale <- sale %>%
  mutate(
    Total.Profit = case_when(
      is.na(Total.Profit) | Total.Profit == "" ~ Total.Revenue - Total.Cost,
      TRUE ~ Total.Profit  # Keep existing value if not missing
    )
  )
#Calculate Total.Cost if its value is missing
sale <- sale %>%
  mutate(
    Total.Cost = case_when(
      is.na(Total.Cost) | Total.Cost == "" ~ Unit.Cost * Units.Sold,
      TRUE ~ Total.Cost  # Keep existing value if not missing
    )
  )

#Calculate Total.Revenue if its value is missing
library(dplyr)
sale <- sale %>%
  mutate(
    Total.Revenue = case_when(
      is.na(Total.Revenue) | Total.Revenue == "" ~ Unit.Price * Units.Sold,
      TRUE ~ Total.Revenue  # Keep existing value if not missing
    )
  )
#Removing rows with No data OR only 'k'
sale_clean<- sale[complete.cases(sale[, c("Region", "Country", "X", "Sales.Channel", "Order.Priority", "Order.Date",
                                          "Order.ID", "Ship.Date", "Units.Sold", "Unit.Price", "Unit.Cost", 
                                          "Total.Revenue", "Total.Cost", "Total.Profit")]), ]
sale_clean<- sale_clean[!apply(sale_clean, 1, function(row) all(row == "k")), ]

#Adding the Region for all countries
region_country <- c(
  "Sudan" = "Sub-Saharan Africa",
  "Kenya" = "Sub-Saharan Africa",
  "Nigeria" = "Sub-Saharan Africa",
  "Ethiopia" = "Sub-Saharan Africa",
  "Saint Kitts and Nevis" = "Sub-Central America and the Caribbean",
  "Ethiopia" = "Sub-Saharan Africa",
  "Denmark" = "Europe",
  "Benin" = "Sub-Saharan Africa",
  "The Gambia" = "Sub-Saharan Africa",
  "Dominican Republic" = "Central America and the Caribbean",
  "The Bahamas" = "Central America and the Caribbean",
  "South Sudan" = "Sub-Saharan Africa",
  "Mauritius" = "Sub-Saharan Africa",
  "Vietnam" = "Asia",
  "Bangladesh" = "Asia",
  "Kosovo" = "Europe",
  "Haiti" = "Central America and the Caribbean",
  "Serbia" = "Europe",
  "North Korea" = "Asia",
  "France" = "Europe",
  "India" = "Asia",
  "Monaco" = "Europe",
  "Saudi Arabia" = "Middle East and North Africa",
  "Mexico" = "North America",
  "Brunei" = "Asia",
  "Djibouti" = "Sub-Saharan Africa",
  "Saint Vincent and the Grenadines" = "Central America and the Caribbean",
  "Seychelles" = "Sub-Saharan Africa",
  "Malaysia" = "Asia",
  "Fiji" = "Australia and Oceania",
  "Sao Tome and Principe" = "Sub-Saharan Africa",
  "Costa Rica" = "Central America and the Caribbean",
  "Botswana" = "Sub-Saharan Africa",
  "Nepal" = "Asia",
  "Philippines" = "Asia",
  "Lithuania" = "Europe",
  "Georgia" = "Europe",
  "Switzerland" = "Europe",
  "Iran" = "Middle East and North Africa",
  "Myanmar" = "Asia",
  "Lesotho" = "Sub-Saharan Africa",
  "Mozambique" = "Sub-Saharan Africa",
  "Brunei" = "Asia",
  "Netherlands" = "Europe",
  "Luxembourg" = "Europe",
  "Cyprus" = "Europe",
  "Taiwan" = "Asia",
  "Nicaragua" = "Central America and the Caribbean",
  "Senegal" = "Sub-Saharan Africa",
  "Burkina Faso" = "Sub-Saharan Africa",
  "Singapore" = "Asia",
  "Cameroon" = "Sub-Saharan Africa",
  "Barbados" = "Central America and the Caribbean",
  "Qatar" = "Middle East and North Africa",
  "San Marino" = "Europe",
  "Norway" = "Europe",
  "Iceland" = "Europe",
  "South Korea" = "Asia",
  "Panama" = "Central America and the Caribbean",
  "Israel" = "Middle East and North Africa",
  "Vanuatu" = "Australia and Oceania",
  "Uganda" = "Sub-Saharan Africa",
  "Greece" = "Europe",
  "Oman" = "Europe",
  "South Africa" = "Sub-Saharan Africa",
  "Uzbekistan" = "Asia",
  "Afghanistan" = "Middle East and North Africa",
  "Guatemala" = "Central America and the Caribbean",
  "Tanzania" = "Sub-Saharan Africa",
  "Kazakhstan" = "Asia",
  "Dominica" = "Central America and the Caribbean",
  "Morocco" = "Middle East and North Africa",
  "Seychelles" = "Sub-Saharan Africa",
  "Central African Republic" = "Sub-Saharan Africa",
  "Bahrain" = "Middle East and North Africa",
  "Madagascar" = "Sub-Saharan Africa",
  "Liberia" = "Sub-Saharan Africa",
  "Malawi" = "Sub-Saharan Africa",
  "Maldives" = "Asia",
  "Somalia" = "Middle East and North Africa",
  "Montenegro" = "Europe",
  "Czech Republic" = "Europe",
  "Turkey" = "Middle East and North Africa",
  "Slovakia" = "Europe",
  "Mauritania" = "Sub-Saharan Africa",
  "Australia" = "Australia and Oceania",
  "New Zealand" = "Australia and Oceania",
  "United States of America" = "North America",
  "Samoa" = "Australia and Oceania",
  "Rwanda" = "Sub-Saharan Africa",
  "Bhutan" = "Asia",
  "Canada" = "North America",
  "Papua New Guinea" = "Australia and Oceania",
  "Japan" = "Asia",
  "Kyrgyzstan" = "Asia",
  "Sweden" = "Europe",
  "Albania" = "Europe",
  "East Timor" = "Australia and Oceania",
  "Russia" = "Europe",
  "Algeria" = "Middle East and North Africa",
  "Cape Verde" = "Sub-Saharan Africa",
  "Libya" = "Middle East and North Africa",
  "Zimbabwe" = "Sub-Saharan Africa",
  "El Salvador" = "Central America and the Caribbean"
  
)
sale_clean$Region <- gsub('"',"?", sale_clean$Region)
sale_clean$Region <- ifelse(sale_clean$Region %in% c("","?", "??" ), region_country[sale_clean$Country], sale_clean$Region)
# Clean the sales.channel
library(dplyr)
sale_clean <- sale_clean %>% 
  filter(Sales.Channel != "YES")
View(sale_clean)

#Rename the "X" column to "Item.Category"
library(dplyr)
sale_clean <- sale_clean %>% 
  rename(Item.Category = X)

#Drop X.1 has there is no use for it
sale_clean <- sale_clean[, -which(names(sale_clean) == "X.1")]

View(sale_clean)
write.csv(sale_clean, "sale_clean1.csv")

#Checking for outliers
# Units.Sold
hist(sale_clean$Units.Sold, main="Histogram of Units Sold", xlab="Values", col="red", breaks=20)
outlier.Units.Sold <- boxplot.stats(sale_clean$Units.Sold)$out
cat("Number of outliers:", length(outlier.Units.Sold), "\n")

# Unit.Price
hist(sale_clean$Unit.Price, main="Histogram of Unit Price", xlab="Values", col="pink", breaks=20)
outlier.Unit.Price <- boxplot.stats(sale_clean$Unit.Price)$out
cat("Number of outliers:", length(outlier.Unit.Price), "\n")

# Unit.Cost
hist(sale_clean$Unit.Cost, main="Histogram of Unit Cost", xlab="Values", col="blue", breaks=20)
outlier.Unit.Cost <- boxplot.stats(sale_clean$Unit.Cost)$out
cat("Number of outliers:", length(outlier.Unit.Cost), "\n")

# Total.Revenue
hist(sale_clean$Total.Revenue, main="Histogram of Total Revenue", xlab="Values", col="orange", breaks=20)
outlier.Total.Revenue <- boxplot.stats(sale_clean$Total.Revenue)$out
cat("Number of outliers:", length(outlier.Total.Revenue), "\n")
#---resolve by equal width binning
n_bins <- 20
minval <- min(sale_clean$Total.Revenue, na.rm = TRUE)
maxval <- max(sale_clean$Total.Revenue, na.rm = TRUE)
width <- (maxval - minval) / n_bins


pretty_breaks <- seq(floor(minval), ceiling(maxval), length.out = n_bins + 1)

pretty_breaks <- pretty(sale_clean$Total.Revenue, n = n_bins)
pretty_breaks[1] <- minval  # Ensure min is covered
pretty_breaks[length(pretty_breaks)] <- maxval  # Ensure max is covered

sale_clean$Total.RevenueCat <- cut(sale_clean$Total.Revenue,
                                breaks = pretty_breaks,
                                include.lowest = TRUE,
                                dig.lab = 5,
                                labels = paste0("$", format(pretty_breaks[-length(pretty_breaks)], big.mark=",", trim=TRUE),
                                                " - $", format(pretty_breaks[-1], big.mark=",", trim=TRUE)))


# Total.Cost
hist(sale_clean$Total.Cost, main="Histogram of Total Cost", xlab="Values", col="green", breaks=20)
outlier.Total.Cost <- boxplot.stats(sale_clean$Total.Cost)$out
cat("Number of outliers:", length(outlier.Total.Cost), "\n")
#---resolve by equal width binning
n_bins <- 20
minval <- min(sale_clean$Total.Cost, na.rm = TRUE)
maxval <- max(sale_clean$Total.Cost, na.rm = TRUE)
width <- (maxval - minval) / n_bins


pretty_breaks <- seq(floor(minval), ceiling(maxval), length.out = n_bins + 1)

pretty_breaks <- pretty(sale_clean$Total.Cost, n = n_bins)
pretty_breaks[1] <- minval  # Ensure min is covered
pretty_breaks[length(pretty_breaks)] <- maxval  # Ensure max is covered

sale_clean$Total.CostCat <- cut(sale_clean$Total.Cost,
                                breaks = pretty_breaks,
                                include.lowest = TRUE,
                                dig.lab = 5,
                                labels = paste0("$", format(pretty_breaks[-length(pretty_breaks)], big.mark=",", trim=TRUE),
                                                " - $", format(pretty_breaks[-1], big.mark=",", trim=TRUE)))




# Total.Profit
hist(sale_clean$Total.Profit, main="Histogram Total Profit", xlab="Values", col="purple", breaks=20)
outlier.Total.Profit <- boxplot.stats(sale_clean$Total.Profit)$out
cat("Number of outliers:", length(outlier.Total.Profit), "\n")
#---resolve by equal width binning
n_bins <- 20
minval <- min(sale_clean$Total.Profit, na.rm = TRUE)
maxval <- max(sale_clean$Total.Profit, na.rm = TRUE)
width <- (maxval - minval) / n_bins


pretty_breaks <- seq(floor(minval), ceiling(maxval), length.out = n_bins + 1)

pretty_breaks <- pretty(sale_clean$Total.Profit, n = n_bins)
pretty_breaks[1] <- minval  # Ensure min is covered
pretty_breaks[length(pretty_breaks)] <- maxval  # Ensure max is covered

sale_clean$Total.ProfitCat <- cut(sale_clean$Total.Profit,
                                breaks = pretty_breaks,
                                include.lowest = TRUE,
                                dig.lab = 5,
                                labels = paste0("$", format(pretty_breaks[-length(pretty_breaks)], big.mark=",", trim=TRUE),
                                                " - $", format(pretty_breaks[-1], big.mark=",", trim=TRUE)))
View(sale_clean)

