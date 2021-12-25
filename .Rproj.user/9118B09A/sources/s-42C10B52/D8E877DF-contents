library(tidyverse)
library(magrittr)
library(purrr)
library(gridExtra)
library(ggpubr)
library(lubridate)
library(viridis)
library(maps)
library(VGAM)
library(vegan)
library(forecast)
library(xts)
library(GGally)
# For text tokenization
library(caret)
library(textclean)
library(wordcloud)
library(tm)
library(cluster)
library(klaR)
library(ggfortify)
library(factoextra)
options(warn = -1) # remove warning messages

commerce_bc <- read.csv('data.csv')

# EDA ---------------------------------------------------------------------
# Missing values 
pMiss <- function(x){sum(is.na(x))/length(x)*100} #function to calculate missing values
apply(commerce_bc, 2, pMiss) # almost 25% of missing values in CustomerID column


# Cleanup -----------------------------------------------------------------
# Remove fees and adjustments 
commerce <- commerce_bc %>%
  filter(!grepl('amazon', Description, ignore.case = T) & !grepl('adjust', Description, ignore.case = T)) %>%
  filter(!is.na(CustomerID)) %>% #remove entries without customer encoding
  filter(!grepl('C', StockCode)) %>% #remove cancelled orders
  distinct() #remove duplicates (around 150.000 rows removed from original dataset)

# Retype columns 
commerce$Country <- factor(commerce$Country)
commerce$InvoiceDate <- as.POSIXct(commerce$InvoiceDate, tryFormats = '%m/%d/%Y %H:%M')

# Change factor labels 
commerce$StockCode <- fct_recode(commerce$StockCode, "Postage" = "POST", "Discount" = "D",
                                 "Carriage" = "C2", "Manual" = "M", "Bank Charges" = "BANK CHARGES",
                                 "Doctom Postage" = "DOT")

# Time related variables
commerce$date <- as.Date(commerce$InvoiceDate)
commerce$year <- format(commerce$date, '%Y')
commerce$month <- factor(format(commerce$date, '%b'), ordered = T, 
                         levels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep',
                                    'Oct', 'Nov', 'Dec'))
commerce$quarter <- case_when(
  commerce$month %in% c('Jan', 'Feb', 'Mar') ~ 'Q1',
  commerce$month %in% c('Apr', 'May', 'Jun') ~ 'Q2',
  commerce$month %in% c('Jul', 'Aug', 'Sep') ~ 'Q3',
  commerce$month %in% c('Oct', 'Nov', 'Dec') ~ 'Q4',
  TRUE ~ 'Unknown'
)

commerce$quarter <- factor(commerce$quarter, ordered = T, levels =c('Q1', 'Q2', 'Q3', 'Q4'))
commerce$weeknd <- ifelse(weekdays(commerce$date) %in% c('Saturday', 'Sunday'), 'weekend', 'weekday')

commerce %<>% filter(!StockCode %in% c('Postage', 'Discount', 'Carriage', 'Manual', 'Bank Charges', 'Doctom Postage'))


# Total amount by invoice
comm_clean <- commerce %>% 
  group_by(CustomerID, Country, date, year, month, quarter, weeknd) %>% 
  summarise(InvoiceAmount = sum(Quantity * UnitPrice)) %>%
  filter(InvoiceAmount > 0) # drop article devolutions

# Outlier extraction (z-score formula)
x_median <- median(comm_clean$InvoiceAmount)
x_sd <- sd(comm_clean$InvoiceAmount, na.rm = T)
comm_clean %<>%
  filter(!(abs(InvoiceAmount - x_median) / x_sd) > 3) 

y_median <- median(commerce$UnitPrice, na.rm = T) 
y_sd <- sd(commerce$UnitPrice, na.rm = T)
commerce %<>%
  filter(!(abs(UnitPrice - y_median) / y_sd) > 3)


# Remove: stopwords in ENG
eng_stop = stopwords("eng")


df<- commerce
set.seed(20)
df<-df[sample(nrow(df)),]

corpus<-Corpus(VectorSource(df$Description))
corpus.clean<-corpus%>%
  tm_map(content_transformer(tolower))%>%
  tm_map(removePunctuation)%>%
  tm_map(removeNumbers)%>%
  tm_map(removeWords,stopwords(kind="en"))%>%
  tm_map(stripWhitespace)

comm_rfm <- commerce %>% 
  filter(!is.na(CustomerID) & UnitPrice > 0 & Quantity > 0) %>%
  group_by(InvoiceNo, CustomerID, date, Country) %>%
  summarise(Quantity = sum(Quantity, na.rm = T),
            InvoiceAmount = sum(Quantity * UnitPrice)) %>%
  ungroup() %>%
  group_by(CustomerID) %>%
  mutate(CustomerID = as.factor(CustomerID)) %>%
  summarise(Recency = as.numeric(as.Date('2011-12-31') - max(date)),
            Frequency = n_distinct(InvoiceNo),
            Monetary = sum(InvoiceAmount, na.rm = T)/Frequency)



desc <- unique(commerce$Description) 

desc <- desc[nchar(desc) > 1]
desc2 <- tibble(text = desc) %>% 
  mutate(tokens = str_split(text, '\\s+')) %>% 
  unnest(tokens)

for(unique_value in unique(desc2$tokens)){
  desc2[paste(unique_value)] <- ifelse(desc2$tokens == unique_value, 1, 0)
}

desc_colnames <- colnames(desc2)
desc_colnames <- desc_colnames[nchar(desc_colnames) > 2]
desc2 <- desc2[,desc_colnames]
desc2 <- desc2[,!names(desc2) == 'tokens']

desc_agg <- aggregate(desc2[,-1], by = list(desc2$text), FUN = sum)
# Remove columns with less than 10 entries 
desc_agg2 <- desc_agg[,colSums(desc_agg[,-1]) > 10] # from 2239 to 333 columns
colnames(desc_agg2)[1] <- 'Description'
# add price unit 
commerce_coll <- commerce %>% 
  group_by(Description) %>% 
  filter(UnitPrice > 0) %>% 
  summarise(UnitPrice = mean(UnitPrice, na.rm = T))

# UnitPrice by categories (should be encoded to run kmeans properly)
#desc_agg2 <- desc_agg2 %>% 
#  left_join(commerce_coll, by = c('Description')) %>% 
#  filter(!is.na(UnitPrice)) %>%
#  mutate(UnitPrice = (UnitPrice - min(UnitPrice)) /(max(UnitPrice) - min(UnitPrice)))

min_com = min(commerce$UnitPrice)
max_com = max(commerce$UnitPrice)

commerce_coll <- commerce %>% 
  filter(UnitPrice > 0) %>% 
  mutate(UnitPrice = ((UnitPrice - min_com) /(max_com - min_com))) %>% 
  group_by(Description) %>% 
  summarise(UnitPrice = mean(UnitPrice, na.rm = T))

desc_agg3 <- desc_agg2 %>% 
  left_join(commerce_coll, by = c('Description')) %>% 
  filter(!is.na(UnitPrice))


desc_agg3 %<>% replace(is.na(.), 0) 
rownames(desc_agg3) <- desc_agg3[,1]
desc_def <- desc_agg3[,-1]
desc_def <- desc_def[,colSums(desc_def) >= 30]

row.names(comm_rfm) <- comm_rfm$CustomerID 
comm_rfm$CustomerID <- NULL
comm_norm <- scale(comm_rfm) # normalize

