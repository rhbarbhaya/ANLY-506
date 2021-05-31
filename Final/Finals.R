# Read the dataset
data = readxl::read_excel('usa-2016-presidential-election-by-county.xls', )

# Type cast categorical values as factors
data$State = as.factor(data$State)
data$County = as.factor(data$County)
data$Precincts = as.factor(data$Precincts)

# Check the structure and summarize the dataset
summary(data)
str(data)

# Define function for calulating percent miss
percentMiss = function(x){
  sum(is.na(x))/length(x)*100
}

missing = apply(data, 1, percentMiss)
table(missing)

# Eliminate all the records which have missing values > 5%
cleanedDataset = subset(data, missing <= 5)

replace_col = cleanedDataset[,-c(1, 2, 3)]
dont_col = cleanedDataset[,c(1, 2, 3)]

temp_no_miss = mice::mice(replace_col)
no_miss = mice::complete(temp_no_miss, 1)

df = cbind(dont_col, no_miss)
summary(df)

# Remove Outliers
temp_df = df[, -c(1,2,3)]
mahal = mahalanobis(temp_df,
                    colMeans(temp_df, na.rm = T),
                    cov(temp_df, use = 'pairwise.complete.obs'))

cutoff = qchisq(1-.001, ncol(temp_df))
summary(mahal < cutoff)

data = subset(df, mahal < cutoff)
num_data = subset(temp_df, mahal < cutoff)

summary(data)

# Additivity
symnum(cor(num_data))

# Linearity
random = rchisq(nrow(num_data), 7)
fake = lm(random ~ ., data = num_data)
summary(fake)

Standardize = rstandard(fake)
qqnorm(Standardize)
abline(0, 1)
plot(fake, 2)

# Skrewness
skewTable = matrix(NA, nrow = 20, ncol = 3)
colnames(skewTable) = c("Column Name", "D'Agostino p-value")
for (i in seq(4,23)){
  skewTable[i-3,] = c(colnames(data[i]), moments::agostino.test(data[, i])$p.value, if(moments::agostino.test(data[, i])$p.value > 0.05){"No Skewness"}else{"Data has skew"})
}
knitr::kable(skewTable, caption = "D'Agostino skewness Test", format = 'pipe')

# Normality Test
hist(Standardize)
normTable = matrix(NA, nrow = 20, ncol = 3)
colnames(normTable) = c("Column Name", "Shapiro-Wilk p-value")
for (i in seq(4,23)){
  normTable[i-3,] = c(colnames(data[i]), shapiro.test(data[, i])$p.value, if(shapiro.test(data[, i])$p.value > 0.05){"Data is Normally distributed"}else{"Data is NOT Normally distributed"})
}
knitr::kable(normTable, caption = "Shapiro-Wilk Normality Test", format = 'pipe')

# Results Variable
# If 1, Democrats win, 
# If 0, Republicans Win
data$winner = ifelse((data$Democrats2016 - data$Republicans2016) > 0, 1, 0)
head(data, 10)
