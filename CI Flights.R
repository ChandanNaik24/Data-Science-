### Package for Confidence Interval ###

install.packages("gmodels")
library(gmodels)

### Data Set ###

install.packages('nycflights13')
library(nycflights13)

### Loading of the data package ###

data("flights")
dataframe1 <- flights

### Selection Dep Column ###

flg <- nycflights13::flights
dep_delay <- flights$dep_delay

### To check NA values ###

sum(is.na(dep_delay))
is.na(dep_delay)

### Remove NA vlaues ###

dep_delay_withoutNA <- dep_delay [!is.na(dep_delay)]

### Confidence Interval ###

ci(dep_delay_withoutNA,confidence = 0.95)

### FOr Arrival Delay ###

data("flights")
ar_delay <- flg$arr_delay

### To chech NA valure for Arr delay ###

sum(is.na(ar_delay))
(is.na(ar_delay))

### Remove NA values ###

ar_delay_withoutNA <- ar_delay[! is.na(ar_delay)]

### Confidence Interval Construction ###

ci(ar_delay_withoutNA, confidence = 0.95)

