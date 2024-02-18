summary(car_data)

car_data <- car_data

library(dplyr)

library(tidyverse)

filtered_car_data <- distinct(car_data, title, .keep_all = TRUE)

onlyID <-  distinct(car_data, url)

manipulate_string <- filtered_car_data[1,3]

filtered_car_data$post_id = str_extract(filtered_car_data$url,"\\b\\d{8}\\b")

onlyID$postID = str_extract(onlyID$url,"\\b\\d{8}\\b")

print out rows

