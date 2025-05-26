library(readxl)
library(dplyr)
install.packages("openxlsx")
library(openxlsx)
# 读取 Excel 文件
data_vaccination <- read_excel("D://vaccination service accessibility/疫苗接种点开放时间.xlsx")

filtered_data <- data_vaccination %>%
  filter(grepl("周六|周日|周六日", opentime))
print(filtered_data)

location_data <- read_excel("D://vaccination service accessibility/vaccination service location result清理后版.xlsx")
print(colnames(filtered_data))
print(colnames(location_data))

matched_data <- location_data %>%
  filter(Name %in% filtered_data$name)
print(matched_data)

write.xlsx(matched_data, "D://vaccination service accessibility/周末开放的疫苗接种点地址数据.xlsx")
