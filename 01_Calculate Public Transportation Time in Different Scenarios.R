library(jsonlite)
library(tidyverse)
library(lubridate)
library(writexl)
require(tictoc)

coords_vaccination <- read_delim("D:/vaccination service accessibility/更新距离最近的三个疫苗接种点(周中).csv", delim = ";") %>%
  mutate(
    from_lat = as.numeric(sapply(strsplit(from, ","), function(x) x[2])),
    from_lon = as.numeric(sapply(strsplit(from, ","), function(x) x[1])),
    to_lat = as.numeric(sapply(strsplit(to, ","), function(x) x[2])),
    to_lon = as.numeric(sapply(strsplit(to, ","), function(x) x[1]))
  )

beijing_time <- with_tz(Sys.time(), tzone = "Asia/Shanghai")
beijing_date <- as.Date(beijing_time)
tod <- format(beijing_time, "%H")
day_of_week <- weekdays(beijing_date)

morning_hrs <- c(paste0(0,8:9), 10:12)
afternoon_hrs <- c(13:17)

day_type <- ifelse(day_of_week %in% c("星期六", "星期日"), "weekend", "weekday")
dri_type <- ifelse(tod %in% morning_hrs, "morning", ifelse(tod %in% afternoon_hrs, "afternoon", NA))
traffic_scenario <- paste0(day_type, "_", dri_type)
api_keys <- c("Your key")
key_count <- length(api_keys)

results <- results_batch <- list()  
batch_size <- 1000 
total_rows <- nrow(coords_vaccination)

tic()
for (i in 1:total_rows) {  
  origin <- c(coords_vaccination$from_lon[i], coords_vaccination$from_lat[i])
  destination <- c(coords_vaccination$to_lon[i], coords_vaccination$to_lat[i])
  current_key <- api_keys[((i - 1) %% key_count) + 1]
  city <- "北京"  
  tmp_url <- paste0(
    "https://restapi.amap.com/v3/direction/transit/integrated?origin=", origin[1], ",", origin[2],
    "&destination=", destination[1], ",", destination[2],
    "&city=", city,  
    "&key=", current_key
  )
  
  
  response <- tryCatch({
    fromJSON(URLdecode(tmp_url))
  }, error = function(e) {
    warning(paste("Error fetching data for row", i))
    return(NULL)
  })
  
  if (!is.null(response) && !is.null(response$route$transits)) {
    transits <- response$route$transits
    
    if (is.data.frame(transits)) {
      for (j in seq_len(nrow(transits))) {
        transit <- transits[j, ]
        
        cost <- as.numeric(transit$cost)  
        duration <- as.numeric(transit$duration)  
        distance <- as.numeric(transit$distance)  
        walking_distance <- as.numeric(transit$walking_distance)  
        
        transitdf <- data.frame(
          traffic_scenario = traffic_scenario,
          from = paste(origin[1], origin[2], sep = ","),
          to = paste(destination[1], destination[2], sep = ","),
          row_index = i,
          path_index = j, 
          cost = cost,
          duration = duration / 60,  
          distance = distance / 1000,  
          walking_distance = walking_distance / 1000  
        )
        

        results <- bind_rows(results, transitdf)
        results_batch <- append(results_batch, list(transitdf))  
      }
    } else {
      warning(paste("Unexpected structure in transits for row", i))
    }
  } else {
    warning(paste("No transit data found for row", i))
  }
  
  
  if (i %% batch_size == 0 || i == total_rows) {  
 
    results <- bind_rows(results_batch)
    

    i_out <- ifelse(i %% batch_size == 0,i,i+batch_size)
    
    file_name <- paste0("public_transportation_paths_results_weekday_morning_batch_", i_out %/% batch_size + 1, ".xlsx")
    write_xlsx(results, file_name)
    print(paste("Batch", i_out %/% batch_size + 1, "saved at", Sys.time()))
    

    results_batch <- list()
  }
}
toc()


