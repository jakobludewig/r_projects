# code to get the data and replicate results for the restaurant review comparison
# blog post: https://natural-blogarithm.com/post/restaurant-reviews-stockholm-vs-berlin/
library(magrittr)
library(httr)
library(glue)
library(tidyverse)
library(mapsapi)
library(leaflet)
library(sp)
library(sf)
library(xml2)
library(jsonlite)
library(utils)
library(osmdata)
library(parallel)
library(mapview)
library(entropy)

theme_set(theme_light())

# number of cores to use for parallel querying of APIs
num_cores <- 
   detectCores()

plots <- list()

##### WARNING: This script will query Google APIs which will incur costs!  #####
#####          Check API pricing before running the script! Run at your    #####
#####          risk!                                                       #####

# to set up access to Google Maps API:
# 1. go to and create a project and get API key: https://developers.google.com/maps/documentation/javascript/get-api-key
# 2. activate Maps API services on this page: https://console.cloud.google.com/apis/library?filter=category:maps
# 3. set environment variable in ~/.Renviron file to the key's value
api_key <- Sys.getenv("GCP_API_KEY")

locations <- 
   tribble(~city_name,~country_code,~language,
           "Stockholm","se","sv",
           "Berlin","de","de")

locations %<>%
   rowwise() %>%
   mutate(geocode_responses = mp_geocode(addresses = city_name,
                                         region = country_code,
                                         key = api_key,
                                         quiet = T)) %>%
   ungroup()

# sample coordinates within cities ----
# try with simple bounding box
set.seed(123)

# number of API calls and therefore costs will scale with variable set below. 
# results in the blog post were obtained with a setting of 750
num_coordinate_samples <- 
   50
locations %<>%
   mutate(bb = purrr::map(geocode_responses,.f = ~ mp_get_bounds(list(.)))) %>%
   mutate(coordinate_samples_bb = purrr::map(bb,
                                             .f = ~ coordinates(spsample(sf:::as_Spatial(.),
                                                                         n=num_coordinate_samples,
                                                                         type = "random"))))

for (i in 1:nrow(locations)){
   plots[[paste0("location_samples_bb_",
                 locations[i,]$city_name)]] <-
      leaflet() %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>%  
      addPolygons(data= locations$bb[[i]])  %>%
      addMarkers(lng = ~x,lat = ~y,
                 data = locations$coordinate_samples_bb[[i]] %>% as.data.frame)
}
plots[1:2]
# not good enough using the bounding box

# try polynomial boundaries
locations$bp <-
   lapply(paste(locations$city_name,
                locations$country_code, 
                sep =" ,"),
          function(x) {
             out <- getbb(x,format_out = "polygon")
             if(any(class(out) == "list")) {
                # for some results multiple polygons are returned, in these 
                # cases we assume the first one is the most useful one
                return(out[[1]])
             } else {
                return(out)
             }
          })

locations %<>%
   mutate(coordinate_samples_bp = purrr::map(bp,
                                             .f = ~ coordinates(spsample(Polygon(.),
                                                                         n = num_coordinate_samples,
                                                                         type = "random"))))

for (i in 1:nrow(locations)){
   plots[[paste0("location_samples_bp_",
                 locations[i,]$city_name)]] <-
      leaflet() %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>%  
      addPolygons(data= locations$bp[[i]])  %>%
      addMarkers(lng = ~x,lat = ~y,
                 data = coordinates(locations[i,]$coordinate_samples_bp) %>% as.data.frame)
}
plots[3:4]
# looking better

# two steps:
# 1. get place ids from nearbysearch endpoint
# 2. get details (incl reviews) from details endpoint

# query nearby API ----
results_nearby <-
   locations %>% 
   select(city_name:language,
          coordinates = coordinate_samples_bp) %>%
   mutate(coordinates = purrr::map(coordinates,.f = as_tibble)) %>% 
   unnest(cols = c(coordinates))

# API specs: https://developers.google.com/places/web-service/search#FindPlaceRequests
query_nearby_api <- 
   function(x,y,api_key) {
      list(fromJSON(paste0("https://maps.googleapis.com/maps/api/place/nearbysearch/json?location=",y,",",x,"&rankby=distance&type=restaurant","&key=",api_key)))
   }

results_nearby$nearby_response <-
   mcmapply(FUN = query_nearby_api,
            results_nearby$x,results_nearby$y, api_key,
            mc.cores = num_cores)

# extract place_ids
places <-
   results_nearby %>%
   select(city_name,country_code,language,nearby_response) %>%
   mutate(nearby_response = purrr::map(nearby_response,
                                       .f = ~ .$results %>% select(name,place_id,rating,types))) %>%
   unnest(nearby_response)

# filter out places without rating and duplicated place_ids
places_nrow_before <-
   nrow(places)
num_places_missing_rating <- 
   places %>% filter(is.na(rating) | (rating == "")) %>% nrow
num_places_duplicate_ids <-
   places %>% count(place_id,sort = T) %>% mutate(n = n-1) %>% 
   summarise(sum(n)) %>% unlist %>% as.vector

places <-
   places %>% 
   filter(!is.na(rating) & (rating != "")) %>% 
   group_by(place_id) %>% 
   filter(row_number()== 1) %>% 
   ungroup()
message(glue("Removed {num_places_missing_rating} places with missing rating, ",
             "removed {num_places_duplicate_ids} duplicate place_id's, ",
             "in total removed {places_nrow_before - nrow(places)} out of {places_nrow_before} observations"
))

# query details API ----

# we are querying in local language as querying directly in English seems to 
# give slightly different results, local language will hopefully give us more
# local reviewers
query_details_api <- 
   function(place_id,language,api_key) {
      list(fromJSON(URLencode(paste0("https://maps.googleapis.com/maps/api/place/details/json?place_id=",
                                     place_id,"&language=",language,"&key=",
                                     api_key))))
   }

places_details <-
   places
places_details$details_response <-
   mcmapply(FUN = query_details_api,
            places_details$place_id,places_details$language, api_key,
            mc.cores = num_cores)

reviews <-
   places_details %>%
   select(city_name:rating,details_response) %>%
   rename(country_language = language,
          place_rating = rating) %>%
   mutate(review = purrr::map(details_response,.f = ~ .$result$reviews)) %>%
   unnest(review)

# query translate API ----
# translate all reviews to English
message(glue("Removing {nrow(reviews %>% filter(is.na(text) | (text == '')))} ",
             "out of {nrow(reviews)} observations with missing review texts"))
reviews_translated <- 
   reviews %>% 
   select(-details_response) %>%
   filter(!is.na(text),text != "") %>%
   mutate(text = str_replace_all(text, pattern = c('"'=""))) %>%
   mutate(translate_body = paste0('{"q": ["',text,'"],
                                  "target": "en",
                                  "source": "',country_language,'",
                                  "format": "text"}'))
query_translate_api <- 
   function(body,api_key) {
      translate_request_url <-
         paste0("https://translation.googleapis.com/language/translate/v2?key=",
                api_key)
      fromJSON(rawToChar(httr::POST(translate_request_url, body = body)$content))
   }

reviews_translated$translate_response <-
   mcmapply(FUN = query_translate_api,
            reviews_translated$translate_body, api_key,
            mc.cores = num_cores)

reviews_translated %<>%
   mutate(text_translated = unlist(unname(purrr::map(translate_response,
                                                     .f = ~ unname(unlist(.$translations))))))
# analysis ----
nrow(reviews_translated)
reviews_translated %>% count(city_name)

rating_dist <-
   reviews_translated %>% 
   mutate(rating = as.character(rating)) %>% 
   count(city_name,rating) %>% 
   group_by(city_name) %>%
   mutate(perc = n/sum(n)) %>%
   ungroup()

rating_dist %>% 
   ggplot(aes(x = rating, 
              y = perc, 
              fill = city_name)) + 
   geom_col(position = position_dodge()) +
   xlab("Rating") +
   ylab("Percentage") +
   scale_y_continuous(labels = scales::percent) +
   scale_fill_viridis_d(name = "City")

rating_dist %>% 
   filter(rating == 5) %>% 
   select(city_name,perc) %>% 
   spread(city_name,perc) %>% 
   mutate(diff = Berlin-Stockholm)

reviews_translated %>% 
   group_by(city_name) %>% 
   summarise(mean_rating = mean(rating),
             .groups = "drop")

bind_rows(tibble(
   city_name = c("Berlin (extreme)"),
   rating = as.character(1:5),
   perc = c(0.1, 0, 0, 0, 0.9)) %>%
   bind_rows(tibble(
      city_name = c("Stockholm (extreme)"),
      rating = as.character(1:5),
      perc = rep(0.2, 5)
   ))) %>% 
   ggplot(aes(x = rating,
              y = perc,
              fill = city_name)) +
   geom_col(position = position_dodge()) +
   xlab("Rating") +
   ylab("Percentage") +
   scale_y_continuous(labels = scales::percent) +
   scale_fill_viridis_d(name = "City")


rating_dist %>% 
   group_by(city_name) %>% 
   summarise(entropy = entropy(n),.groups = "drop")
