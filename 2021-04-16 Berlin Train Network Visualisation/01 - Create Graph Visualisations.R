# this code reproduces the figures shown in the Berlin train network visualisation 
# post which can be found here: https://natural-blogarithm.com/post/berlin-traffic-matrix-vis/
# before executing the code make sure to download the GTFS files from the Berlin 
# Open Data website (https://daten.berlin.de/datensaetze/vbb-fahrplandaten-gtfs)
# and extract it into the folder 'data/GTFS'
library(magrittr)
library(ggraph)
library(tidygraph)
library(tidyverse)
library(readr)
library(geosphere)
library(gganimate)
theme_set(theme_minimal())

if(!dir.exists("out")) dir.create("out")
theme_update(
  legend.position = "none",
  legend.title = element_blank(),
  plot.background = element_rect(fill = "white", color = NA)
)
plots <-
  list()


# reproducing 20x20 matrix visualisation ----
# see here: https://fronkonstin.com/2016/08/22/the-breathtaking-1-matrix/
(plots[["one-matrix-20x20"]] <-
   ggraph(tbl_graph(nodes = tibble(name = 1:20),
                    edges = tibble(from = 1:20) %>% 
                      crossing(to = 1:20) %>% 
                      filter(from >= to)),
          layout = "linear",circular = T) + 
   theme(plot.background = element_rect(fill = "violetred4")) + 
   geom_node_point() + 
   geom_edge_arc(color = "white", 
                 alpha = 0.4, 
                 strength = 0.8))

# get Berlin traffic data ----
# manually picked colors from route map (https://www.bvg.de/de/Fahrinfo/Downloads/BVG-Liniennetz)
routes_colors <-
  tribble(~route_short_name,~color,
          "U1","#55b831",
          "U2","#f1471c",
          "U3","#36ab94",
          "U4","#fdd205",
          "U5","#78513c",
          "U6","#775fb0",
          "U7","#399fdf",
          "U8","#1a5ea4",
          "U9","#f67c13",
          "S1","#dd4daf",
          "S2","#108449",
          "S26","#108449",
          "S25","#108449",
          "S3","#156ab8",
          "S41","#a64c35",
          "S42","#ca5b21",
          "S45","#c48d42",
          "S46","#c48d42",
          "S47","#c48d42",
          "S5","#f46717",
          "S7","#775fb0",
          "S75","#775fb0",
          "S8","#55b831",
          "S85","#57B933",
          "S9","#94244d")
routes_colors_vec <-
  setNames(routes_colors$color, routes_colors$route_short_name)

# download GTFS data from Berlin open data and unzip into 'data/'
# https://daten.berlin.de/datensaetze/vbb-fahrplandaten-gtfs
# GTFS reference: https://developers.google.com/transit/gtfs/reference
stops <-
  read_csv("data/GTFS/stops.txt",
           col_types = 
             cols(
               stop_id = col_character(),
               stop_code = col_logical(),
               stop_name = col_character(),
               stop_desc = col_logical(),
               stop_lat = col_double(),
               stop_lon = col_double(),
               location_type = col_double(),
               parent_station = col_character(),
               wheelchair_boarding = col_double(),
               platform_code = col_character(),
               zone_id = col_character()
             ))

stop_times <-
  read_csv("data/GTFS/stop_times.txt",
           col_types = 
             cols(
               trip_id = col_double(),
               arrival_time = col_character(),
               departure_time = col_character(),
               stop_id = col_character(),
               stop_sequence = col_double(),
               pickup_type = col_double(),
               drop_off_type = col_double(),
               stop_headsign = col_character()
             ))

trips <-
  read_csv("data/GTFS/trips.txt",
           col_types = 
             cols(
               route_id = col_character(),
               service_id = col_double(),
               trip_id = col_double(),
               trip_headsign = col_character(),
               trip_short_name = col_character(),
               direction_id = col_double(),
               block_id = col_double(),
               shape_id = col_double(),
               wheelchair_accessible = col_double(),
               bikes_allowed = col_logical()
             )
  )

agency <- 
  read_csv("data/GTFS/agency.txt")

routes <- 
  read_csv("data/GTFS/routes.txt",
           col_types = 
             cols(
               route_id = col_character(),
               agency_id = col_double(),
               route_short_name = col_character(),
               route_long_name = col_character(),
               route_type = col_double(),
               route_color = col_character(),
               route_text_color = col_character(),
               route_desc = col_character()
             ))

# identify train routes (S-Bahn and U-Bahn)
# unfortunately route types in dataset do not adhere to GTFS standards so we 
# will have to filter by route_short_name instead
routes_train <-
  routes %>% 
  filter(grepl(route_short_name, 
               pattern = "^U|^S"),
         agency_id %in% c(1,796)) %>%
  arrange(route_short_name)

# the routes table has no information about the stops, this information is 
# spread across the trips, stop_times, and stops dataframes
trips %<>%
  semi_join(routes_train,by = "route_id")

stops_by_route <-
  trips %>% 
  left_join(stop_times,by = "trip_id") %>% 
  distinct(route_id,trip_id,stop_id) %>% 
  left_join(stops %>% select(stop_id, stop_name), by = "stop_id")

# several stop_ids with same stop_name
stops_by_route %>% 
  distinct(stop_id,stop_name) %>% 
  arrange(stop_name)

# take the route with the highest number of stops to represent that specific line
routes_train %<>% 
  left_join(stops_by_route %>% 
              group_by(route_id) %>% 
              summarise(unique_stops = n_distinct(stop_name)),
            by = "route_id")

routes_train %<>% 
  arrange(route_short_name,desc(unique_stops)) %>% 
  group_by(route_short_name) %>% 
  filter(row_number() == 1) %>% 
  ungroup()
trips %<>%
  semi_join(routes_train,by = "route_id")

# for each route find the trip with the highest number of stops to represent that 
# train line
routes_train %<>% 
  left_join(trips %>% 
              left_join(stop_times,by = "trip_id") %>% 
              group_by(route_id, trip_id) %>% 
              left_join(stops %>% 
                          select(stop_id,stop_name), by = "stop_id") %>% 
              summarise(total_stops_trip = n_distinct(stop_name),
                        .groups = "drop") %>% 
              arrange(route_id,desc(total_stops_trip)) %>% 
              group_by(route_id) %>% 
              filter(row_number() == 1),
            by = "route_id")

# put all the data we need in one DF
all_routes_stops <- 
  routes_train %>% 
  select(route_id,
         route_short_name,
         trip_id) %>% 
  left_join(stop_times %>% 
              select(trip_id,
                     stop_id,
                     stop_sequence),
            by = "trip_id") %>% 
  left_join(stops %>% 
              select(stop_id,
                     stop_name,
                     stop_lat,
                     stop_lon),
            by = "stop_id") %>% 
  arrange(route_id,
          stop_sequence) %>% 
  left_join(routes_colors, 
            by = "route_short_name")

# to double check we get appropriate representations of the train lines I 
# manually checked against the stops on the official websites below
# https://www.bvg.de/images/content/linienverlaeufe/LinienverlaufU2.pdf
# https://sbahn.berlin/fahren/s9/

# correct some discrepancies here
all_routes_stops %<>% 
  filter((route_short_name == "S45" & stop_sequence <= 13) | 
           route_short_name != "S45") %>% 
  filter((route_short_name == "S46" & stop_sequence >= 5) | 
           route_short_name != "S46") %>% 
  filter((route_short_name == "S5" & stop_name != "S Grunewald (Berlin)") | 
           route_short_name != "S5")  %>% 
  filter((route_short_name == "S75" & stop_name != "S Ostbahnhof (Berlin)") | 
           route_short_name != "S75")

# for our visualisations we only want one representation for each station, in the 
# data we currently have multiple stop_ids for some stations, so we map them
stops_mappings <-
  # map all Alexanderplatz stops to "S+U Alexanderplatz Bhf (Berlin)"
  tibble(stop_id = c("070201022602",
                     "070201054002",
                     "070201083602"),
         stop_id_mapped = c("060100003723")) %>% 
  # map all "S+U Neukölln" stops to "S+U Neukölln (Berlin)"
  bind_rows(tibble(stop_id = c("070201075102",
                               "060078201462"),
                   stop_id_mapped = c("060078201461"))) %>% 
  # map Potsdamer Platz stations to "S+U Potsdamer Platz Bhf (Berlin)"
  bind_rows(tibble(stop_id = c("070201023302"),
                   stop_id_mapped = c("060100020451"))) %>% 
  # map Yorckstr. stations to "S+U Yorckstr. (Berlin)" %>% 
  bind_rows(tibble(stop_id = c("060057102801",
                               "060058103481"),
                   stop_id_mapped = c("070201074302"))) %>%
  # map Lichtenberg stations to "S+U Lichtenberg Bhf (Berlin)"
  bind_rows(tibble(stop_id = c("900000160004", "060160004099", "450009160004", 
                               "710009160004", "900000160701", "070201053201", "070201053202", 
                               "000008089182", "750000400501", "060160004691", "060160004692"),
                   stop_id_mapped = c("000008010036")))

stops_mappings %<>% 
  left_join(stops %>% 
              select(stop_id,
                     stop_name,
                     stop_lat,
                     stop_lon),
            by = c("stop_id_mapped"="stop_id")) %>% 
  rename_at(.vars = vars(-c("stop_id","stop_id_mapped")),
            .funs = ~ paste0(.,"_mapped"))

all_routes_stops %<>% 
  left_join(stops_mappings,
            by="stop_id") %>% 
  mutate(stop_id = ifelse(!is.na(stop_id_mapped),stop_id_mapped,stop_id),
         stop_name = ifelse(!is.na(stop_id_mapped),stop_name_mapped,stop_name),
         stop_lat = ifelse(!is.na(stop_id_mapped),stop_lat_mapped,stop_lat),
         stop_lon = ifelse(!is.na(stop_id_mapped),stop_lon_mapped,stop_lon)) %>% 
  select(-matches("_mapped$"))

# move "S", "S+U", "U" prefixes to end so we get a more useful alphabetical order
all_routes_stops %<>% 
  mutate(prefix = str_extract(stop_name,pattern = "^S\\+U |^S |^U ")) %>% 
  mutate(stop_name = str_replace_all(stop_name,pattern = c("^S\\+U |^S |^U "=""))) %>% 
  mutate(stop_name = ifelse(!is.na(prefix),paste0(stop_name, ", ",
                                                  str_replace_all(prefix,
                                                                  pattern = c(" $"=""))),
                            stop_name)) %>% 
  select(-prefix)

# check results of mapping
# all_routes_stops %>% distinct(stop_name) %>% arrange(stop_name) %>% View

# build graph objects ----
# helper function to generate tbl_graph object
generate_graph_data <- 
  function(nodes, routes_data) {
    edges <-
      routes_data %>% 
      select(route_short_name,
             stop_sequence_left = stop_sequence,
             stop_name_left = stop_name,
             color) %>%
      left_join(routes_data %>% 
                  select(route_short_name,
                         stop_sequence_right = stop_sequence,
                         stop_name_right = stop_name), 
                by = "route_short_name") %>%
      filter(stop_sequence_left == (stop_sequence_right-1)) %>%
      select(route_short_name,
             from = stop_name_left, 
             to = stop_name_right,
             color,
             stop_sequence = stop_sequence_left) %>% 
      rowwise() %>% 
      mutate_at(.vars = c("from","to"),.funs = ~ which(. == nodes$name)) %>% 
      ungroup()   
    return(tbl_graph(nodes, edges))
  }

# graph with alphabetical ordering by station names
all_stations <- 
  sort(unique(all_routes_stops$stop_name))
nodes <- 
  tibble(name = all_stations)
graphs <-
  list()

graphs[["alphabetical"]] <-
  generate_graph_data(nodes = nodes,
                      routes_data = all_routes_stops)
# random ordering
set.seed(123)
nodes_random <-
  nodes %>% 
  sample_n(size = nrow(.),
           replace = F)
graphs[["random"]] <-
  generate_graph_data(nodes_random, 
                      all_routes_stops)

# distance from center
berlin_center <-
  c(lat = 52.501434,
    lon = 13.402589)

stations_center_distance <-
  all_routes_stops %>% 
  distinct(stop_id,stop_name) %>% 
  group_by(stop_name) %>% 
  arrange(stop_name) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  left_join(stops %>% select(stop_id,
                             stop_lat,
                             stop_lon),
            by = "stop_id") %>% 
  rowwise() %>% 
  mutate(dist_center = distm(c(berlin_center[['lon']],berlin_center[['lat']]),
                             c(stop_lon,stop_lat),
                             fun = distHaversine)[1])

nodes_distance <-
  nodes %>% 
  left_join(stations_center_distance %>% select(stop_name,dist_center),
            by = c("name"="stop_name")) %>% 
  arrange(dist_center) %>% 
  select(-dist_center)

graphs[["distance_center"]] <-
  generate_graph_data(nodes = nodes_distance,
                      routes_data = all_routes_stops)

# number of edges by node ----
nodes_number_edges <-
  nodes[graphs[["alphabetical"]] %>% 
          activate(edges) %>% 
          as_tibble() %>% 
          select(from,to) %>% 
          gather(key,value) %>% 
          count(value) %>% 
          arrange(desc(n)) %>% 
          pull(value),]
graphs[["number_edges"]] <-
  generate_graph_data(nodes = nodes_number_edges,
                      routes_data = all_routes_stops)

# create plots ----
plots <- 
  c(plots,unlist(lapply(names(graphs),function(n) {
    out <- list()
    out[[n]] <-
      ggraph(graphs[[n]], 
             layout = 'linear', circular = T) +
      geom_edge_arc(aes(color = route_short_name),
                    alpha = 0.6) +
      scale_edge_color_manual(values = routes_colors_vec) +
      geom_node_point(size = 0.5,
                      color = "darkgrey")
    
    out[[paste0(n,"_facetted")]] <-
      ggraph(graphs[[n]], 
             layout = 'linear', circular = T) +
      geom_edge_arc(aes(color = route_short_name),
                    alpha = 0.8) +
      scale_edge_color_manual(values = routes_colors_vec) +
      geom_node_point(size = 0.01,
                      color = "lightgrey") +
      facet_wrap(~route_short_name)
    return(out)
  }),recursive = F))
plots

# make plot more beautiful
plots[["random_beautified"]] <-
  ggraph(graphs[["random"]], 
         layout = 'linear', circular = T) +
  geom_edge_arc(color = "ivory",
                alpha = 0.3) +
  geom_node_point(size = 1,
                  color = "lightgrey") +
  theme(plot.background = element_rect(fill = "dodgerblue4"))

# write out plots
lapply(names(plots),function(n) {
  if(grepl(n,pattern = "_facetted$")) {
    ggsave(filename = paste0("out/",n,".png"),
           plot = plots[[n]], 
           width = 12, 
           height = 12)
  } else {
    ggsave(filename = paste0("out/",n,".png"),
           plot = plots[[n]], 
           width = 8, 
           height = 8)   
  }
})

# animation ----
# move from geographical location to circle and have arcs appear
animations <-
  list()
nodes_geographical <-
  all_routes_stops %>% 
  distinct(stop_name,stop_lat,stop_lon) %>% 
  # project onto plane (https://stackoverflow.com/questions/16266809/convert-from-latitude-longitude-to-x-y)
  mutate(x = -stop_lon * cos(berlin_center['lat']),
         y = stop_lat) %>% 
  # scale to -1...+1
  mutate_at(c("x","y"), .funs = ~ -1 + (.-min(.))*2/(max(.) - min(.)))

graphs[["geographical_to_circle"]] <-
  tbl_graph(nodes = create_layout(graphs[["random"]], 
                                  layout = "circle") %>% 
              select(name, x,y) %>% 
              mutate(frame = 2)  %>% 
              bind_rows(nodes_geographical %>% 
                          select(name = stop_name, x, y) %>% 
                          mutate(frame = 1)) %>% 
              arrange(desc(frame), name))

animations[["geographical_to_circle"]] <-
  ggraph(graphs[["geographical_to_circle"]],
         x = x, 
         y = y) + 
  geom_node_point(size = 0.5,
                  color = "white") +
  transition_states(frame,
                    transition_length = 1,
                    state_length = 1,
                    wrap = F) +
  theme(plot.background = element_rect(fill = "dodgerblue4"))
anim_save(filename = "out/geographical_to_circle.gif",
          animation = animations[["geographical_to_circle"]], 
          duration = 3)

# and the reverse animation for the end
graphs[["circle_to_geographical"]] <-
  graphs[["geographical_to_circle"]] %>% 
  activate(nodes) %>% 
  mutate(frame = ifelse(frame == 2, 1, 2))
animations[["circle_to_geographical"]] <-
  ggraph(graphs[["circle_to_geographical"]],
         x = x, 
         y = y) + 
  geom_node_point(size = 0.5,
                  color = "white") +
  transition_states(frame,
                    transition_length = 1,
                    state_length = 1,
                    wrap = F) +
  theme(plot.background = element_rect(fill = "dodgerblue4"))

anim_save(filename = "out/circle_to_geographical.gif",
          animation = animations[["circle_to_geographical"]], 
          duration = 3)

# second part: have lines appear in groups
num_frames <-
  40
set.seed(31412)
nodes_random_animation <-
  graphs[["random"]] %>% 
  activate(edges) %>% 
  as_tibble() %>% 
  mutate(first_frame = sample(1:(num_frames/2),nrow(.), replace = T),
         last_frame = first_frame + num_frames/2) %>% 
  crossing(frame = 1:num_frames) %>% 
  filter(frame >= first_frame, frame <= last_frame) %>% 
  select(from, to, frame)

graphs[["random_animation"]] <-
  tbl_graph(nodes = nodes_random, 
            nodes_random_animation)

animations[["random_animation"]] <-
  ggraph(graphs[["random_animation"]],
         layout = 'linear', circular = T) + 
  geom_node_point(size = 0.5,
                  color = "white") +
  geom_edge_arc(color = "ivory",
                alpha = 0.35) +
  transition_states(frame,
                    transition_length = 2,
                    state_length = 1,
                    wrap = F) +
  theme(plot.background = element_rect(fill = "dodgerblue4")) +
  enter_appear() + 
  exit_disappear()

anim_save(filename = "out/animate_edges_appearing.gif",
          animation = animations[["random_animation"]], 
          duration = 24)

# could not find a way to merge the two animations in gganimate so will be 
# done with gifsicle with the gif files