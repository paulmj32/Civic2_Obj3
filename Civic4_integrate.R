##########################################################################################
#### LOAD PACKAGES AND SET WORKING DIRECTORY #############################################
##########################################################################################
require(plumber)
require(tidyverse)
require(tidycensus) 
require(sf)
require(xgboost)
require(viridis)
require(spdep)

setwd("~/Documents/Civic2") #download GitHub folder to this location 

##########################################################################################
#### LOAD DATA ###########################################################################
##########################################################################################
## Census Map
mycrs = 5070 #chose projected coordinate system: EPSG 5070 NAD83 Conus Albers
year=2019 # year for county boundaries 
options(tigris_use_cache = TRUE) #cache shapefiles for future sessions
state = "Texas"
county = c("Harris County")
census_map = get_acs(geography = "tract", state = state, county = county,
                     variables=c("B01003_001"), year = year, geometry = TRUE, 
                     cache_table = TRUE)
census_map = census_map %>%
  mutate(POPULATION = estimate) %>%
  dplyr::select(GEOID, NAME, POPULATION) %>% 
  st_transform(mycrs) # project to Conic Equal Area Albers, EPSG:5070 
census_map_area = census_map %>%  
  mutate(AREA = as.vector(st_area(census_map))) %>% #sq-meters; as.vector removes units suffix 
  mutate(DENSITY = POPULATION / AREA * 1000^2) #population per sq-km 

## NLCD Land Type
load(file = "./Data/census_map_nlcd.Rda") 
census_map_nlcd = census_map_nlcd %>% st_set_geometry(NULL)

## Digital Elevation Map 
load(file = "./Data/census_map_dem.Rda") 
census_map_dem = census_map_dem %>% st_set_geometry(NULL)

## Root Zone 
load(file = "./Data/census_map_rz.Rda") 
census_map_rz = census_map_rz %>% st_set_geometry(NULL)

## Socio-Economic 
load(file = "./Data/census_map_social.Rda") 
census_map_social = census_map_social %>% st_set_geometry(NULL)

## SPI drought 
load(file = "./Data/census_map_spi.Rda") 
census_map_spi = census_map_spi %>% st_set_geometry(NULL)

## Harvey wind forecast
load(file = "./Data/census_map_WINDmax.Rda") 
census_map_WINDmax = census_map_WINDmax %>% st_set_geometry(NULL)

# Join everything together 
census_map_CLEAN = census_map_area %>%
  dplyr::select(GEOID, DENSITY) %>%
  inner_join(dplyr::select(census_map_nlcd, c(GEOID, Developed:Wetlands)), by = "GEOID") %>%
  inner_join(dplyr::select(census_map_dem, c(GEOID, DEM_mean:DEM_max)), by = "GEOID") %>%
  inner_join(dplyr::select(census_map_rz, c(GEOID, RZ_mean:RZ_mode)), by = "GEOID") %>%
  inner_join(dplyr::select(census_map_social, c(GEOID, 1, 6:ncol(census_map_social))), by = "GEOID") %>%
  inner_join(dplyr::select(census_map_spi, c(GEOID, spi03_mean:spi24_mean)), by = "GEOID") %>%
  inner_join(census_map_WINDmax, by = "GEOID") %>%
  rename(spi03_lag = spi03_mean, spi12_lag = spi12_mean, spi24_lag = spi24_mean, 
         Density = DENSITY)

##########################################################################################
#### MODEL  ##############################################################################
##########################################################################################
load(file = "xgb_final_model.Rda") 
census_map_FINAL = census_map_CLEAN
X = census_map_FINAL %>%
  dplyr::select(xgb_final_model$feature_names) %>%
  st_set_geometry(NULL) %>%
  as.matrix()
predictions = predict(xgb_final_model, X)


##########################################################################################
#### RISK MAP  ###########################################################################
##########################################################################################
## Map of model predictions (raw and normalized)
Harris_map = census_map_FINAL %>%
  mutate(risk = predictions) %>%
  mutate(risk01 = (risk - min(predictions)) / (max(predictions) - min(predictions))) 

## Getis-Ord Gi* HotSpot Analysis
neighbors = poly2nb(Harris_map, queen = T) #find census tract polygon neighbors 
localg_weights = nb2listw(include.self(neighbors)) #include self as a neighbor for Local Gi* test
local_gistar = localG(Harris_map$risk01, localg_weights)
Harris_map$gistar = local_gistar
Harris_map = Harris_map %>%
  mutate(gistar01 = (gistar - min(local_gistar)) / (max(local_gistar) - min(local_gistar))) #normalize Gi*

## Plot Map 
gg2 = ggplot(Harris_map)+
  geom_sf(aes(fill = gistar01), color = NA) + 
  scale_fill_viridis_c(option="plasma", na.value = "grey50") +
  theme_dark() +
  labs(title = "Harris County - Power Outage Risk", fill = "Risk") + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
  )
print(gg2)

## Output to csv
obj3_out = Harris_map %>%
  #dplyr::filter(GEOID %in% obj3_tracts) %>%
  arrange(GEOID) %>%
  mutate(geoId = GEOID) %>%
  mutate(vulnerabilityIndex = as.numeric(gistar01)) %>%
  dplyr::select(geoId, vulnerabilityIndex) %>%
  st_set_geometry(NULL) %>%
  dplyr::slice(1:50)
#write.csv(obj3_out, file = "./Data/obj3_out.csv", row.names = FALSE)

require(jsonlite)
require(httr)
api = 'https://civic2-webapi.azurewebsites.net/powergridvulnerability'
api2 = "https://269a5777-047e-4d5a-94f8-6c8dfee7001c.mock.pstmn.io"
#https://stackoverflow.com/questions/39809117/how-to-post-api-in-r-having-header-json-body
obj3_out_json = toJSON(obj3_out, pretty = T)
POST(url = api,
     body = obj3_out_json,
     add_headers(.headers = c("Content-Type" = "application/json"))
     )

#GET try
get = GET(url = api)
get_content_text = content(get, as = "text", encoding = "UTF-8")
get_content_json = fromJSON(get_content_text)

to_json = toJSON(get_content_json, pretty = T)
POST(url = api, accept_json(), content_type_json(), body = to_json)


