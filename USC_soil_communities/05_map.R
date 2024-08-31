library(tigris)
library(usmap)
library(sf)
library(ggplot2)
#library(rgdal)
library(ggspatial)
library(raster)
library(cowplot)
library(egg)
library(rnaturalearth)
library(kml)
library(grid)
library(mapproj)

site_metadata<-read.csv("~/Documents/USC_soil_communities/NEON_Field_Site_Metadata_20240723.csv")

site_metadata<-site_metadata%>%select(field_domain_id,field_site_id,field_site_name,field_latitude,field_longitude) %>% 
  rename(siteID = field_site_id)

# Convert the latitude and longitude data into an sf object
alaska_domain_df <- domain_df %>%
  select(everything()) %>% 
  filter(DomainName == "Tundra" | DomainName == "Taiga")
alaska_sample <- sampling_boundaries_df %>% 
  select(everything()) %>% 
  filter(domainName == "Tundra" | domainName == "Taiga")
alaska_site <- site_locations_sf %>% 
  select(everything()) %>% 
  filter(siteID == "BONA" | siteID == "DEJU"| siteID == "HEAL"| siteID == "BARR"| siteID == "TOOL")


hawaii_domain_df <- domain_df %>%
  select(everything()) %>% 
  filter(DomainName == "Pacific Tropical")
hawaii_sample <- sampling_boundaries_df %>% 
  select(everything()) %>% 
  filter(domainName == "Pacific Tropical")
hawaii_site <- site_locations_sf %>% 
  select(everything()) %>% 
  filter(siteID == "PUUM")

atl_domain_df <- domain_df %>%
  select(everything()) %>% 
  filter(DomainName == "Atlantic Neotropical")
atl_sample <- sampling_boundaries_df %>% 
  select(everything()) %>% 
  filter(domainName == "Atlantic Neotropical")
atl_site <- site_locations_sf %>% 
  select(everything()) %>% 
  filter(siteID == "GUAN"| siteID == "LAJA")

site_locations_sf <- st_as_sf(site_metadata, coords = c("field_longitude", "field_latitude"), crs = 4326) %>% 
  inner_join(site_color_df, by = join_by(siteID)) 
domain_df<-read_sf("~/Documents/USC_soil_communities/NEONDomains_0/NEON_Domains.shp")  
sampling_boundaries_df<-read_sf("~/Documents/USC_soil_communities/Field_Sampling_Boundaries_2024/terrestrialSamplingBoundaries.shp") %>% 
  inner_join(site_color_df, by = join_by(siteID))

cat_domain_df <- read_sf("~/Documents/USC_soil_communities/catalina/Zoning_-_Catalina.shp") %>% 
  mutate(c(item = "", "", "", "USC Wrigley Campus", "", "", "", "Avalon", "Two Harbors", "", "")) %>% 
  rename(city = "c(...)") %>% 
  st_as_sf(crs = st_crs(4326))
 
lon = c(33.3428, 33.4400, 33.4400)
lat = c(118.3282,118.4983, 118.4883)


ggplot() +
  geom_sf(data = cat_domain_df, fill = "lightpink", color = "black")+
  geom_point(data = recruitment_info, aes(x = geometry, y = geometry, color = city))+
  theme_cowplot()+
  labs(title = "Catalina Island Summer 2024", caption = "Source: County of Los Angeles Enterprise GIS")


recruitment_info <- data.frame(city = c("Avalon", "Two Harbors", "USC Wrigley"),
                               Lat = c(118.32, 118.49, 118.48),
                               Long = c(33.34, 33.44, 33.46),
                               stringsAsFactors = FALSE) %>% 
  st_as_sf(coords = c("Long", "Lat"), crs = st_crs(4326))


US_domain_df<-read_sf("~/Documents/USC_soil_communities/NEONDomains_0/NEON_Domains.shp") %>% 
  select(everything()) %>% 
  filter(!(DomainName == "Tundra" | DomainName == "Taiga" | DomainName == "Pacific Tropical"| DomainName == "Atlantic Neotropical")) 

US_sampling_boundaries_df<-read_sf("~/Documents/USC_soil_communities/Field_Sampling_Boundaries_2024/terrestrialSamplingBoundaries.shp") %>%
  select(everything()) %>% 
  filter(!(domainName == "Tundra" | domainName == "Taiga" | domainName == "Pacific Tropical"| domainName == "Atlantic Neotropical")) %>% 
  inner_join(site_color_df, by = join_by(siteID))

US_site_locations_sf <- st_as_sf(site_metadata, coords = c("field_longitude", "field_latitude"), crs = 4326) %>% 
  inner_join(site_color_df, by = join_by(siteID)) %>% 
  filter(!(siteID == "BONA" | siteID == "DEJU"| siteID == "HEAL"| siteID == "BARR"| siteID == "TOOL"| siteID == "PUUM"| siteID == "GUAN"| siteID == "LAJA"))

# Extract the KMZ file to a temporary directory
kmz_file <- "~/Documents/USC_soil_communities/NEON_Field_Sites_KMZ_v19_Apr2024.kmz"
temp_dir <- tempdir()
unzip(kmz_file, exdir = temp_dir)




# List the contents of the extracted files
extracted_files <- list.files(temp_dir, full.names = TRUE)
kml_file <- extracted_files[grepl(".kml$", extracted_files)]
kmz_df <- read_sf(kml_file)

map_final <- ggplot() +
  geom_sf(data = domain_df, fill = "beige"  , color = "black") +
  geom_sf(data = sampling_boundaries_df, fill = NA, aes(color = color)) +
  geom_sf(data = site_locations_sf, fill = NA, aes(color = color, size = 1)) +
  scale_color_identity(guide="legend", labels=unique(sampling_boundaries_df$siteID))+
  labs(title = "NEON Domains with Sampling Sites",
       subtitle = "Overlay of NEON Domains, Field Sampling Boundaries, and KMZ Data",
       caption = "Source: NEON", color = "siteID") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom", panel.background = element_rect(fill='white', colour='black'))
print(map_final)

US_map_final <- ggplot() +
  geom_sf(data = US_domain_df, fill = "beige"  , color = "black") +
  geom_sf(data = US_sampling_boundaries_df, fill = NA, aes(color = color)) +
  geom_sf(data = US_site_locations_sf, fill = NA, aes(color = color, size = 1)) +
  scale_color_identity(guide="legend", labels=unique(US_sampling_boundaries_df$siteID))+
  labs(title = "NEON Domains with Sampling Sites",
       subtitle = "Overlay of NEON Domains, Field Sampling Boundaries, and KMZ Data",
       caption = "Source: NEON", color = "siteID") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none", panel.background = element_rect(fill='white', colour='black')) 
  print(US_map_final)
ggsave(plot=US_map_final, dpi=300, units ="cm", width =40, height =15, bg="white", filename= "~/Documents/USC_soil_communities/US_map_final.png")

alaska_map <- ggplot() +
  geom_sf(data = alaska_domain_df, fill = "beige"  , color = "black")+
geom_sf(data = alaska_sample, fill = NA, aes(color = color)) +
  geom_sf(data = alaska_site, fill = NA, aes(color = color, size = 1)) +
  scale_color_identity(guide="legend", labels=unique(alaska_sample$siteID))+
  labs(color = "siteID") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none", panel.background = element_rect(fill='white', colour='black')) 

ggsave(plot=alaska_map, dpi=300, units ="cm", width =40, height =15, bg="white", filename= "~/Documents/USC_soil_communities/alaska_map.png")

hawaii_map <- ggplot() +
  geom_sf(data = hawaii_domain_df, fill = "beige"  , color = "black")+
  geom_sf(data = hawaii_sample, fill = NA, aes(color = color)) +
  geom_sf(data = hawaii_site, fill = NA, aes(color = color, size = 1)) +
  scale_color_identity(guide="legend", labels=unique(alaska_sample$siteID))+
  labs(color = "siteID") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none", panel.background = element_rect(fill='white', colour='black'))

ggsave(plot=hawaii_map, dpi=300, units ="cm", width =40, height =15, bg="white", filename= "~/Documents/USC_soil_communities/hawaii_map.png")

atl_map <- ggplot() +
  geom_sf(data = atl_domain_df[2,], fill = "beige"  , color = "black")+
  geom_sf(data = atl_sample, fill = NA, aes(color = color)) +
  geom_sf(data = atl_site, fill = NA, aes(color = color, size = 1)) +
  scale_color_identity(guide="legend", labels=unique(atl_sample$siteID))+
  labs(color = "siteID") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none", panel.background = element_rect(fill='white', colour='black'))

ggsave(plot=atl_map, dpi=300, units ="cm", width =40, height =15, bg="white", filename= "~/Documents/USC_soil_communities/atl_map.png")

extract_legend <- function(plot) {
  g <- ggplotGrob(plot)
  legend <- g$grobs[[which(sapply(g$grobs, function(x) x$name) == "guide-box")]]
  return(legend)
}

##map legend
map_legend <- extract_legend(map_final)

# Display the legend
grid.newpage()
grid.draw(map_legend)

ggsave(plot=map_legend, dpi=300, units ="cm", width =40, height =15, bg="white", filename= "~/Documents/USC_soil_communities/map_legend.png")

