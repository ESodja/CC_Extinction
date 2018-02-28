# BIEN data collection script

library(BIEN)
library(maps)

# Import the species list
## species <- read.csv("this is a species csv.csv", as.is=TRUE)
#testing
species <- read.csv("C:/Users/Eric/Documents/Plant_Extinction/SpeciesNames_1missing_irred.csv", as.is = TRUE)
species <- read.csv("/home/eric/Documents/Projects/C_Working/CC_Extinction/SpeciesNames_1missing_irred.csv", as.is=TRUE)

# grab the species name column (genus species) -- as character -- only for getting list of species from BIEN
#name <- species$scrubbed_species_binomial

#gsub("\\", "", species, fixed=TRUE)
# get list of all shapefiles -- gives species names as integer?
data.available <- BIEN_ranges_species(species=species$x,  match_names_only = TRUE)

# subset to only available ones
data.downloadable <- subset(data.available, `Range_map_available?` == "Yes")
data.missed <- subset(data.available, `Range_map_available?` == "No")
write.csv(data.missed, "C:/Users/Eric/Documents/Plant_Extinction/missing_species.csv")

#merged <- merge(species, data.missed, by.x = species$x, by.y = data.missed$Species)

data.downloadable$as.char <- as.character(data.downloadable$Species)
gsub("_", " ", data.downloadable$as.char, fixed = TRUE)

# download all available shapefiles -- requires character
data.download <- BIEN_ranges_species(species=data.downloadable$as.char, 
                                     directory="C:/Users/Eric/Documents/Plant_Extinction/GIS_Data")

# Occurrence data for all species (WARNING: THIS IS LIKE 30 GB OF DATA without only.new.world/native status = T)
species.occurrence <- BIEN_occurrence_species(species=species$x, only.new.world = T, native.status = T)
write.csv(species.occurrence, "C:/Users/Eric/Documents/Plant_Extinction/GIS_Data/species_occurrences.csv")
write.csv(species.occurrence, "/home/eric/Documents/Projects/C_Working/CC_Extinction/NW_Native_Occurrence.csv")

# In ArcMap, combined all species range shapefiles into one file using Merge
## Potentially just use occurrence data exclusively
# In ArcMap, combine biome data into 14 discrete biomes based on Biome column of WWF data using merge
library(rgeos)
library(rgdal)
library(maptools)
library(gridExtra)
library(sp)

WWF_Biomes <- readOGR(dsn = "/home/eric/Documents/Projects/C_Working/CC_Extinction/Biome_Boundaries_WWF", 
                      layer = "wwf_terr_ecos")

#View shapefile attributes
head(WWF_Biomes@data)

# get rid of data that has NAs in the location data
so.cleaned <- species.occurrence[complete.cases(species.occurrence[, c("latitude", "longitude")]),]

coordinates(so.cleaned) <- ~longitude+latitude
proj4string(so.cleaned) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
so.projected <- spTransform(so.cleaned, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
species.occurrence.data <- over(so.cleaned, WWF_Biomes[, "BIOME"])
species.occurrence$biome <- species.occurrence.data$
Merged_Biomes <- unionSpatialPolygons(WWF_Biomes, unique(WWF_Biomes$BIOME))

# Spatial join to get biomes and species (i.e. which biomes have what species in them)

# function(species_dir){
#     species.list <- read.csv(species_dir)
#     data.available <- BIEN_ranges_species(species=species.list$x, match_names_only = TRUE)
#     data.downloadable <- subset(data.available, `Range_map_available?` == "Yes")
#     data.missed <- subset(data.available, `Range_map_available?` == "No")
#     
# }

## Trait Data
# how many of which observed trait data per species
BIEN_trait_traits_per_species(species = species$x)

# download trait data
species.traits <- BIEN_trait_species(species=species$x)
write.csv(species.traits, "/home/eric/Documents/Projects/C_Working/Biomes/species_trait_data.csv")

out.df <- data.frame(matrix(ncol=7, nrow=0))
column.names <- c("Species", "Plant_Height_Min", "Plant_Height_Max", 
                  "Seed_Mass_Min", "Seed_Mass_Max", "Growth Form", "Dispersal Mode") #Add biome to this list?
colnames(out.df) <- column.names

for (i in unique(species.traits$scrubbed_species_binomial)){
    # For each species, find the max and min of each trait of interest
    active.subset <- subset(species.traits, species==i)
    spec.out <- i
    phmin <- min(active.subset$)
    outlist <- list(nrow=8)
}

# Creates a background map
map('world', fill=T, col="grey", bg="light blue", xlim=c(-180, -20), ylim=c(-60, 80))
# Plots occurrence points on the map
points(cbind(so.cleaned$longitude, so.cleaned$latitude), col="red")

plot(species.occurrence, col="green", add=T)
