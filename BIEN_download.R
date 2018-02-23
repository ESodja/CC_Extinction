# BIEN data collection script

library(BIEN)
library(maps)

# Import the species list
## species <- read.csv("this is a species csv.csv", as.is=TRUE)
#testing
species <- read.csv("C:/Users/Eric/Documents/Plant_Extinction/SpeciesNames_1missing_irred.csv", as.is = TRUE)

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

# Occurrence data for all species (WARNING: THIS IS LIKE 30 GB OF DATA)
species.occurrence <- BIEN_occurrence_species(species=species$x)

# In ArcMap, combined all species range shapefiles into one file using Merge
# In ArcMap, combine biome data into 14 discrete biomes based on Biome column of WWF data using merge
# Spatial join to get biomes and species (i.e. which biomes have what species in them)

function(species_dir){
    species.list <- read.csv(species_dir)
    data.available <- BIEN_ranges_species(species=species.list$x, match_names_only = TRUE)
    data.downloadable <- subset(data.available, `Range_map_available?` == "Yes")
    data.missed <- subset(data.available, `Range_map_available?` == "No")
    
}

## Trait Data
# how many of which observed trait data per species
BIEN_trait_traits_per_species(species = species$x)

# download trait data
species.traits <- BIEN_trait_species(species=species$x)
write.csv(species.traits, "/home/eric/Documents/Projects/C_Working/Biomes/species_trait_data.csv")

map('world', fill=T, col="grey", bg="light blue", xlim=c(-180, -20), ylim=c(-60, 80))

plot(data.exists, col="green", add=T)
