# BIEN data collection script

library(BIEN)
library(maps)
library(data.table)

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

# list of species names for TNRS validation
nw.countries <- c("Canada", "United States", "Mexico", "Guatemala", "Brazil", "Peru", "Argentina", "Colombia", "Chile", "Venezuela",
                  "Cuba", "Costa Rica", "Ecuador", "Panama", "Bolivia", "Dominican Republic", "Puerto Rico", "Uruguay", "Honduras", 
                  "Belieze", "Nicaragua", "Jamaica", "El Salvador", "Haiti", "Paraguay", "Guyana", "Bahamas", "Aruba", "Suriname",
                  "Barbados", "Curacao", "Turks and Caicos Islands", "Saint Lucia", "Bermuda", "Guadeloupe", "Martinique", 
                  "Cayman Islands", "French Guiana", "Trinidad and Tobago", "Grenada", "Falkland Islands", "Dominica",
                  "Antigua and Barbuda", "Saint Kitts and Nevis", "Saint Vincent and the Grenadines", "Montserrat", "Anguilla",
                  "Sint Maarten", "Carribean Netherlands")
bien.nw.species <- BIEN_list_country(country=nw.countries) # list of species in all the new world countries
bien.nw.unique.species <- unique(bien.nw.species$scrubbed_species_binomial) # unique species
dataframe <- as.data.frame(bien.nw.unique.species) # set as a dataframe for output
write.csv(dataframe, "/home/eric/Documents/Projects/C_Working/CC_Extinction/BIEN_NW_specieslist.csv") 
write.csv(dataframe, "C:/Users/Eric/Documents/Plant_Extinction/unique_species.csv")
# to view this ^^^ in a csv reader, have to change the input format
# this is input to TNRS, which will generate matches

# Compare names in TNRS
BIEN.species.corrected <- read.csv("/home/eric/Documents/Projects/C_Working/CC_Extinction/tnrs_bien_NW_results.csv", 
                                   fileEncoding = "UTF-16")

BIEN.species.corrected <- read.csv("C:/Users/Eric/Documents/Plant_Extinction/tnrs_bien_NW_results.csv", 
                                   fileEncoding = "UTF-16")
# Join requested species list with list of species available in BIEN by corrected name
species.translation <- merge(species, BIEN.species.corrected, by.x = "x", by.y = "Name_matched", all.x=TRUE)

# Use BIEN name to get requested occurrence data
# List species not covered by the BIEN data (Name_submitted == <NA>)
not.included <- species.translation[is.na(species.translation$Name_submitted),1]

# Clear out rows that are not in BIEN dataset (Name_submitted != <NA>)
species.in.BIEN <- species.translation[!is.na(species.translation$x),1]

# Occurrence data for all species (WARNING: THIS IS LIKE 30 GB OF DATA without only.new.world/native status = T)
species.occurrence <- BIEN_occurrence_species(species=species.in.BIEN, only.new.world = T, native.status = T)
species.occurrence <- fread("C:/Users/Eric/Documents/Plant_Extinction/NW_Native_Occurrence.csv")
write.csv(species.occurrence, "C:/Users/Eric/Documents/Plant_Extinction/NW_Native_Occurrence.csv")
species.occurrence <- read.csv("/home/eric/Documents/Projects/C_Working/CC_Extinction/NW_Native_Occurrence.csv")
write.csv(species.occurrence, "/home/eric/Documents/Projects/C_Working/CC_Extinction/NW_Native_Occurrence.csv")

# In ArcMap, combined all species range shapefiles into one file using Merge
## Potentially just use occurrence data exclusively
# In ArcMap, combine biome data into 14 discrete biomes based on Biome column of WWF data using merge
library(rgeos)
library(rgdal)
library(maptools)
library(gridExtra)
library(sp)

WWF_Biomes <- readOGR(dsn = "C:/Users/Eric/Documents/Plant_Extinction/GIS_Data/WWF_Biomes", 
                      layer = "wwf_terr_ecos")
WWF_Biomes <- readOGR(dsn = "/home/eric/Documents/Projects/C_Working/CC_Extinction/Biome_Boundaries_WWF", 
                      layer = "wwf_terr_ecos")

#View shapefile attributes
head(WWF_Biomes@data)

# get rid of data that has NAs in the location data
so.cleaned <- species.occurrence[complete.cases(species.occurrence[, c("latitude", "longitude")]),]

# make the coodinates read as geographic data
coordinates(so.cleaned) <- ~longitude+latitude

# get rid of some messed up location data
so.cleaned <- so.cleaned[so.cleaned@coords[,2]<90,]

# attach a coordinate system to the data
proj4string(so.cleaned) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# UNNECESSARY? so.projected <- spTransform(so.cleaned, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# create a table of which occurrences are in which biomes, and add that table to the cleaned species occurrence data table
species.occurrence.data <- over(so.cleaned, WWF_Biomes[, "BIOME"])
so.cleaned$biome <- species.occurrence.data$BIOME

# collapse the species data so it has species and biome only
so.unique <- unique(so.cleaned[,c("scrubbed_species_binomial", "biome")])

# export occurrence - biome data
write.csv(so.unique, "C:/Users/Eric/Documents/Plant_Extinction/species_with_biome.csv")

# function(species_dir){
#     species.list <- read.csv(species_dir)
#     data.available <- BIEN_ranges_species(species=species.list$x, match_names_only = TRUE)
#     data.downloadable <- subset(data.available, `Range_map_available?` == "Yes")
#     data.missed <- subset(data.available, `Range_map_available?` == "No")
#     
# }

## Trait Data
# how many of which observed trait data per species
BIEN_trait_traits_per_species(species = species.in.BIEN)

# download trait data
species.traits <- BIEN_trait_species(species=species.in.BIEN)
write.csv(species.traits, "/home/eric/Documents/Projects/C_Working/CC_Extinction/species_trait_data.csv")
write.csv(species.traits, "C:/Users/Eric/Documents/Plant_Extinction/species_trait_data.csv")

out.df <- data.frame(matrix(ncol=7, nrow=0))
column.names <- c("Species", "Plant_Height_Min", "Plant_Height_Max", 
                  "Seed_Mass_Min", "Seed_Mass_Max", "Growth Form", "Dispersal Mode") #Add biome to this list?
colnames(out.df) <- column.names
library(data.table)
for (i in unique(species.traits$scrubbed_species_binomial)){
    # For each species, find the max and min of each trait of interest
    active.subset <- subset(species.traits, species==i)
    spec.out <- i
    # does growth form need to have separate values for the other trait ranges? 
    # i.e. something as a shrub has some height values, where that thing as a tree has another range
    # this does that. ^^ if it shouldn't, move everything but the growth form variable out of this for loop
    # growth form will need to be looked into a little bit more (herb_tree? really??)
    for (j in unique(active.subset$trait_value[active.subset$trait_name=="whole plant growth form diversity"])){
      gf <- j
      # same goes for dispersal syndrome (we will see if it matters in the output) -- skipping blank dispersal syndromes
      wpds <- unique(active.subset$trait_value[active.subset$trait_name=="whole plant dispersal syndrome"])
      wpds[is.na(wpds)] <- "none"
      for (k in wpds){
        ds <- k
        # print(unique(active.subset$trait_name))
        ph <- active.subset$trait_value[active.subset$trait_name=="whole plant height"]
        phmin <- min(ph)
        phmax <- max(ph)
        sm <- active.subset$trait_value[active.subset$trait_name=="seed mass"]
        smmin <- min(sm)
        smmax <- max(sm)
        data.out <- c(i, phmin, phmax, smmin, smmax, gf, ds)
        out.df = rbindlist(list(out.df, as.list(data.out)))
      }
    }
}

## species names are not matching!!
write.csv(out.df, "C:/Users/Eric/Documents/Plant_Extinction/traits_by_species.csv")
write.csv(out.df, "/home/eric/Documents/Projects/C_Working/CC_Extinction/traits_by_species.csv")
# Creates a background map
map('world', fill=T, col="grey", bg="light blue", xlim=c(-180, -20), ylim=c(-60, 80))
# Plots occurrence points on the map
points(cbind(so.cleaned$longitude, so.cleaned$latitude), col="red")

plot(species.occurrence, col="green", add=T)
