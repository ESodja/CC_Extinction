# BIEN data collection script

library(BIEN)
library(data.table)

# Import the species list
species <- read.csv("C:/Users/Eric/Documents/Plant_Extinction/SpeciesNames_1missing_irred.csv", as.is = TRUE)
species <- read.csv("/home/eric/Documents/Projects/C_Working/CC_Extinction/SpeciesNames_1missing_irred.csv", as.is=TRUE)

### SPECIFIC TO THE EXISTING RANGE MAPS ###

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

### END RANGE MAP DOWNLOAD ###

### OCCURRENCE DATA DOWNLOAD ###

# list of species names for TNRS validation
bien.species <- unique(BIEN_list_all())
# csv for TNRS validation
write.csv(bien.species, "/home/eric/Documents/Projects/C_Working/CC_Extinction/BIEN_specieslist.csv") 
write.csv(bien.species, "C:/Users/Eric/Documents/Plant_Extinction/unique_species.csv")
# to view this ^^^ in a csv reader, have to change the input format
# this is input to TNRS, which will generate matches

# Compare names in TNRS
BIEN.species.corrected <- read.table("/home/eric/Documents/Projects/C_Working/CC_Extinction/tnrs_bien_results.txt", 
                                   fileEncoding = "UTF-16", sep="\t", header = TRUE)

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
species.occurrence <- BIEN_occurrence_species(species=species.in.BIEN, only.new.world = FALSE)
species.occurrence <- fread("C:/Users/Eric/Documents/Plant_Extinction/NW_Native_Occurrence.csv")
write.csv(species.occurrence, "C:/Users/Eric/Documents/Plant_Extinction/NW_Native_Occurrence.csv")
species.occurrence <- read.csv("/home/eric/Documents/Projects/C_Working/CC_Extinction/NW_Native_Occurrence.csv")
write.csv(species.occurrence, "/home/eric/Documents/Projects/C_Working/CC_Extinction/Native_Occurrence.csv")

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
WWF_Biomes <- readOGR(dsn = "/home/eric/Documents/Projects/C_Working/CC_Extinction/WWF_Biomes", 
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

# create a table of which occurrences are in which biomes, and add that table to the cleaned species occurrence data table
species.occurrence.data <- over(so.cleaned, WWF_Biomes[, "BIOME"])
so.cleaned$biome <- species.occurrence.data$BIOME

# collapse the species data so it has species and biome only
so.unique <- unique(data.frame(species = so.cleaned$scrubbed_species_binomial, biome = so.cleaned$biome)[,c("species", "biome")])

# export occurrence - biome data
write.csv(so.unique, "C:/Users/Eric/Documents/Plant_Extinction/species_with_biome.csv")
write.csv(so.unique, "/home/eric/Documents/Projects/C_Working/CC_Extinction/species_with_biome.csv")
so.unique <- fread("/home/eric/Documents/Projects/C_Working/CC_Extinction/species_with_biome.csv")
## Trait Data
# how many of which observed trait data per species
BIEN_trait_traits_per_species(species = species.in.BIEN)

# download trait data
library(data.table)
species.traits <- BIEN_trait_species(species=species.in.BIEN)
write.csv(species.traits, "/home/eric/Documents/Projects/C_Working/CC_Extinction/species_trait_data.csv")
write.csv(species.traits, "C:/Users/Eric/Documents/Plant_Extinction/species_trait_data.csv")
species.traits <- fread("/home/eric/Documents/Projects/C_Working/CC_Extinction/species_trait_data.csv")

N=length(species$x)
out.df <- data.frame(txt=rep("", N), num=rep(NA, N), num=rep(NA, N), num=rep(NA, N), num=rep(NA, N),
                     txt=rep("", N), txt=rep("", N), txt=rep("", N), stringsAsFactors=FALSE)#matrix(ncol=8, nrow=0))
column.names <- c("Species", "Plant_Height_Min", "Plant_Height_Max", 
                  "Seed_Mass_Min", "Seed_Mass_Max", "Growth Form", 
                  "Growth Form Diversity", "Dispersal Mode") #Add biome to this list?
colnames(out.df) <- column.names
x = 1
for (i in species.in.BIEN){
    print(x)
    # For each species, find the max and min of each trait of interest
    active.subset <- subset(species.traits, scrubbed_species_binomial==i)
    # does growth form need to have separate values for the other trait ranges? 
    # i.e. something as a shrub has some height values, where that thing as a tree has another range
    # this does that. ^^ if it shouldn't, move everything but the growth form variable out of this for loop
    # growth form will need to be looked into a little bit more (herb_tree? really??)
    # print(active.subset$trait_value==character(0)[active.subset$trait_name=="whole plant dispersal syndrome"])
    # (i, phmin, phmax, smmin, smmax, gf, gfd, ds)
    if (length(active.subset$trait_name[active.subset$trait_name == "whole plant growth form"])>0){
        gf <- active.subset$trait_value[active.subset$trait_name == "whole plant growth form"]
    }
    else {
        gf <- "none"
    }
    if (length(active.subset$trait_name[active.subset$trait_name == "whole plant growth form diversity"])>0){
        gfd <- active.subset$trait_value[active.subset$trait_name == "whole plant growth form diversity"]
    }
    else {
        gfd <- "none"
    }
    if (length(active.subset$trait_name[active.subset$trait_name == "whole plant dispersal syndrome"])>0){
        wpds <- active.subset$trait_value[active.subset$trait_name == "whole plant dispersal syndrome"]
    }
    else{
        wpds <- "none"
    }
    # for (j in gf){
    #     for (l in gfd){
    #     # # same goes for dispersal syndrome (we will see if it matters in the output) -- skipping blank dispersal syndromes
    #         for (k in wpds){
    ph <- active.subset$trait_value[active.subset$trait_name=="whole plant height"]
    phmin <- min(ph)
    phmax <- max(ph)
    sm <- active.subset$trait_value[active.subset$trait_name=="seed mass"]
    smmin <- min(sm)
    smmax <- max(sm)
    data.out <- c(i, phmin, phmax, smmin, smmax, gf, gfd, wpds)
    data.out[is.na(data.out)] <- "none"
    #out.df[is.na(out.df)] <- "none"
    out.df[x, ] <- data.out
    # if you don't clear the system memory between runs this list will get really long and give you weird results
    x = x + 1        
        
    
}

# export species traits as a csv
write.csv(out.df, "C:/Users/Eric/Documents/Plant_Extinction/traits_by_species_NW.csv")
write.csv(out.df, "/home/eric/Documents/Projects/C_Working/CC_Extinction/traits_by_species.csv")
out.df <- read.csv("/home/eric/Documents/Projects/C_Working/CC_Extinction/traits_by_species.csv")

# adding biomes to trait data
species.trait.biome <- merge(out.df, so.unique, by.x = "Species", by.y = "species", all.x=TRUE)
# biome names copied from the layer labels in the WWF shapefile
biome.number <- read.csv("/home/eric/Documents/Projects/C_Working/CC_Extinction/Biome_Names.csv")
species.trait.biome.name <- merge(species.trait.biome, biome.number, by.x = "biome", by.y = "Biome_No", all.x=TRUE)
write.csv(species.trait.biome.name, "/home/eric/Documents/Projects/C_Working/CC_Extinction/species_trait_biome.csv")
