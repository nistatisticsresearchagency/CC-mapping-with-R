
# Mapping with R
# v5
# 07/05/2024


## Data, Packages and Parameters ##

# Load the data

# Use Windows Explorer to locate the downloaded file 
  # '2024-05-07 coffee and coding - mapping with R.zip'
  # un-zip it to a location you can find e.g. Documents or Downloads
  # r-click on "2024-05-07 coffee and coding - mapping with R.Rproj" -> open with -> R Studio
    # if you get an 'access denied' error, try opening the "Mapping with R - code.R" file instead

# Check the top-right R Studio "Environment" pane
  # if it contains 8 datasets, great, the data is loaded
  # if it is empty then you will need to load the data

load(".RData")

# There should now be 8 datasets in the "Environment" pane of R Studio (top-right)
  # if you get a 'No such file or directory error'...
    # try to locate ".Rdata" in the RStudio lower-right pane
    # you may need to check where you downloaded the files to
    # once located, click ".Rdata" and then click 'Yes' to confirm
    # if it works you will see 8 data frames appear in the upper-right pane


# Install packages
  # Note: You might already have these packages installed
    # if so, they won't be re-installed
MappingwithR_packages = c("sf", "tmap", "dplyr", "RColorBrewer")
Packages_to_install = MappingwithR_packages[!(MappingwithR_packages %in% installed.packages()[,"Package"])]
if(length(MappingwithR_packages)>0) install.packages(Packages_to_install)
rm(MappingwithR_packages)
rm(Packages_to_install)


# load packages
library(sf)           # for mapping (sf stands for 'Simple Feature' which is a standard for representing spatial data)
library(tmap)         # for thematic mapping i.e. visualising spatial data distributions, in the style of ggplot2
library(dplyr)        # for manipulating data
library(RColorBrewer) # for colour palettes


# Set R Studio and map parameters

# I find the web browser is better for viewing maps than R Studio's viewer
options(viewer = NULL) # tells R Studio not to open the map

# change a parameter to avoid an error in Part 4 - "Error: Shape contains invalid polygons."
sf_use_s2(FALSE)



## PART 1 - Mapping Coordinate Data ##

# For this example we will use a dataset showing the locations of trees in Belfast.

# inspect the data
View(data_trees)
# 37,557 rows; 1 row per tree 🌲🌳🌴🎄

# Currently this object is a data frame.
# In order to map the data it contains we need to tell R about the coordinates: which variables and which projection.
# We have a choice of coordinates: TREELOCATIONX/TREELOCATIONY, also LONGITUDE/LATITUDE.
data_trees <- data_trees %>%
  st_as_sf(coords = c("TREELOCATIONX", "TREELOCATIONY"), crs = 29903)
# this step did two distinct actions:
  # 1. it told R that the variables containing the coordinates are TREELOCATIONX and TREELOCATIONY
  # 2. it also told R that the coordinate reference system (CRS) is Irish Grid (29903)

# inspect again
View(data_trees)
# notice that we have the same number of rows as before
# but now each row has a geometry column, each containing POINT(x,y)

# NB: we now have a simple feature data frame
  # I have noticed some odd behaviour when analysing this data frame 
  # e.g. using dplyr's group_by
  # If you plan to do more analysis after mapping the data, I recommend having two data frames:
    # one containing just the data i.e. a data frame
    # and a second containing the data + geometries i.e. the sf data frame


# Tree Map Version 1
# create a very simple map to get us started
map_trees_1 = tm_shape(data_trees) + tm_dots()

# view the map
map_trees_1
# this is a static image
# we can see the outline of Belfast, and certain roads
# let's improve this map

# Tree Map Version 2
  # resize dots
  # colour by species
  # edit pop-up content
  # change the background (called the basemap)
  # change initial zoom to Botanic Gardens

# set some parameters 
# default plot mode in tmap was static
# let's change it to interactive
  # better for exploratory data analysis
  # you can pan, zoom and see underlying features on map

tmap_mode("view") # changes tmap mode from static ("plot") to interactive ("view")

# make this change too
tmap_options(max.categories = 53)
# because there are more categories of tree than tmap's default settings like to see 

# create the map
map_trees_2 = tm_shape(data_trees) + 
  tm_dots(size = 0.1,
          col = "SPECIESTYPE",
          alpha = 0.75,
          id = "SPECIESTYPE",
          popup.vars=c("TYPEOFTREE", "SPECIESTYPE", "SPECIES", 
                       "AGE", "DESCRIPTION", "TREESURROUND", "VIGOUR", "CONDITION",
                       "TREEHEIGHTinMETRES", "SPREADRADIUSinMETRES", "DIAMETERinCENTIMETRES")) +
  tm_basemap(server = "OpenStreetMap.Mapnik") +
  tm_basemap(server = "OpenTopoMap", alpha = 0.5) +
  tm_basemap(server = "Stadia.AlidadeSmoothDark") +
  tm_basemap(server = "Esri.WorldImagery") +
  tm_view(set.view = c(-5.9337813, 54.582652, 17))

# launch the map
map_trees_2

# End of Part 1.



## PART 2 - Mapping Postcode Data ##

# In this section we will use two datasets:
  # 1. A sample of data from the Central Postcode Directory (CPD)
  # 2. Northern Ireland schools data (from DE)

# inspect the CPD data
View(data_CPD)
# 1,038 rows; 1 row per postcode
# key variables for us today: 
  # PC5 contains the postcodes in BT11AA format i.e. no space and no zero-padding
  # X contains Irish Grid Easting
  # Y contains Irish Grid Northing

# inspect the schools data
View(data_schools)
# 1,135 rows; 1 row per school
  # we have a postcode variable 
  # but no coordinates

# In order to plot the schools dataset we need coordinates
  # we join it to the CPD, using postcode
  # and then append the coordinates from the CPD
data_schools_CPD = data_schools %>% 
  left_join(data_CPD, by = join_by(Postcode_Clean == PC5)) %>% 
  filter(X != 666666 & Y != 666666 & is.na(X) == FALSE & is.na(Y) == FALSE)
# note: we also filtered out schools with invalid coordinates

# inspect
View(data_schools_CPD)
# 1125 rows
# our schools table now contains X and Y variables
  # which we can use to map the data

# before we can map the data, we need to add geometry
  # we tell R which variables contain the coordinates
  # and which coordinate reference system to use (Irish Grid)
data_schools_CPD <- data_schools_CPD %>%
  st_as_sf(coords = c("X", "Y"), crs = 29903)
# recall: 
  # this tells R which variables contain the coordinates
  # and that they are Irish Grid coordinates (crs = 29903)

# inspect
View(data_schools_CPD)
# we still have 1125 rows
  # each row has a geometry column, containing POINT(x,y)

# Create a map showing locations of schools in NI
  # enabled "clustering" so that schools with same postcode can be distinguished from each other
  # changed basemap to better show buildings, including schools
  # updated the pop-up to contain only variables of interest
map_schools = tm_shape(data_schools_CPD) + 
  tm_dots(col = "Type", 
          size = 0.1,
          clustering = TRUE,
          id = "Institution Name",
          popup.vars=c("Institution Name", "Type",
                       "Address Line 1", "Address Line 2", "Address Line 3", "Postcode")) +
  tm_basemap(server = "OpenStreetMap.Mapnik")

# launch the map
map_schools

# NB: Limitations:
#  the coordinates are only approximate because we assigned them from postcodes
  # and postcode coordinates in the CPD are the average coordinates of the properties with that postcode
  # e.g. show Steeple Road in Antrim
  map_schools + tm_view(set.view = c(-6.21025, 54.72095, 16))
  # for more information about coordinates from postcodes 
  # see https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/CPD-Guidance-Jan-2022.pdf#page=5


# End of Part 2.

  

## PART 3 - Mapping UPRN Data ##

# Although postcodes are more common, sometimes we are lucky enough to have UPRN data
  # UPRN stands for Unique Property Reference Number
  # every property in Northern Ireland has one
  # and UPRNs come with accurate coordinates
  
# inspect the Pointer data
  # note: this is only a sample, the full Pointer dataset has approx. 1 million rows
View(data_PTR)
# 349 rows
  # 29 variables inc. UPRN, X_COR, Y_COR
  # specifically properties in Coleraine: Captain Street Lower and Upper
  
# For this example we will use a fictional dataset
  # Imagine we are developing a household survey and are testing two possible questionnaires, call them A & B
  # We are going to use questionnaire A on 50 households, and questionnaire B on 50 households
  # So we have a dataset containing two variables: UPRN, QUESTIONNAIRE
  # inspect our UPRN-level made-up dataset:
View(data_survey)
# 100 UPRNs
  # each labelled A or B 
  # depending on which questionnaire that household received
  # but not coordinates

# Now we have two datasets: 
  # one for our made-up survey, and 
  # one containing a smaller version of Pointer
# Join our survey dataset to Pointer
  # and append the coordinates onto our dataset
data_survey_PTR = data_survey %>% 
  left_join(data_PTR, by = join_by(UPRN == UPRN)) %>% 
  select(UPRN, QUESTIONNAIRE, X_COR, Y_COR, BUILDING_NUMBER, PRIMARY_THORFARE, POSTCODE, CLASSIFICATION)
  
# inspect our data after joining to Pointer
View(data_survey_PTR)
# we have appended coordinates (and some other useful columns from Pointer)
  
# convert our dataset to sf format i.e. add geometry
  # X_COR and Y_COR in Pointer are Irish Grid Eastings and Northings
  # the Coordinate Reference System code for Irish Grid is 29903 
data_survey_PTR <- data_survey_PTR %>%
  st_as_sf(coords = c("X_COR", "Y_COR"), crs = 29903)
# recall: 
# this tells R which variables contain the coordinates
# and that they are Irish Grid coordinates (crs = 29903)
  
# Create our map
map_survey = tm_shape(data_survey_PTR) + 
  tm_dots(col = "QUESTIONNAIRE", 
          size = 0.1,
          id = "UPRN",
          popup.vars = c("UPRN", "QUESTIONNAIRE", "CLASSIFICATION", "BUILDING_NUMBER", "PRIMARY_THORFARE", "POSTCODE"),
          popup.format=list("UPRN" = list(fun=function(x) formatC(x, digits=0, format="d")))) + 
  tm_basemap(server = "OpenStreetMap.Mapnik", alpha = 0.5)
  
# launch the map
map_survey
  
# Note:
  # geographic area = part of Coleraine
  # points = households
  # legend indicates which questionnaire was received (different colours)
  # hover over points for UPRN, click for more information
  # **Note the more accurate coordinates i.e. specific buildings**
    # This is more accurate than coordinates from postcodes
  

# End of Part 3.



## PART 4 - Mapping Boundary Data ##

# For this example we will use the council boundaries

# create a map showing the 26 council boundaries
map_LGD1993 = tm_shape(data_LGD1993_boundary) + tm_polygons(col = NA, alpha = 0, border.col = "blue", border.alpha = 0.75)

# create a map showing the 11 council boundaries
map_LGD2012 = tm_shape(data_LGD2012_boundary) + tm_polygons(col = NA, alpha = 0, border.col = "red", border.alpha = 0.75)

# combine the maps
map_LGDs = map_LGD1993 + map_LGD2012

# launch the map
map_LGDs


# Extra: we can add boundaries to an earlier map e.g. schools
map_schools_LGDs = map_schools + map_LGD2012

# launch the map
map_schools_LGDs


# Extra: choropleth maps 
  # e.g. colour the council areas based on their population
  # we will use population data for 2022

# inspect the population data
View(data_population_LGD_2022)

# inspect the boundary data
View(data_LGD2012_boundary)

# join the 11 council boundaries with the population data
data_LGD2012_boundary_pop2022 = left_join(data_LGD2012_boundary, data_population_LGD_2022, by = join_by(LGDCode == area_code))


# choose a colour palette
display.brewer.all()
# I like "Greens" for this map

# create a choropleth map showing NI population in 2022 by 11 councils
map_pop2022 = tm_shape(data_LGD2012_boundary_pop2022) + 
  tm_polygons(col = "MYE", 
              alpha = 0.75, 
              breaks = c(100000, 150000, 200000, 250000, 300000, 350000),
              palette = brewer.pal(5, "Greens")
              )

# launch the map
map_pop2022

# these maps also work quite well as plots / static maps
tmap_mode("plot") 
# this switches tmap back to 'static' mode rather than 'interactive'

# view our last map as a static map
map_pop2022

# view the previous map with some extra features
# I have added a compass and a scale bar
map_LGDs + 
  tm_compass(type = "rose", size = 4, position = c("left","top")) +
  tm_scale_bar(position = c("left","bottom"))
# This would benefit from boundaries for the loughs and Ireland.


# End of Part 4.



## Links:

# Additional background maps (browse)
  # https://leaflet-extras.github.io/leaflet-providers/preview/index.html

# Colour schemes (browse)
  # https://colorbrewer2.org/
  
# Finding Coordinate Reference System codes
  # https://epsg.io/
  
# More on tmap
  # https://r-tmap.github.io/tmap-book/nutshell.html
  # https://book.rfortherestofus.com/maps
  # https://r.geocompx.org/adv-map

# More on Simple Features
  # https://r-spatial.github.io/sf/articles/sf1.html

# Data sources:
  # Belfast trees data https://admin.opendatani.gov.uk/dataset/belfast-trees
  # CPD https://www.nisra.gov.uk/support/geography/central-postcode-directory
  # NI schools https://www.education-ni.gov.uk/services/schools-plus
  # Pointer (sample) https://www.nidirect.gov.uk/publications/pointer-sample-data
  # Boundary for 26 councils https://admin.opendatani.gov.uk/dataset/osni-open-data-50k-boundaries-local-government-districts-1993
  # Boundary for 11 councils https://admin.opendatani.gov.uk/dataset/osni-open-data-largescale-boundaries-local-government-districts-2012
  # Mid Year Population Estimates https://www.nisra.gov.uk/publications/2022-mid-year-population-estimates-northern-ireland

