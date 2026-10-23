
### Mapping with R ###
# 22/09/2026
# NB: tested using R 4.6.1


### Data, Packages and Parameters ###

# Download the files that were emailed to you.
  # save them to a location you can find e.g. your Documents or Downloads folder
  # launch the R project file: '2026-09-22 coffee and coding - mapping with R.Rproj'


## Install packages
  # if you already have these packages they won't re-install
  # ⚠ if you don't yet have these, they can take 20-30 mins to install from scratch ⚠ 

MappingwithR_packages = c("sf", "tmap", "dplyr", "RColorBrewer", "kableExtra", "colorblindcheck", "plotly", "shinyjs", "cols4all", "leaflet.extras")
Packages_to_install = MappingwithR_packages[!(MappingwithR_packages %in% installed.packages()[,"Package"])]
if(length(MappingwithR_packages)>0) install.packages(Packages_to_install)
rm(MappingwithR_packages)
rm(Packages_to_install)


# Check your tmap version
  # The tmap package has had a major update in early 2025 from v3 to v4
  # This update required changes to be made to the coffee and coding script
  # It is important that you are version 4.x of tmap
  # This next line checks your tmap version and if it is earlier than v4 it instructs R to re-install the latest version

if(packageVersion("tmap") < package_version("4.1")) install.packages("tmap") else packageVersion("tmap")


## Load packages

library(sf)           # for mapping (sf stands for 'Simple Feature' which is a standard for representing spatial data)
library(tmap)         # for thematic mapping i.e. visualising spatial data distributions, in the style of ggplot2
library(dplyr)        # for manipulating data
library(cols4all)     # for colour palettes since tmap v4

# Don't worry about warning messages like "package ‘dplyr’ was built under R version 4.5.2"


## Load data

# There should be 8 datasets in the "Environment" pane of R Studio (top-right of screen)
# If that space is empty, run the next line to load the data

load(".RData")



## PART 1 - Mapping Coordinate Data ##

# For the first example we will use a dataset called data_trees.
# It contains the locations of trees in Belfast.

# inspect the data
View(data_trees)
# 37,557 rows; 1 row per tree 🌲🌳🌴🎄
# 18 variables

# Currently this object is a data frame.
  # Notice that it contains coordinates.
  # We can see that... but at the moment R does not know those are coordinates.
  # To map this data we need to tell R about the coordinates: 
    # specifically we want to tell R two things:
      # 1. which variables contain the coordinates
      # 2. which 'projection' the coordinates are in

# We even have a choice of coordinates: 
  # TREELOCATIONX/TREELOCATIONY 
  # also LONGITUDE/LATITUDE
  # aside: these are different types of coordinate system 
    # Irish Grid coordinates are distances from an origin on a grid
    # Latitude and longitude are angles on a sphere from equator and prime meridian

# We will use Irish Grid coordinates
  # this means we want the variables TREELOCATIONX and TREELOCATIONY
    # TREELOCATIONX is the Irish Grid "Easting"
    # TREELOCATIONY is the Irish Grid "Northing"
  # For Irish Grid, the coordinate reference system code is 29903

data_trees_sf <- data_trees %>%
  st_as_sf(coords = c("TREELOCATIONX", "TREELOCATIONY"), crs = 29903)
# this step did two actions for us:
  # 1. it told R that the variables containing the coordinates are TREELOCATIONX and TREELOCATIONY
  # 2. it also told R that the coordinate reference system (CRS) is Irish Grid (29903)

# inspect our new data frame
View(data_trees_sf)
# notice that we have the same number of rows as before (37557 rows)
# but the number of variables has reduced from 18 to 17
# and now each row has a geometry column: each row has a POINT(x,y)
# NB: we now have a "simple feature" data frame
  
# aside: I have seen odd behaviour when analysing this data frame e.g. using dplyr's group_by
# I recommend having two data frames:
  # one containing just the data i.e. a data frame (for analysis)
  # and a second containing the data + geometry i.e. the sf data frame (for mapping)


# Create a very simple map to get us started
# Map of trees, version 1
map_trees_1 = tm_shape(data_trees_sf) + tm_dots()

# view the map
map_trees_1
# this is a static image
# we can see the outline of Belfast, and certain roads
# but we can do much better! let's get to work with the tmap package

# Map of trees, version 2
  # resize the dots
  # colour by species
  # edit pop-up content
  # change the background (called the basemap)
  # change initial zoom (to Botanic Gardens)

# The default plot mode in tmap was 'static'
# this produced an image in the RStudio plots pane
# now let's change it to 'interactive'
  # this is better for exploratory data analysis
  # you can pan, zoom and see underlying features on map

tmap_mode("view") # changes tmap mode from static ("plot") to interactive ("view")

# create the new map
map_trees_2 = tm_shape(data_trees_sf) + 
  tm_symbols(size = 1,
          fill = "SPECIESTYPE_GROUPED",
          fill_alpha = 0.75,
          fill.scale = tm_scale_categorical(values = "light24"),
          id = "SPECIESTYPE_GROUPED",
          popup = tm_popup(vars = c("TYPEOFTREE", "SPECIESTYPE_GROUPED", "SPECIESTYPE", "SPECIES", 
                                    "AGE", "DESCRIPTION", "TREESURROUND", "VIGOUR", "CONDITION",
                                    "TREEHEIGHTinMETRES", "SPREADRADIUSinMETRES", "DIAMETERinCENTIMETRES"))
          ) +
  tm_basemap(server = "Stadia.AlidadeSmoothDark") +
  tm_basemap(server = "OpenStreetMap.Mapnik") +
  tm_basemap(server = "OpenTopoMap", alpha = 0.5) +
  tm_basemap(server = "Esri.WorldImagery") +
  tm_view(set_view = c(-5.9337813, 54.582652, 17))

# launch the map
map_trees_2
# note: R Studio launches the map in its Viewer pane
  # but full screen is much better
  # click 'show in new window' to launch the map in your web browser


# Summary of Part 1:
  # If you have coordinates...
    # 1. tell R which variables contain the coordinates and what type of coordinates they are e.g. Irish Grid
    # 2. create a "simple feature" data frame containing geometries
    # 3. plot your map

# End of Part 1.



## PART 2 - Mapping Postcode Data ##

# In this section we will use two datasets:
  # 1. A sample of data from the Central Postcode Directory (CPD)
  # 2. Northern Ireland schools data (from DE)

# inspect the CPD data
View(data_CPD)
# 1,038 rows; 1 row per postcode
# The key variables for us are: 
  # PC5 which contains the postcodes 
    # in the format BT11AA
    # i.e. no space and no zero-padding
  # X contains Irish Grid Easting
  # Y contains Irish Grid Northing

# inspect the schools data
View(data_schools)
# 1,135 rows; 1 row per school
  # unlike with the trees, we have no coordinates 👎 
  # but we do have a postcode variable 👍

# To map the schools dataset we need coordinates
  # we can join the schools data to the CPD, using the postcode variable
  # and then append the coordinates from the CPD
data_schools_CPD = data_schools %>% 
  left_join(data_CPD, by = join_by(Postcode_Clean == PC5)) %>% 
  filter(X != 666666 & Y != 666666 & is.na(X) == FALSE & is.na(Y) == FALSE)
# note an extra step: we also filtered out schools with invalid coordinates

# inspect
View(data_schools_CPD)
# 1125 rows
# our schools table now contains X and Y variables
  # which we can use to map the data

# Like we did with the trees dataset, we need to add geometry before we can map the data
  # so we tell R which variables contain the coordinates
  # and which coordinate reference system to use (Irish Grid again)
data_schools_CPD_sf <- data_schools_CPD %>%
  st_as_sf(coords = c("X", "Y"), crs = 29903)
# recall: 
  # this tells R which variables contain the coordinates: X and Y
  # and that they are Irish Grid coordinates (crs = 29903)

# inspect
View(data_schools_CPD_sf)
# we still have 1125 rows
  # each row has a geometry column, containing POINT(x,y)


# Create a map showing locations of schools in NI
  # some changes from our previous map of trees:
    # changed the size of the dots - a bit smaller than the trees
    # changed the colour palette to one suitable for 8 types of school
      # to choose a palette you can run cols4all::c4a_gui()
      # but you'll have to close the Shiny pop-up window before making more maps 
    # changed the basemap to better show buildings, including schools
    # updated the pop-up to contain only variables of interest
map_schools = tm_shape(data_schools_CPD_sf) + 
  tm_symbols(fill = "Type",
          fill_alpha = 0.75,
          fill.scale = tm_scale_categorical(values = "brewer.dark2"),
          size = 0.75,
          id = "Institution Name",
          popup = tm_popup(vars = c("Institution Name", "Type",
                                    "Address Line 1", "Address Line 2", "Address Line 3", 
                                    "Postcode"))
  ) +
  tm_basemap(server = "OpenStreetMap.Mapnik")

# launch the map
map_schools
# again you'll want to click "Show in new window" (beside the broom in the Viewer pane)

# NB: There is a significant limitation of this coordinates-from-postcodes approach.
  # The coordinates are only approximate
  # because we assigned them from postcodes using the CPD:
    # For each postcode on the POINTER dataset the average of the X and Y coordinates of constituent postcode properties is calculated. 
    # This is compared with the grid reference of the nearest property to the average 
    # and the grid reference of that property is assigned as the postcode grid-reference ‘centroid’.  
  # e.g. show Steeple Road in Antrim
  map_schools + tm_view(set_view = c(-6.21025, 54.72095, 16))
  # for more information about coordinates from postcodes 
  # see https://www.nisra.gov.uk/files/nisra/documents/2025-03/CPD-Guidance-Jan-2025.pdf#page=6
  # for a quick work-around to the issue of multiple schools at the same point
    # we can move all the points around by a small amount so that they don't overlap:
map_schools_jittered = tm_shape(data_schools_CPD_sf %>% st_jitter(factor = 0.00005)) + 
    tm_symbols(fill = "Type",
               fill_alpha = 0.75,
               fill.scale = tm_scale_categorical(values = "brewer.dark2"),
               size = 0.75,
               id = "Institution Name",
               popup = tm_popup(vars = c("Institution Name", "Type",
                                         "Address Line 1", "Address Line 2", "Address Line 3", 
                                         "Postcode"))
    ) +
    tm_basemap(server = "OpenStreetMap.Mapnik")

map_schools_jittered + tm_view(set_view = c(-6.21025, 54.72095, 20))
# now the 3 overlapping points from before can be seen side-by-side
# and hovered or clicked separately

# End of Part 2.




## PART 3 - Mapping UPRN Data ##

# Sometimes we are lucky enough to have UPRN data
  # UPRN stands for Unique Property Reference Number
  # every property in Northern Ireland has one
  # and UPRNs come with accurate coordinates.
  
# Inspect the Pointer data
  # Note: this is only a sample from Pointer
  # The full Pointer dataset has approx. 1 million rows.
View(data_PTR)
# 349 rows
  # 29 variables inc. UPRN, X_COR, Y_COR
  # specifically properties in Coleraine: Captain Street Lower and Upper
  
# For this example we will use a fictional dataset
  # Imagine we are developing a household survey and are testing two possible questionnaires, call them A & B
  # We are going to give questionnaire A to 50 households, and questionnaire B to 50 households.
  # So we have a made-up dataset containing two variables: UPRN, QUESTIONNAIRE
  # inspect our UPRN-level made-up dataset:
View(data_survey)
# 100 UPRNs
  # each labelled A or B 
  # depending on which questionnaire that household received
  # notice we don't have any coordinates 👎
  # but we do have UPRN 👍

# In this scenario we have two datasets: 
  # one for our made-up survey, and 
  # one containing a sample from Pointer

# We join our made-up survey dataset to Pointer
  # and append the coordinates onto our dataset
data_survey_PTR = data_survey %>% 
  left_join(data_PTR, by = join_by(UPRN == UPRN)) %>% 
  select(UPRN, QUESTIONNAIRE, X_COR, Y_COR, BUILDING_NUMBER, PRIMARY_THORFARE, POSTCODE, CLASSIFICATION)
  
# inspect our data after joining to Pointer
View(data_survey_PTR)
# we have appended coordinates (and some other useful columns from Pointer)
  
# next we convert our dataset to sf format i.e. add geometry
  # X_COR and Y_COR in Pointer are Irish Grid Eastings and Northings
  # so like we did with the trees and schools, we use Coordinate Reference System code 29903 
data_survey_PTR_sf <- data_survey_PTR %>%
  st_as_sf(coords = c("X_COR", "Y_COR"), crs = 29903)
# recall: 
  # this tells R which variables contain the coordinates i.e. X_COR and Y_COR
  # and that they are Irish Grid coordinates (crs = 29903)
  
# Create map
map_survey = tm_shape(data_survey_PTR_sf) + 
  tm_symbols(fill = "QUESTIONNAIRE",
          fill.scale = tm_scale_categorical(values = "brewer.accent"),
          size = 1,
          id = "UPRN",
          popup = tm_popup(vars = c("UPRN", "QUESTIONNAIRE", "CLASSIFICATION", "BUILDING_NUMBER", "PRIMARY_THORFARE", "POSTCODE"),
                           format = list("UPRN" = list(fun=function(x) formatC(x, digits=0, format="d")))
                           )
          )+
          tm_basemap(server = "OpenStreetMap.Mapnik", alpha = 0.5)


# launch the map
map_survey
# remember to "show in new window"
  
# Note:
  # geographic area = part of Coleraine
  # points = households
  # legend indicates which questionnaire was received (different colours)
  # hover over points for UPRN, click for more information
  # **Note the more accurate coordinates i.e. specific buildings**
    # This is much more accurate than coordinates from postcodes!
  

# End of Part 3.



## PART 4 - Mapping Boundary Data ##
# (if we have time)


# In Parts 1, 2 and 3 we have been working with coordinate data i.e. points.
# Another important type of data in maps is boundaries i.e. polygons or shapes.
# We include boundaries in our maps by sourcing boundary files, also known as shapefiles.
# These can come in different formats, for example .shp

# For this example we will use the council boundaries.
  # I downloaded the current 11 council boundaries and the previous 26 council boundaries
  # from the Open Data NI website
  # where they had been uploaded by Land and Property Services (LPS)

# In NISRA we are licensed to use these shapefiles under the NI Mapping Agreement (NIMA)

# Create a map showing the 26 council boundaries
map_LGD1993 = tm_shape(data_LGD1993_boundary) + 
  tm_polygons(fill = NA, 
              fill_alpha = 0, 
              col = "blue", 
              col_alpha = 0.75)

map_LGD1993

# Create a map showing the 11 council boundaries
map_LGD2012 = tm_shape(data_LGD2012_boundary) + 
  tm_polygons(fill = NA, 
              fill_alpha = 0, 
              col = "red", 
              col_alpha = 0.75)

map_LGD2012

# We can combine the maps
# this is useful to compare the boundaries
map_LGDs = map_LGD1993 + map_LGD2012

# launch the map
map_LGDs


# Extra #1: we can add these boundaries to an earlier map e.g. schools
  # this can be useful to provide context
map_schools_LGDs = map_schools + map_LGD2012

# launch the map
map_schools_LGDs


# Extra #2: choropleth maps 
  # i.e. colour the council areas based on some statistic
  # we will use population data for 2024

# inspect the population data
View(data_population_LGD_2024)

# inspect the boundary data
View(data_LGD2012_boundary)

# Join the 11 council boundaries with the population data
data_LGD2012_boundary_pop2024 = left_join(data_LGD2012_boundary, 
                                          data_population_LGD_2024, 
                                          by = join_by(LGDCode == area_code))

# Inspect
View(data_LGD2012_boundary_pop2024)
# We have the councils (codes + names)
  # and populations
  # and geometries, in this case POLYGONS rather than POINTS

# choose a colour palette
cols4all::c4a_gui()
# I like "brewer.greens" for this map
# but do take care with palettes - colouring particular LGDs green or orange can be problematic!
# remember to close the R Shiny cols4all pop-up when you're finished with it - it will prevent your maps from loading


# create a choropleth map showing NI population in 2024 by 11 councils
map_pop2024 = tm_shape(data_LGD2012_boundary_pop2024) + 
  tm_polygons(fill = "MYE", 
              fill_alpha = 0.75, 
              fill.scale = tm_scale(values = "brewer.greens", 
                                    breaks = c(100000, 150000, 200000, 250000, 300000, 350000, 400000)
                                    )
              )

# launch the map
map_pop2024

# these maps also work quite well as plots / static maps
tmap_mode("plot") 
# this switches tmap back to 'static' mode rather than 'interactive'

# view our last map as a static map
map_pop2024

# This is very basic and could be used for quick internal QA 
# but is definitely not publication quality.
# In my recent-ish experience, when I needed a static map for a publication
  # I used QGIS software (not in IT Assist Store but available on request)
  # which provides a point-and-click interface
  # and gives you fine control over labelling, copyright note, scale bar
  # part of the battle is also sourcing all the different boundaries e.g. loughs, RoI



# Extra #3: Adding a search box

# ⚠️NB: I've noticed this stops working with R 4.6.1 ⚠️
# I haven't found a solution yet, other than using R 4.5.1

# A colleague asked if it would be difficult to add a placename search box
# This isn't supported by the tmap package but the leaflet package can do this


#install.packages("leaflet")
#install.packages("leaflet.extras")

library(leaflet)
library(leaflet.extras)

# let's recreate our tree map with a search box
map_trees_2 %>% 
  tmap_leaflet() %>% 
  leaflet.extras::addSearchOSM()
# we now have a magnifying glass on the map
# the trick is to type your place name and wait a second for it to search
# then select it from the drop-down
# note: this is better with settlements, streets and buildings
  # but not great with postcodes
  # and doesn't know anything about our LGDs, wards, DZs etc

# let's try it again but with our schools map instead
map_schools %>% 
  tmap_leaflet() %>% 
  leaflet.extras::addSearchOSM()
# this code takes our existing map
  # converts it from tmap-format to leaflet-format (different mapping package)
  # then adds an Open Street Map (OSM) search box
  # there's also a Google Maps option but you would need to register for a Google API


# Extra #4: heatmaps

library(leaflet)

# tmap used to be able to do heatmaps easily
# but the new version 4 seems to have dropped this functionality!

# Let's make a quick heatmap of trees in Belfast using the leaflet package instead
# The data_trees table happens to have lat/long coordinates already

leaflet(data_trees) %>%
  addTiles() %>%
  addHeatmap(lng = ~LONGITUDE, 
             lat = ~LATITUDE, 
             intensity = 1,
             radius = 10, 
             blur = 20, 
             max = 1)

# and we can consult the help file and tinker with the parameters
# this works because the table contains lat/long coordinates
# but unfortunately this doesn't work with Irish Grid coordinates
# it's relatively simple to recode lat/long into Irish Grid
# it's too much information for this session 
# but get in touch if you're trying to do this as I have some code.


# End of Part 4.



## Links:

# NISRA-specific map template
  # RAP Skeleton 2.1 on NISRA's MS Teams page
  # https://nicsonline.sharepoint.com/sites/TM-DOF-NISRATEAM/SitePages/RAP-Skeleton.aspx
  # https://github.com/NISRA-Tech-Lab/rap-skeleton

# Additional background maps (browse)
  # https://leaflet-extras.github.io/leaflet-providers/preview/index.html

# Colour palettes
  # https://colorbrewer2.org/
  # This is a subset of the available colour schemes in tmap v4
  # run cols4all::c4a_gui() within your R session to see these plus other colour palettes

# Finding Coordinate Reference System codes
  # https://epsg.io/
  
# NISRA Boundaries Application on Spatial NI
  # This is useful for browsing boundary codes and names in your browser
  # and seeing how different boundaries overlap
  # https://apps.spatialni.gov.uk/NISRABoundariesApplication/

# More on tmap
  # https://github.com/r-tmap/tmap
  # https://r-tmap.github.io/tmap/index.html
  # https://r-tmap.github.io/tmap-book/index.html
  # https://r-tmap.github.io/tmap/articles/
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
  # Mid Year Population Estimates 2024 https://www.nisra.gov.uk/publications/2024-mid-year-population-estimates-northern-ireland-and-estimates-population-aged-85

# Adding a search box
  # https://forum.posit.co/t/how-to-add-search-box-on-tmap-leaflet-linked-to-google-street-map-using-r/67741/2

# Relevant UK context
  # GSS Geography Policy on Analysis Function: https://analysisfunction.civilservice.gov.uk/policy-store/gss-geography-policy/