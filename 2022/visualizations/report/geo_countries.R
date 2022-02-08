library(rgdal) # for readOGR function
library(rmapshaper) # for ms_simplify function

## This script only needs to be run once

## From https://datahub.io/core/geo-countries#r
download.file("https://datahub.io/core/geo-countries/r/countries.geojson",
              destfile = "countries.geojson")

## readOGR is a common function to read geojsons, could also use geojsonio's
world_geojson = readOGR("countries.geojson") 

#ms_simplify reduce size of the file from 16MB to 2MB!
world_geojson = ms_simplify(world_geojson,
                            sys = TRUE, # uses system's mapshaper installation
                            ## which speeds up process
                            keep_shapes = TRUE) ## keeps small shapes that
## would otherwise be lost
saveRDS(world_geojson, "countries_map.rds")

## test map
agg = world_geojson
cumulative = readRDS("ProcessedData/cumulative.rds")
agg@data = right_join(agg@data,
                      cumulative %>% group_by(location, iso_code) %>%
                        summarise(total_cases = total_cases[date==max(date)]),
                      by = c('ISO_A3' = 'iso_code'))

agg@polygons = agg@polygons[world_geojson@data$ISO_A3 %in% cumulative$iso_code]

# Example map
pal <- colorNumeric("Blues", NULL)
leaflet(agg,
        width = 1040,
        height = 800,
        options = leafletOptions(center = c(30,0),
                                 zoom=2,
                                 maxBounds = list(c(-90, -180),
                                                  c(90,180))))%>%
  addTiles() %>%
  addPolygons(stroke = FALSE,
              smoothFactor = 0.3,
              fillOpacity = 1,
              fillColor = ~pal(log10(total_cases)),
              label = ~paste0(location, ": ",
                              ez_labels(total_cases, signif = 3))
  ) %>%
  addLegend(pal = pal,
            values = ~log10(total_cases),
            opacity = 1.0,
            labFormat = labelFormat(transform = function(x) round(10^x))
  )

system('rm countries.geojson')