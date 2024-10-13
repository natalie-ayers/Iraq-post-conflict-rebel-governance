# Post-Conflict Impacts of Rebel Governance in Iraq


## Data Sources

### ISIS Territorial Control

#### IOM

Territorial control at the monthly level as recorded by IOM surveys; manually corrected and confirmed to get start and end date of occupation for each area by Blair and Nihad. 


#### Liveuamap

ISIS control maps and recorded incidents downloaded as KMLs for the first day of each month from Oct 1, 2014 to Dec 1, 2024 (geoJSON downloads didn't include correct labels for the polygons of control). ArcGIS Pro was used to import each KML, combine the polygons and points into two separate layers, and export both as shapefiles, producing `liveuamap_polygon_merge_2014_2023.shp` and `liveuamap_points_merge_2014_2023.shp`. 

These shapefiles were then processed in Python and R. 