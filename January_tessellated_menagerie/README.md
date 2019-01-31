# How to make

## Triangulation and voronoi tessellation
Step 1: get an animal (or other) silhouette as png or jpg

Step 2: use the script get_edge_points.R to get the edge points

Step 3: use the shiny app select_points_app.R to interactively select points for the border

Step 4: use triangulate_image.R or voronoi_tessellate_image.R to create a tessellation

## Hexagon tessellation and circlepacking
Step 1: get an animal (or other) silhouette as png or jpg

Step 2: follow the script hexbin_image.R or circlepack_image.R to make a tessellation

## Animation
Step 1: make any combination of triangle, voronoi, or circlepack tessellations

Step 2: make sure to export the polygon dataframes for your tessellations as RDS files

Step 3: Use the animate_animals.R script to make an animation (you will need to customize it for your needs)