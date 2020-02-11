#-----------------------------------------------------
# Randomly relocating polygons
# Base source: https://community.esri.com/thread/46497
#-----------------------------------------------------


import arcpy
from arcpy import da
import random
import math


workspace = r"Drive:\path"                                         # Data location
pa = workspace + '\\' + "name.shp"                                 # Shapefile - containing polygons need to be randomized
sr = arcpy.Describe(pa).spatialReference                           # Spatial reference
sa = workspace + '\\' + "name.shp"                                 # Shapefile - boundary of the targeted area

extent = arcpy.Describe(sa).extent                                 # Study area extent
sa_geom = [row[0] for row in arcpy.da.SearchCursor(sa,'SHAPE@',spatial_reference=sr)][0] # study area geometry
new_polys = []                                                     # A place holder


def rotate(poly,centroid): 
    new_array = arcpy.Array()                                      
    rnd_x = random.uniform(extent.XMin,extent.XMax)                # random x coordinate for new centroid
    rnd_y = random.uniform(extent.YMin,extent.YMax)                # random y coordinate for new centroid
    rnd_centroid = arcpy.Point(rnd_x,rnd_y)                        # random centroid point

    while True:
            try:
                for part in poly:                                  # for each polygon part
                    ang = 0                                        # No rotation
                
                    for pnt in part:                               # Apply for each vertex
                #print pnt
                        x_trans = pnt.X - centroid.X               # Normalize to zero
                        y_trans = pnt.Y - centroid.Y               # Normalize to zero
                        x_transprime = (math.cos(ang) * x_trans) - (math.sin(ang) * y_trans) # New x coord, from zero
                        y_transprime = (math.sin(ang) * x_trans) + (math.cos(ang) * y_trans) # New y coord, from zero
                        x_prime = x_transprime + rnd_centroid.X    # Move to new centroid x
                        y_prime = y_transprime + rnd_centroid.Y    # Move to new centroid y
                        new_array.add(arcpy.Point(x_prime, y_prime))                         # add to array

                temp_poly = arcpy.Polygon(new_array,sr)            # Make a temporary polygon for test

                if sa_geom.contains(temp_poly) is True:
                    print 'inside'
                    break
                
                else:
                    raise Exception                                # Except Exception:
                                                                   # Continue
             except: 
                print 'outside polygon, start over...'
                return rotate(poly,centroid)

            break

                
    good_poly = arcpy.Polygon(new_array,sr)                        # Make polygon from array

    return (good_poly)                                             # Return the final good polygon


print "start processing..."

with arcpy.da.SearchCursor(pa,'SHAPE@',spatial_reference=sr) as cursor:           
    for row in cursor:
        centroid = row[0].centroid                                 # Calculate centroid
        new_polys.append(rotate(row[0],centroid))                  # Add returned good polygon to list
arcpy.CopyFeatures_management(new_polys, workspace + '\\' + "GAP03r09.shp")                 # Write data to location


print "done"

#-------