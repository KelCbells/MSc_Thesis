import arcpy
from arcpy import env

#LSDM
out_folder_path = "E://WCW//80k//f17//Predictions//a//Mosaics"
out_name = "LSDM"
arcpy.CreateFolder_management(out_folder_path, out_name)

#elevation
out_folder_path = "E://WCW//80k//f17//Predictions//a//Mosaics//LSDM"
out_name = "elev_b"
arcpy.CreateFolder_management(out_folder_path, out_name)


#aspect
out_folder_path = "E://WCW//80k//f17//Predictions//a//Mosaics//LSDM"
out_name = "aspect_f"
arcpy.CreateFolder_management(out_folder_path, out_name)


#slope
out_folder_path = "E://WCW//80k//f17//Predictions//a//Mosaics//LSDM"
out_name = "slope_d"
arcpy.CreateFolder_management(out_folder_path, out_name)

#height
out_folder_path = "E://WCW//80k//f17//Predictions//a//Mosaics//LSDM"
out_name = "height_h"
arcpy.CreateFolder_management(out_folder_path, out_name)


#slope
out_folder_path = "E://WCW//80k//f17//Predictions//a//Mosaics//LSDM"
out_name = "cover_q"
arcpy.CreateFolder_management(out_folder_path, out_name)


#tpi
out_folder_path = "E://WCW//80k//f17//Predictions//a//Mosaics//LSDM"
out_name = "tpi_p"
arcpy.CreateFolder_management(out_folder_path, out_name)


#wind
out_folder_path = "E://WCW//80k//f17//Predictions//a//Mosaics//LSDM"
out_name = "wind_w"
arcpy.CreateFolder_management(out_folder_path, out_name)

