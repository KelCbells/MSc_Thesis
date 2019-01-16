#a16     F:\WC\WC_RF_inputs\3m
LSDM = "wc_a16_3m.tif"
TPI = "wc_3p.tif"
Elevation = "wc_3b.tif"
Aspect = "wc_3f.tif"
Slope = "wc_3c.tif"
Height = "wc_3h.tif"
Cover = "wc_3q.tif"
Wind = "Wind.tif"
TPI_qc = "F:/CSRS/Predictors/a16/TPI.tif"
Elevation_qc = "F:/CSRS/Predictors/a16/Elevation.tif"
Aspect_qc = "F:/CSRS/Predictors/a16/Aspect.tif"
Slope_qc = "F:/CSRS/Predictors/a16/Slope.tif"
Height_qc = "F:/CSRS/Predictors/a16/Height.tif"
Cover_qc = "F:/CSRS/Predictors/a16/Cover.tif"
Wind_qc = "F:/CSRS/Predictors/a16/Wind.tif"

mypath = "F:/CSRS/Predictors/a16/"
if not os.path.isdir(mypath):
    os.makedirs(mypath)

arcpy.env.extent = "F:/CSRS/LSDMs/wc_a16_3m.tif"
arcpy.env.snapRaster = "F:/CSRS/LSDMs/wc_a16_3m.tif"
arcpy.env.workspace = r"F:/CSRS/Predictors/a16/"
arcpy.gp.ExtractByMask_sa(TPI, LSDM, TPI_qc)
arcpy.gp.ExtractByMask_sa(Elevation, LSDM, Elevation_qc)
arcpy.gp.ExtractByMask_sa(Aspect, LSDM, Aspect_qc)
arcpy.gp.ExtractByMask_sa(Slope, LSDM, Slope_qc)
arcpy.gp.ExtractByMask_sa(Height, LSDM, Height_qc)
arcpy.gp.ExtractByMask_sa(Cover, LSDM, Cover_qc)
arcpy.gp.ExtractByMask_sa(Wind, LSDM, Wind_qc)


#f17     F:\WC\WC_RF_inputs\3m
LSDM = "wc_f17_3m.tif"
TPI = "wc_3p.tif"
Elevation = "wc_3b.tif"
Aspect = "wc_3f.tif"
Slope = "wc_3c.tif"
Height = "wc_3h.tif"
Cover = "wc_3q.tif"
TPI_qc = "F:/CSRS/Predictors/f17/TPI.tif"
Elevation_qc = "F:/CSRS/Predictors/f17/Elevation.tif"
Aspect_qc = "F:/CSRS/Predictors/f17/Aspect.tif"
Slope_qc = "F:/CSRS/Predictors/f17/Slope.tif"
Height_qc = "F:/CSRS/Predictors/f17/Height.tif"
Cover_qc = "F:/CSRS/Predictors/f17/Cover.tif"
arcpy.gp.ExtractByMask_sa(Wind, LSDM, Wind_qc)

mypath = "F:/CSRS/Predictors/f17/"
if not os.path.isdir(mypath):
    os.makedirs(mypath)

arcpy.env.extent = "F:/CSRS/LSDMs/wc_f17_3m.tif"
arcpy.env.snapRaster = "F:/CSRS/LSDMs/wc_f17_3m.tif"
arcpy.env.workspace = r"F:/CSRS/Predictors/f17/"
arcpy.gp.ExtractByMask_sa(TPI, LSDM, TPI_qc)
arcpy.gp.ExtractByMask_sa(Elevation, LSDM, Elevation_qc)
arcpy.gp.ExtractByMask_sa(Aspect, LSDM, Aspect_qc)
arcpy.gp.ExtractByMask_sa(Slope, LSDM, Slope_qc)
arcpy.gp.ExtractByMask_sa(Height, LSDM, Height_qc)
arcpy.gp.ExtractByMask_sa(Cover, LSDM, Cover_qc)
