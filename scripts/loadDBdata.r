###################################################################################
###################################################################################
###################################################################################
#
# This will load database data from the file DBdata.RData. Put the data file in the
# working directory, then run the line at the bottom of the file.
#
###################################################################################
###################################################################################
###################################################################################
#
# The data is stored in the following objects:
# --- timeseries
#	The time series data is a matrix object with the following headers/columns:
#	(1) assessid (2) stockid (3) stocklong (4) tsid (5) tsyear (6) tsvalue
# --- bioparams
#	The time series data is a matrix object with the following headers/columns:
#	(1) assessid (2) stockid (3) stocklong (4) bioid (5) biovalue (6) bioyear (7) bionotes
# --- timeseries.views.data
#	This stores the timeseries values with timeseries type along the columns (TB, SSB, TN, R,
#	TC, TL, F, ER, B/Bmsy, SSB/SSBmsy, F/Fmsy, U/Umsy, Btouse, Ctouse, Utouse, B/Bmsytouse, U/Umsytouse) 
#	and stocks along the rows
# --- timeseries.views.units
#	This stores the timeseries units, with timeseries type along the columns (TB, SSB, TN, R,
#	TC, TL, F, ER) and stocks along the rows
# --- bioparams.views.data
#	This stores the bioparams values, with bioparam type along the columns
#	(Bmsy, SSBmsy, Nmsy, MSY, Fmsy, Umsy, B0, SSB0, M, Bmsytouse, Umsytouse) and stocks along the rows
# --- bioparams.views.units
#	This stores the bioparams units, with bioparam type along the columns
#	(Bmsy, SSBmsy, Nmsy, MSY, Fmsy, Umsy, B0, SSB0, M) and stocks along the rows 
# --- meta.data
#	This stores assorted metadata associated with the stock, with datatypes along the columns 
#	(assessid, stockid, stocklong, scientificname, FisheryType, region, areaid, areaname, 
#	assessorid, mgmt, management authority) and stock by row
#
###################################################################################
###################################################################################
###################################################################################
#
# Once the DBdata.RData file is in the working directory, simply run the following command to
# load up the database data into R objects


load("DBdata.RData")

