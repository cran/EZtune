#' Mullein data from Lava Beds National Monument - test dataset
#'
#' This dataset contains information about the presence and absence of
#' common mullein (Verbascum thapsus) at 1,512 randomly selected sites
#' in Lava Beds National Monument. The data were collected in summer 2006.
#' This dataset may be used to evaluate predictive statistical procedures
#' that have been fit on the mullein dataset.
#'
#' @format A data frame with 1512 observations and 32 variables. One variable
#' identifies the presence or absence of mullein in a 30m by 30m site
#' and 31 variables are characteristics of the site where the data were
#' collected.
#'
#' In the original data there were 12 monthly values for each of the
#' bioclimatic predictors. Principal components analyses suggested
#' that for each of these predictors 2 principal components explained
#' the vast majority (95.0\%-99.5\%) of the total variability. Based on
#' these analyses, indices were created for each set of bioclimatic predictors.
#' The variables with the suffix Ave in the variable name are the average
#' of 12 monthly variables. The variables with the suffix Diff are contrasts
#' between the sum of the April-September monthly values and the sum of the
#' October-December and January-March monthly values, divided by 12.
#' Roughly speaking, these are summer-to-winter contrasts.
#'
#' The variables are summarized as follows:
#' \describe{
#' \item{VerbThap}{Presence or absence of Verbascum thapsus, common mullein, (Absent = 0, Present = 1)}
#' \item{DegreeDays}{Degree days in degrees Celsius}
#' \item{EvapoTransAve}{Average monthly potential evapotranspiration in mm}
#' \item{EvapoTransDiff}{Summer-to-winter difference in monthly potential evapotranspiration in mm}
#' \item{MoistIndAve}{Average monthly moisture index in cm}
#' \item{MoistIndDiff}{Summer-to-winter difference in monthly moisture index in cm}
#' \item{PrecipAve}{Average monthly precipitation in cm}
#' \item{PrecipDiff}{Summer-to-winter difference in monthly precipitation in cm}
#' \item{RelHumidAve}{Average monthly relative humidity in percent}
#' \item{RelHumidDiff}{Summer-to-winter difference in monthly relative humidity in percent}
#' \item{PotGlobRadAve}{Average monthly potential global radiation in kJ}
#' \item{PotGlobRadDiff}{Summer-to-winter difference in monthly potential global radiation in kJ}
#' \item{AveTempAve}{Average monthly average temperature in degrees Celsius}
#' \item{AveTempDiff}{Summer-to-winter difference in monthly average temperature in degrees Celsius}
#' \item{MinTempAve}{Average monthly minimum temperature in degrees Celsius}
#' \item{MinTempDiff}{Summer-to-winter difference in monthly minimum temperature in degrees Celsius}
#' \item{MaxTempAve}{Average monthly maximum temperature in degrees Celsius}
#' \item{MaxTempDiff}{Summer-to-winter difference in monthly maximum temperature in degrees Celsius}
#' \item{DayTempAve}{Mean average daytime temperature in degrees Celsius}
#' \item{DayTempDiff}{Summer-to-winter difference in average daytime temperature in degrees Celsius}
#' \item{AmbVapPressAve}{Average monthly average ambient vapor pressure in Pa}
#' \item{AmbVapPressDiff}{Summer-to-winter difference in monthly average ambient vapor pressure in Pa}
#' \item{SatVapPressAve}{Average monthly average saturated vapor pressure in Pa}
#' \item{SatVapPressDiff}{Summer-to-winter difference in monthly average saturated vapor pressure in Pa}
#' \item{VapPressDefAve}{Average monthly average vapor pressure deficit in Pa}
#' \item{VapPressDefDiff}{Summer-to-winter difference in monthly average vapor pressure deficit in Pa}
#' \item{Elevation}{Elevation in meters}
#' \item{Slope}{Percent slope}
#' \item{TransAspect}{Transformed Aspect: TransAspect=(1-cos(Aspect))/2}
#' \item{DistRoad}{Distance to the nearest road in meters}
#' \item{DistTrail}{Distance to the nearest trail in meters}
#' \item{DistRoadTrail}{Distance to the nearest road or trail in meters}
#' }
#' @source Cutler, D. Richard., Thomas C. Edwards Jr., Karen H. Beard,
#' Adele Cutler, Kyle T. Hess, Jacob Gibson, and Joshua J. Lawler. 2007.
#' Random Forests for Classification in Ecology. Ecology 88(11): 2783-2792.
"mulleinTest"
