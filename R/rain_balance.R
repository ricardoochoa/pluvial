#' rain_balance
#'
#' Estimates a balance of potential used rainwater, wasted rainwater and stored rainwater based on precipitation data, catchment_area, water_demand, tank_capacity, tank_initial_volume, loss_factor.
#' @param precipitation Precipitation data in millimeters (vector)
#' @param catchment_area Catchment area in square meters
#' @param water_demand Water demand in liters (vector of the same length as precipitation)
#' @param tank_capacity Maximum tank volume capacity in liters
#' @param tank_initial_volume Tank water volume in liters at the begining of the period
#' @param loss_factor Fraction (numeric value ranging from 0 to 1) of the rainwater representing the percentage of water that gets wasted in the harvesting process
#' @keywords rainwater harvesting, water balance
#' @export
#' @examples
#' rain_balance(precipitation = c(1, 2, 3),
#'              catchment_area = 100,
#'              water_demand = c(100, 0, 100),
#'              tank_capacity = 450,
#'              tank_initial_volume = 0,
#'              loss_factor = 0.75)

rain_balance <- function(precipitation,
                         catchment_area,
                         water_demand,
                         tank_capacity,
                         tank_initial_volume = 0,
                         loss_factor){

  # calculate harvested rainwater
  supply = catchment_area * (100) *   # convert m2 to dm2
           precipitation  * (1/100) * # convert mm to dm
           (1-loss_factor)

  # length
  data_length <- length(precipitation)

  # Inicialize variables
  used_rainwater <- rep(0, data_length)
  wasted_water   <- rep(0, data_length)
  tank_volume     <- rep(0, data_length)

  # All the rest
  for(c in 1:data_length){
    used_rainwater[c] <-  min(tank_capacity,
                              water_demand[c],
                              ifelse(c==1, tank_initial_volume, tank_volume[c-1]) +
                              supply[c])

    wasted_water[c] <-    max(0,
                              ifelse(c==1, tank_initial_volume, tank_volume[c-1]) +
                              supply[c] -
                              used_rainwater[c] -
                              tank_capacity)

    tank_volume[c] <-      max(0,
                              ifelse(c==1, tank_initial_volume, tank_volume[c-1]) +
                              supply[c] -
                              used_rainwater[c] -
                              wasted_water[c])
  }

return(data.frame(precipitation  = precipitation,
                  water_demand   = water_demand,
                  used_rainwater = used_rainwater,
                  wasted_water   = wasted_water,
                  tank_volume     = tank_volume
                  ))
  }
