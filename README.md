# Bike-Renting-Prediction
A model which can predict the bike rental count based on different environmental and seasonal settings

## Problem Description
Bike Renting is dependant on lot of things not just usage patterns but also on weather conditions. This is what the solution to this problem aims to achieve to find the factors affecting the bike renting.

## Problem Statement
The objective of this case is to predict the bike rental count on daily basis, based on usage statistics and different environmental and seasonal settings. The variables provided are as follows:

● instant: Record index
● dteday: Date
● season: Season (1:spring, 2:summer, 3:fall, 4:winter)
● yr: Year (0: 2011, 1:2012)
● mnth: Month (1 to 12)
● holiday: weather day is holiday or not (extracted fromHoliday Schedule)
● weekday: Day of the week
● workingday: If day is neither weekend nor holiday is 1, otherwise is 0.
● weathersit:
  1: Clear, Few clouds, Partly cloudy, Partly cloudy
  2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist
  3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered
  clouds
  4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog
● temp: Normalized temperature in Celsius. The values are derived via
  (t-t_min)/(t_max-t_min),
  t_min=-8, t_max=+39 (only in hourly scale)
  atemp: Normalized feeling temperature in Celsius. The values are derived via
  (t-t_min)/(t_maxt_min), 
  t_min=-16, t_max=+50 (only in hourly scale)
● hum: Normalized humidity. The values are divided to 100 (max)
● windspeed: Normalized wind speed. The values are divided to 67 (max)
● casual: count of casual users
● registered: count of registered users
● cnt: count of total bike rentals including both casual and registered

### Target Variable
To simplify the model creation we'll create different models for both "casual" and "registered" variables. Then add the best predicted values to get the total count for "cnt" variable.

### I have included the project report.
### Sonn I'll be uploading the HTML version for the same.
