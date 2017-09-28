---
title: "NOAA Storm database - Worst Weather Events"
author: "erickfis@gmail.com"
date: "27 de setembro de 2017"
output: ioslides_presentation
runtime: shiny
---



## NOAA Storm Database

The U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage.

The database currently contains data from January 1950 to January 2017, as entered by NOAA's National Weather Service (NWS).

Full study: https://erickfis.github.io/NOAA-Storm-Database/

## Economic losses




<!--html_preserve--><div class="shiny-input-panel">
<div class="shiny-flow-layout">
<div>
<div class="form-group shiny-input-container">
<label class="control-label" for="type">Type (economic losses: Property or Crops?)</label>
<div>
<select id="type"><option value="Property" selected>Property</option>
<option value="Crops">Crops</option></select>
<script type="application/json" data-for="type" data-nonempty="">{}</script>
</div>
</div>
</div>
</div>
</div><!--/html_preserve--><!--html_preserve--><div id="out0f79ba6195df9cf9" class="shiny-plot-output" style="width: 100% ; height: 400px"></div><!--/html_preserve-->

## Bullets

- Bullet 1
- Bullet 2
- Bullet 3

## R Output


```
##      speed           dist       
##  Min.   : 4.0   Min.   :  2.00  
##  1st Qu.:12.0   1st Qu.: 26.00  
##  Median :15.0   Median : 36.00  
##  Mean   :15.4   Mean   : 42.98  
##  3rd Qu.:19.0   3rd Qu.: 56.00  
##  Max.   :25.0   Max.   :120.00
```


