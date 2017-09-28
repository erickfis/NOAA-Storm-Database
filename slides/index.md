---
title       : NOAA - Worst natural disasters
subtitle    : Most damaging types of natural disasters
author      : erickfis@gmail.com
job         : 
framework   : shower        # {io2012, html5slides, shower, dzslides, ...}
highlighter : github  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : [bootstrap] 
ext_widgets : {rCharts: [libraries/nvd3]}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides

---




## NOAA Storm Database

### Worst Weather Events

The U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage.

The database currently contains data from January 1950 to January 2017, as entered by NOAA's National Weather Service (NWS).

Full study: https://erickfis.github.io/NOAA-Storm-Database/

erickfis@gmail.com

2017-09-28



---







## Property losses, in billions $

<link rel='stylesheet' href=/data/data-erick/R/x86_64-pc-linux-gnu-library/3.3/rCharts/libraries/nvd3/css/nv.d3.css>
<link rel='stylesheet' href=/data/data-erick/R/x86_64-pc-linux-gnu-library/3.3/rCharts/libraries/nvd3/css/rNVD3.css>
<script type='text/javascript' src=/data/data-erick/R/x86_64-pc-linux-gnu-library/3.3/rCharts/libraries/nvd3/js/jquery-1.8.2.min.js></script>
<script type='text/javascript' src=/data/data-erick/R/x86_64-pc-linux-gnu-library/3.3/rCharts/libraries/nvd3/js/d3.v3.min.js></script>
<script type='text/javascript' src=/data/data-erick/R/x86_64-pc-linux-gnu-library/3.3/rCharts/libraries/nvd3/js/nv.d3.min-new.js></script>
<script type='text/javascript' src=/data/data-erick/R/x86_64-pc-linux-gnu-library/3.3/rCharts/libraries/nvd3/js/fisheye.js></script> 
 <style>
  .rChart {
    display: block;
    margin-left: auto; 
    margin-right: auto;
    width: 800px;
    height: 400px;
  }  
  </style>
<div id = 'chart58a54b2dd89c' class = 'rChart nvd3'></div>
<script type='text/javascript'>
 $(document).ready(function(){
      drawchart58a54b2dd89c()
    });
    function drawchart58a54b2dd89c(){  
      var opts = {
 "dom": "chart58a54b2dd89c",
"width":    800,
"height":    400,
"x": "event",
"y": "total.raw",
"type": "discreteBarChart",
"id": "chart58a54b2dd89c" 
},
        data = [
 {
 "event": "hurricane",
"total.raw":    87.00517031,
"media.raw": 11433157543.75,
"rank": 1,
"colors": "#E41A1C" 
},
{
 "event": "flood",
"total.raw":    83.73651668,
"media.raw": 11433157543.75,
"rank": 2,
"colors": "#377EB8" 
},
{
 "event": "tornado",
"total.raw": 63.74192619175,
"media.raw": 11433157543.75,
"rank": 3,
"colors": "#4DAF4A" 
},
{
 "event": "tide",
"total.raw":     54.1551026,
"media.raw": 11433157543.75,
"rank": 4,
"colors": "#984EA3" 
},
{
 "event": "wind",
"total.raw": 25.47653061825,
"media.raw": 11433157543.75,
"rank": 5,
"colors": "#FF7F00" 
},
{
 "event": "hail",
"total.raw":  25.4024745937,
"media.raw": 11433157543.75,
"rank": 6,
"colors": "#FFFF33" 
},
{
 "event": "storm",
"total.raw":    16.75469036,
"media.raw": 11433157543.75,
"rank": 7,
"colors": "#A65628" 
} 
]
  
      if(!(opts.type==="pieChart" || opts.type==="sparklinePlus" || opts.type==="bulletChart")) {
        var data = d3.nest()
          .key(function(d){
            //return opts.group === undefined ? 'main' : d[opts.group]
            //instead of main would think a better default is opts.x
            return opts.group === undefined ? opts.y : d[opts.group];
          })
          .entries(data);
      }
      
      if (opts.disabled != undefined){
        data.map(function(d, i){
          d.disabled = opts.disabled[i]
        })
      }
      
      nv.addGraph(function() {
        var chart = nv.models[opts.type]()
          .width(opts.width)
          .height(opts.height)
          
        if (opts.type != "bulletChart"){
          chart
            .x(function(d) { return d[opts.x] })
            .y(function(d) { return d[opts.y] })
        }
          
         
        chart
  .color([ "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628" ])
          
        

        
        
        
      
       d3.select("#" + opts.id)
        .append('svg')
        .datum(data)
        .transition().duration(500)
        .call(chart);

       nv.utils.windowResize(chart.update);
       return chart;
      });
    };
</script>


---

## Crops losses, in billions $

<link rel='stylesheet' href=/data/data-erick/R/x86_64-pc-linux-gnu-library/3.3/rCharts/libraries/nvd3/css/nv.d3.css>
<link rel='stylesheet' href=/data/data-erick/R/x86_64-pc-linux-gnu-library/3.3/rCharts/libraries/nvd3/css/rNVD3.css>
<script type='text/javascript' src=/data/data-erick/R/x86_64-pc-linux-gnu-library/3.3/rCharts/libraries/nvd3/js/jquery-1.8.2.min.js></script>
<script type='text/javascript' src=/data/data-erick/R/x86_64-pc-linux-gnu-library/3.3/rCharts/libraries/nvd3/js/d3.v3.min.js></script>
<script type='text/javascript' src=/data/data-erick/R/x86_64-pc-linux-gnu-library/3.3/rCharts/libraries/nvd3/js/nv.d3.min-new.js></script>
<script type='text/javascript' src=/data/data-erick/R/x86_64-pc-linux-gnu-library/3.3/rCharts/libraries/nvd3/js/fisheye.js></script> 
 <style>
  .rChart {
    display: block;
    margin-left: auto; 
    margin-right: auto;
    width: 800px;
    height: 400px;
  }  
  </style>
<div id = 'chart58a5f952642' class = 'rChart nvd3'></div>
<script type='text/javascript'>
 $(document).ready(function(){
      drawchart58a5f952642()
    });
    function drawchart58a5f952642(){  
      var opts = {
 "dom": "chart58a5f952642",
"width":    800,
"height":    400,
"x": "event",
"y": "total.raw",
"type": "discreteBarChart",
"id": "chart58a5f952642" 
},
        data = [
 {
 "event": "drought",
"total.raw":    27.45486262,
"media.raw":  2857717344.65,
"rank": 1,
"colors": "#E41A1C" 
},
{
 "event": "flood",
"total.raw":     7.75026627,
"media.raw":  2857717344.65,
"rank": 2,
"colors": "#377EB8" 
},
{
 "event": "hurricane",
"total.raw":      5.3418748,
"media.raw":  2857717344.65,
"rank": 3,
"colors": "#4DAF4A" 
},
{
 "event": "cold",
"total.raw":      5.1239932,
"media.raw":  2857717344.65,
"rank": 4,
"colors": "#984EA3" 
},
{
 "event": "wind",
"total.raw":     4.42968525,
"media.raw":  2857717344.65,
"rank": 5,
"colors": "#FF7F00" 
},
{
 "event": "hail",
"total.raw":    3.657850413,
"media.raw":  2857717344.65,
"rank": 6,
"colors": "#FFFF33" 
} 
]
  
      if(!(opts.type==="pieChart" || opts.type==="sparklinePlus" || opts.type==="bulletChart")) {
        var data = d3.nest()
          .key(function(d){
            //return opts.group === undefined ? 'main' : d[opts.group]
            //instead of main would think a better default is opts.x
            return opts.group === undefined ? opts.y : d[opts.group];
          })
          .entries(data);
      }
      
      if (opts.disabled != undefined){
        data.map(function(d, i){
          d.disabled = opts.disabled[i]
        })
      }
      
      nv.addGraph(function() {
        var chart = nv.models[opts.type]()
          .width(opts.width)
          .height(opts.height)
          
        if (opts.type != "bulletChart"){
          chart
            .x(function(d) { return d[opts.x] })
            .y(function(d) { return d[opts.y] })
        }
          
         
        chart
  .color([ "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33" ])
          
        

        
        
        
      
       d3.select("#" + opts.id)
        .append('svg')
        .datum(data)
        .transition().duration(500)
        .call(chart);

       nv.utils.windowResize(chart.update);
       return chart;
      });
    };
</script>


---

## Worst Weather Events - Fatalities




<link rel='stylesheet' href=/data/data-erick/R/x86_64-pc-linux-gnu-library/3.3/rCharts/libraries/nvd3/css/nv.d3.css>
<link rel='stylesheet' href=/data/data-erick/R/x86_64-pc-linux-gnu-library/3.3/rCharts/libraries/nvd3/css/rNVD3.css>
<script type='text/javascript' src=/data/data-erick/R/x86_64-pc-linux-gnu-library/3.3/rCharts/libraries/nvd3/js/jquery-1.8.2.min.js></script>
<script type='text/javascript' src=/data/data-erick/R/x86_64-pc-linux-gnu-library/3.3/rCharts/libraries/nvd3/js/d3.v3.min.js></script>
<script type='text/javascript' src=/data/data-erick/R/x86_64-pc-linux-gnu-library/3.3/rCharts/libraries/nvd3/js/nv.d3.min-new.js></script>
<script type='text/javascript' src=/data/data-erick/R/x86_64-pc-linux-gnu-library/3.3/rCharts/libraries/nvd3/js/fisheye.js></script> 
 <style>
  .rChart {
    display: block;
    margin-left: auto; 
    margin-right: auto;
    width: 800px;
    height: 400px;
  }  
  </style>
<div id = 'chart58a5649e7dbd' class = 'rChart nvd3'></div>
<script type='text/javascript'>
 $(document).ready(function(){
      drawchart58a5649e7dbd()
    });
    function drawchart58a5649e7dbd(){  
      var opts = {
 "dom": "chart58a5649e7dbd",
"width":    800,
"height":    400,
"x": "event",
"y": "total",
"type": "discreteBarChart",
"id": "chart58a5649e7dbd" 
},
        data = [
 {
 "event": "tornado",
"total":           5887,
"mean": 680.9285714286,
"median":          187.5,
"rank": 1,
"colors": "#E41A1C" 
},
{
 "event": "heat",
"total":           2855,
"mean": 680.9285714286,
"median":          187.5,
"rank": 2,
"colors": "#377EB8" 
},
{
 "event": "wind",
"total":           2325,
"mean": 680.9285714286,
"median":          187.5,
"rank": 3,
"colors": "#4DAF4A" 
},
{
 "event": "flood",
"total":           1959,
"mean": 680.9285714286,
"median":          187.5,
"rank": 4,
"colors": "#984EA3" 
},
{
 "event": "winter",
"total":           1211,
"mean": 680.9285714286,
"median":          187.5,
"rank": 5,
"colors": "#FF7F00" 
},
{
 "event": "hurricane",
"total":           1128,
"mean": 680.9285714286,
"median":          187.5,
"rank": 6,
"colors": "#FFFF33" 
},
{
 "event": "lightning",
"total":            834,
"mean": 680.9285714286,
"median":          187.5,
"rank": 7,
"colors": "#A65628" 
},
{
 "event": "rip current",
"total":            806,
"mean": 680.9285714286,
"median":          187.5,
"rank": 8,
"colors": "#F781BF" 
} 
]
  
      if(!(opts.type==="pieChart" || opts.type==="sparklinePlus" || opts.type==="bulletChart")) {
        var data = d3.nest()
          .key(function(d){
            //return opts.group === undefined ? 'main' : d[opts.group]
            //instead of main would think a better default is opts.x
            return opts.group === undefined ? opts.y : d[opts.group];
          })
          .entries(data);
      }
      
      if (opts.disabled != undefined){
        data.map(function(d, i){
          d.disabled = opts.disabled[i]
        })
      }
      
      nv.addGraph(function() {
        var chart = nv.models[opts.type]()
          .width(opts.width)
          .height(opts.height)
          
        if (opts.type != "bulletChart"){
          chart
            .x(function(d) { return d[opts.x] })
            .y(function(d) { return d[opts.y] })
        }
          
         
        chart
  .color([ "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF" ])
          
        

        
        
        
      
       d3.select("#" + opts.id)
        .append('svg')
        .datum(data)
        .transition().duration(500)
        .call(chart);

       nv.utils.windowResize(chart.update);
       return chart;
      });
    };
</script>

---

## Worst Weather Events - Injuries

<link rel='stylesheet' href=/data/data-erick/R/x86_64-pc-linux-gnu-library/3.3/rCharts/libraries/nvd3/css/nv.d3.css>
<link rel='stylesheet' href=/data/data-erick/R/x86_64-pc-linux-gnu-library/3.3/rCharts/libraries/nvd3/css/rNVD3.css>
<script type='text/javascript' src=/data/data-erick/R/x86_64-pc-linux-gnu-library/3.3/rCharts/libraries/nvd3/js/jquery-1.8.2.min.js></script>
<script type='text/javascript' src=/data/data-erick/R/x86_64-pc-linux-gnu-library/3.3/rCharts/libraries/nvd3/js/d3.v3.min.js></script>
<script type='text/javascript' src=/data/data-erick/R/x86_64-pc-linux-gnu-library/3.3/rCharts/libraries/nvd3/js/nv.d3.min-new.js></script>
<script type='text/javascript' src=/data/data-erick/R/x86_64-pc-linux-gnu-library/3.3/rCharts/libraries/nvd3/js/fisheye.js></script> 
 <style>
  .rChart {
    display: block;
    margin-left: auto; 
    margin-right: auto;
    width: 800px;
    height: 400px;
  }  
  </style>
<div id = 'chart58a55b2a2d4e' class = 'rChart nvd3'></div>
<script type='text/javascript'>
 $(document).ready(function(){
      drawchart58a55b2a2d4e()
    });
    function drawchart58a55b2a2d4e(){  
      var opts = {
 "dom": "chart58a55b2a2d4e",
"width":    800,
"height":    400,
"x": "event",
"y": "total",
"type": "discreteBarChart",
"id": "chart58a55b2a2d4e" 
},
        data = [
 {
 "event": "tornado",
"total":          94815,
"mean": 5212.516129032,
"median":            317,
"rank": 1,
"colors": "#E41A1C" 
},
{
 "event": "heat",
"total":          15436,
"mean": 5212.516129032,
"median":            317,
"rank": 2,
"colors": "#377EB8" 
},
{
 "event": "wind",
"total":          13518,
"mean": 5212.516129032,
"median":            317,
"rank": 3,
"colors": "#4DAF4A" 
},
{
 "event": "flood",
"total":           8809,
"mean": 5212.516129032,
"median":            317,
"rank": 4,
"colors": "#984EA3" 
},
{
 "event": "winter",
"total":           8251,
"mean": 5212.516129032,
"median":            317,
"rank": 5,
"colors": "#FF7F00" 
} 
]
  
      if(!(opts.type==="pieChart" || opts.type==="sparklinePlus" || opts.type==="bulletChart")) {
        var data = d3.nest()
          .key(function(d){
            //return opts.group === undefined ? 'main' : d[opts.group]
            //instead of main would think a better default is opts.x
            return opts.group === undefined ? opts.y : d[opts.group];
          })
          .entries(data);
      }
      
      if (opts.disabled != undefined){
        data.map(function(d, i){
          d.disabled = opts.disabled[i]
        })
      }
      
      nv.addGraph(function() {
        var chart = nv.models[opts.type]()
          .width(opts.width)
          .height(opts.height)
          
        if (opts.type != "bulletChart"){
          chart
            .x(function(d) { return d[opts.x] })
            .y(function(d) { return d[opts.y] })
        }
          
         
        chart
  .color([ "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00" ])
          
        

        
        
        
      
       d3.select("#" + opts.id)
        .append('svg')
        .datum(data)
        .transition().duration(500)
        .call(chart);

       nv.utils.windowResize(chart.update);
       return chart;
      });
    };
</script>


