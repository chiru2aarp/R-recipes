# ************************************
# ****Recipe 1 : maps in googleVis****
# ************************************
#install.packages("googleVis")
library(googleVis)
debt = read.csv("~/../Downloads/Analytics/Books/R Recipes/module 2/3/data/debt.csv", 
                header = TRUE, sep =",")
eurdebt <- gvisGeoChart(debt, locationvar="Country", 
                      #numvar= "Debt_to_GDP_Ratio_2003",
                      hovervar="Debt_to_GDP_Ratio_2003", options = list(width = "600px", height ="700px",
                                                      dataMode = "regions", region = '150',
				colors= "[0xF8DFA7,0x8D9569,0xE9CC99,0xE2AD5A,0xCA7363]"))
plot(eurdebt)

# ************************************
# *** recipe 2 : Choropleth map*******
# ************************************
well = read.csv("~/../Downloads/Analytics/Books/R Recipes/module 2/3/data/wellbeing.csv", 
                header = TRUE, sep =",")
USA <- gvisGeoChart(well, "Score", locationvar="State",
                    #numvar ="Score",
                    hovervar = "text",
options = list(width = "600px", height ="700px",  region = "US"))
plot(USA)

# *****************************
# *** There's More*************
# *****************************
obese = read.csv("~/../Downloads/Analytics/Books/R Recipes/module 2/3/data/obesity.csv", 
                 header = TRUE, sep =",")
US_2012 <- gvisGeoChart(obese, locationvar="state",
                        hovervar ="X2012",
 options = list(width = 500, height =300, dataMode = "regions", region = "US",colors= "[0x64693D,0xF1CC80,0xEABA58,0xE3A732,0xDC9411]"))
US_2011 <- gvisGeoChart(obese, locationvar="state",
                        hovervar ="X2011",
options = list(width = 500, height =300,dataMode = "regions", region = "US",colors= "[0x64693D,0xF1CC80,0xEABA58,0xE3A732,0xDC9411]"))
US_2010 <- gvisGeoChart(obese, locationvar="state", hovervar ="X2010",
options = list(width = 500, height =300,dataMode = "regions", region = "US",colors= "[0x64693D,0xF1CC80,0xEABA58,0xE3A732,0xDC9411]"))
US_2009 <- gvisGeoChart(obese, locationvar="state", hovervar ="X2009",
options = list(width = 500, height =300,dataMode = "regions", region = "US",colors= "[0x64693D,0xF1CC80,0xEABA58,0xE3A732,0xDC9411]"))
merged = gvisMerge(gvisMerge(US_2009,US_2010, horizontal = TRUE),gvisMerge(US_2011,US_2012, horizontal = TRUE))
plot(merged)
# *****************************
# ***recipe 4 contour maps*****
# *****************************
contour(volcano,  main = "Topographic map of a Volcano", col = "blue")
filled.contour(volcano, color.palette = heat.colors, main = "Topographic map of a Volcano")# fills contours using the palette->  heat.color
filled.contour(volcano, color.palette = terrain.colors, main = "Topographic map of a Volcano")
filled.contour(volcano, color.palette = topo.colors, main = "Topographic map of a Volcano") # fills contours using the palette->  topo.color
### Using RColorBrewer
library(RColorBrewer)
custom = brewer.pal(9, "BuPu")
filled.contour(volcano, col = custom, main = "Topographic map of a Volcano") # fills contours using the custom palette defined 
# *******************************
# *** Theres More: NYC collision**
# ********************************
#install.packages("ggmap", dependencies = TRUE)
library(ggmap)
colide = read.csv("~/../Downloads/Analytics/Books/R Recipes/module 2/3/data/NYPD_Motor_Vehicle_Collisions.csv", 
                  header = TRUE, sep =",")
nyc = get_map("newyorkcity", zoom = 12)
nycmap = ggmap(nyc, extent = "device", legend = "topleft")
nycmap +
stat_density2d(
aes(x = LONGITUDE, y = LATITUDE, fill = ..level..,alpha = ..level..),
size = 2, bins = 4, data = colide,
geom = "polygon"
)

# *****************************
# **recipe 5 maps with bubbles*
# *****************************
emisn = read.csv("~/../Downloads/Analytics/Books/R Recipes/module 2/3/data/ghg.csv",sep =",", header = TRUE)
emit = gvisGeoChart(emisn,locationvar = "Country",
 	        sizevar = "Emission_in_2010", 
	        options = list(displayMode = "markers",width = 900, height =500,
                markerOpacity = 0.5,sizeAxis="{maxSize:'35'}",
	        colorAxis="{colors:['green','red']}"))
plot(emit)
# *****************************
# ******Theres More***********
# *****************************
emisn = read.csv("~/../Downloads/Analytics/Books/R Recipes/module 2/3/data/ghg.csv",sep =",", header = TRUE)
emit = gvisGeoChart(emisn,locationvar = "Country",
 	        sizevar = "Emission_in_2010", 
	        options = list(displayMode = "markers",width = 900, height =500,
                markerOpacity = 0.5,sizeAxis="{maxSize:'35'}",
	        colorAxis="{colors:['green','red']}"))
emisn1= read.csv("~/../Downloads/Analytics/Books/R Recipes/module 2/3/data/ghg1.csv")
emit1 = gvisLineChart(emisn1, xvar = "Year", 
		      yvar=c("Brazil","China","India","Russia","USA"), 
                      options = list( width = 500, height = 500,title ="Green House Gas Emissions", 
                                     vAxis="{title:'Million Metric tons of CO2'}",
                                      hAxis="{title:'Year'}",gvis.editor ="Edit the Line Chart"))
merge = gvisMerge(emit,emit1, horizontal = TRUE)
plot(merge)
# ****************************
# ** Text and Maps************
# ****************************
#install.packages("maps")
library(maps)
names = read.csv("~/../Downloads/Analytics/Books/R Recipes/module 2/3/data/names.csv")
map("state")
for(i in 1: 50){
    text(names$lon[i],names$lat[i], names$name[i], adj= 0.5)
  }


