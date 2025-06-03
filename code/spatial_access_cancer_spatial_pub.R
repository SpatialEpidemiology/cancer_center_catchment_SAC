
############################
#### RUN THIS FILE 2ND #####
############################

library(tmap)
#this code utilizes tmap version 3.0
#it will not work with tmap version 4.0
library(sf)

#formatting
c1_c24ct2 <- st_drop_geometry(c1_c24ct2)
c1_c24ct2 <- left_join(tr,c1_c24ct2,by="id")
c1_c24ct2 <- st_as_sf(c1_c24ct2)
c1_c24ct<-c1_c24ct2
c2_c24ct2 <- st_drop_geometry(c2_c24ct2)
c2_c24ct2 <- left_join(tr,c2_c24ct2,by="id")
c2_c24ct2 <- st_as_sf(c2_c24ct2)
c2_c24ct<-c2_c24ct2
p24ct2 <- st_drop_geometry(p24ct2)
p24ct2 <- left_join(tr,p24ct2,by="id")
p24ct2 <- st_as_sf(p24ct2)
p24ct<-p24ct2

#adding county fips
c1_c24ct$cty <- str_sub(c1_c24ct$id,1,5)
c2_c24ct$cty <- str_sub(c2_c24ct$id,1,5)
p24ct$cty <- str_sub(p24ct$id,1,5)
#adding per 100k
c1_c24ct$car100k <- c1_c24ct$count.x*100000
c2_c24ct$car100k <- c2_c24ct$count.x*100000
p24ct$car100k <- p24ct$count.x*100000
#adding state
c1_c24ct$st <- str_sub(c1_c24ct$id,1,2)
c2_c24ct$st <- str_sub(c2_c24ct$id,1,2)
p24ct$st <- str_sub(p24ct$id,1,2)


#legend title
legend_title <- "Providers per 100k"

#cancer center locations
# Case Comprehensive Cancer Center
case_sf <- st_as_sf(case_df, coords = c("Longitude", "Latitude"), crs = 4326) |> 
  st_transform(crs = 3857)

# Moffitt Cancer Center
moffitt_sf <- st_as_sf(moffitt_df, coords = c("Longitude", "Latitude"), crs = 4326) |> 
  st_transform(crs = 3857)

# O'Neal Comprehensive Cancer Center
oneal_sf <- st_as_sf(oneal_df, coords = c("Longitude", "Latitude"), crs = 4326) |> 
  st_transform(crs = 3857)

# Fred & Pamela Buffett Cancer Center
buffett_sf <- st_as_sf(buffett_df, coords = c("Longitude", "Latitude"), crs = 4326) |> 
  st_transform(crs = 3857)

#oncology
p1 <- tm_shape(c1_c24ct[which(c1_c24ct$cty == "39007" | c1_c24ct$cty == "39035" | c1_c24ct$cty == "39055" | 
                                c1_c24ct$cty == "39085" | c1_c24ct$cty == "39093" | c1_c24ct$cty == "39099" | 
                                c1_c24ct$cty == "39103" | c1_c24ct$cty == "39043" | c1_c24ct$cty == "39133" | 
                                c1_c24ct$cty == "39005" | c1_c24ct$cty == "39077" | c1_c24ct$cty == "39151" | 
                                c1_c24ct$cty == "39153" | c1_c24ct$cty == "39155" | c1_c24ct$cty == "39169"),])+ tm_borders(lwd=0, col = "white") + tm_polygons("car100k",palette="plasma",breaks=c(0,10,20,30,40,50,Inf), title=legend_title, colorNA="lightgrey") +
               tm_compass(position=0) + tm_scale_bar(position=0.75, width=0.15) + tm_layout(legend.title.size = 0.7, legend.text.size = 0.5, legend.position=c(0.15,0.64), panel.labels = "A") +
               tm_shape(case_sf) + tm_squares("Center", size=0.2)


p2 <- tm_shape(c1_c24ct[which(c1_c24ct$cty == "12009" | c1_c24ct$cty == "12015" | c1_c24ct$cty == "12017" |
                                c1_c24ct$cty == "12021" | c1_c24ct$cty == "12027" | c1_c24ct$cty == "12043" |
                                c1_c24ct$cty == "12049" | c1_c24ct$cty == "12051" | c1_c24ct$cty == "12053" |
                                c1_c24ct$cty == "12055" | c1_c24ct$cty == "12057" | c1_c24ct$cty == "12069" |
                                c1_c24ct$cty == "12071" | c1_c24ct$cty == "12081" | c1_c24ct$cty == "12083" |
                                c1_c24ct$cty == "12095" | c1_c24ct$cty == "12097" | c1_c24ct$cty == "12101" |
                                c1_c24ct$cty == "12103" | c1_c24ct$cty == "12105" | c1_c24ct$cty == "12115" |
                                c1_c24ct$cty == "12117" | c1_c24ct$cty == "12119"), ]) + 
                tm_borders(lwd = 0, col="white") + 
                tm_polygons("car100k", palette = "plasma", breaks = c(0, 10, 20, 30, 40, 50, Inf), 
                title = legend_title, colorNA = "lightgrey") + 
                tm_compass(position = 0.02) + 
                tm_scale_bar(position = 0.75, width = 0.15) + 
                tm_layout(legend.title.size = 0.7, legend.text.size = 0.5, legend.position=c(0.05,0.64), panel.labels = "B")+
                tm_shape(moffitt_sf) + tm_squares("Center", size=0.2)   
 

p3 <- tm_shape(c1_c24ct[which(c1_c24ct$st=="01"),])+ 
               tm_borders(lwd = 0, col="white") + 
               tm_polygons("car100k", palette = "plasma", breaks = c(0, 10, 20, 30, 40, 50, Inf), 
               title = legend_title, colorNA = "lightgrey") + 
               tm_compass(position = 0.02) + 
               tm_scale_bar(position = 0.75, width = 0.15) + 
               tm_layout(legend.title.size = 0.7, legend.text.size = 0.5, legend.position = c(0.05,0.64), panel.labels = "C")+
               tm_shape(oneal_sf) + tm_bubbles("Center", size=0.2, col=NA)


p4 <- tm_shape(c1_c24ct[which(c1_c24ct$st=="31"),])+ tm_borders(lwd=0) + 
               tm_borders(lwd = 0, col="white") + 
               tm_polygons("car100k", palette = "plasma", breaks = c(0, 10, 20, 30, 40, 50, Inf), 
               title = legend_title, colorNA = "lightgrey") + 
               tm_compass(position = 0.15) + 
               tm_scale_bar(position = 0.7, width = 0.15) + 
               tm_layout(legend.title.size = 0.6, legend.text.size = 0.5, panel.labels = "D")+
               tm_shape(buffett_sf) + tm_squares("Center", size=0.2)

#arrange plots into facets
p_c1<-tmap_arrange(p1,p2,p3,p4,ncol=2,nrow=2)
#figure for oncologists
tmap_save(p_c1, "/YOUR_PATH/c1_c24_map.png", dpi=400, width=8, height=7)

#expanded cancer care
p5 <- tm_shape(c2_c24ct[which(c2_c24ct$cty == "39007" | c2_c24ct$cty == "39035" | c2_c24ct$cty == "39055" | 
                                c2_c24ct$cty == "39085" | c2_c24ct$cty == "39093" | c2_c24ct$cty == "39099" | 
                                c2_c24ct$cty == "39103" | c2_c24ct$cty == "39043" | c2_c24ct$cty == "39133" | 
                                c2_c24ct$cty == "39005" | c2_c24ct$cty == "39077" | c2_c24ct$cty == "39151" | 
                                c2_c24ct$cty == "39153" | c2_c24ct$cty == "39155" | c2_c24ct$cty == "39169"),]) + 
               tm_borders(lwd=0, col = "white") + tm_polygons("car100k",palette="plasma",breaks=c(0,50,100,150,200,250,Inf), title=legend_title, colorNA="lightgrey") +
               tm_compass(position=0) + tm_scale_bar(position=0.75, width=0.15) + 
               tm_layout(legend.title.size = 0.7, legend.text.size = 0.5, legend.position=c(0.15,0.64), panel.labels = "A") 


p6 <- tm_shape(c2_c24ct[which(c2_c24ct$cty == "12009" | c2_c24ct$cty == "12015" | c2_c24ct$cty == "12017" |
                                c2_c24ct$cty == "12021" | c2_c24ct$cty == "12027" | c2_c24ct$cty == "12043" |
                                c2_c24ct$cty == "12049" | c2_c24ct$cty == "12051" | c2_c24ct$cty == "12053" |
                                c2_c24ct$cty == "12055" | c2_c24ct$cty == "12057" | c2_c24ct$cty == "12069" |
                                c2_c24ct$cty == "12071" | c2_c24ct$cty == "12081" | c2_c24ct$cty == "12083" |
                                c2_c24ct$cty == "12095" | c2_c24ct$cty == "12097" | c2_c24ct$cty == "12101" |
                                c2_c24ct$cty == "12103" | c2_c24ct$cty == "12105" | c2_c24ct$cty == "12115" |
                                c2_c24ct$cty == "12117" | c2_c24ct$cty == "12119"), ]) + 
               tm_borders(lwd = 0, col="white") + 
               tm_polygons("car100k", palette = "plasma", breaks = c(0,50,100,150,200,250,Inf), 
               title = legend_title, colorNA = "lightgrey") + 
               tm_compass(position = 0.02) + 
               tm_scale_bar(position = 0.75, width = 0.15) + 
               tm_layout(legend.title.size = 0.7, legend.text.size = 0.5, legend.position=c(0.05,0.64), panel.labels = "B")   


p7 <- tm_shape(c2_c24ct[which(c2_c24ct$st=="01"),])+ 
               tm_borders(lwd = 0, col="white") + 
               tm_polygons("car100k", palette = "plasma", breaks = c(0,50,100,150,200,250,Inf), 
               title = legend_title, colorNA = "lightgrey") + 
               tm_compass(position = 0.02) + 
               tm_scale_bar(position = 0.75, width = 0.15) + 
               tm_layout(legend.title.size = 0.7, legend.text.size = 0.5, legend.position = c(0.05,0.64), panel.labels = "C") 


p8 <- tm_shape(c2_c24ct[which(c2_c24ct$st=="31"),])+ tm_borders(lwd=0) + 
               tm_borders(lwd = 0, col="white") + 
               tm_polygons("car100k", palette = "plasma", breaks = c(0,50, 100, 150, 200, 250, Inf), 
               title = legend_title, colorNA = "lightgrey") + 
               tm_compass(position = 0.15) + 
               tm_scale_bar(position = 0.7, width = 0.15) + 
               tm_layout(legend.title.size = 0.6, legend.text.size = 0.5, panel.labels = "D") 

#arrange plots into facets
p_c2<-tmap_arrange(p5,p6,p7,p8,ncol=2,nrow=2)
#figure for cancer care
tmap_save(p_c2, "/YOUR_PATH/c2_c24_map.png", dpi=400, width=8, height=7)

#primary care
p9 <- tm_shape(p24ct[which(p24ct$cty == "39007" | p24ct$cty == "39035" | p24ct$cty == "39055" | 
                                p24ct$cty == "39085" | p24ct$cty == "39093" | p24ct$cty == "39099" | 
                                p24ct$cty == "39103" | p24ct$cty == "39043" | p24ct$cty == "39133" | 
                                p24ct$cty == "39005" | p24ct$cty == "39077" | p24ct$cty == "39151" | 
                                p24ct$cty == "39153" | p24ct$cty == "39155" | p24ct$cty == "39169"),])+ tm_borders(lwd=0, col = "white") + tm_polygons("car100k",palette="plasma",breaks=c(0,30,60,90,120,150,Inf), title=legend_title, colorNA="lightgrey") +
               tm_compass(position=0) + tm_scale_bar(position=0.75, width=0.15) + 
               tm_layout(legend.title.size = 0.7, legend.text.size = 0.5, legend.position=c(0.15,0.64), panel.labels = "A") 


p10 <- tm_shape(p24ct[which(p24ct$cty == "12009" | p24ct$cty == "12015" | p24ct$cty == "12017" |
                                p24ct$cty == "12021" | p24ct$cty == "12027" | p24ct$cty == "12043" |
                                p24ct$cty == "12049" | p24ct$cty == "12051" | p24ct$cty == "12053" |
                                p24ct$cty == "12055" | p24ct$cty == "12057" | p24ct$cty == "12069" |
                                p24ct$cty == "12071" | p24ct$cty == "12081" | p24ct$cty == "12083" |
                                p24ct$cty == "12095" | p24ct$cty == "12097" | p24ct$cty == "12101" |
                                p24ct$cty == "12103" | p24ct$cty == "12105" | p24ct$cty == "12115" |
                                p24ct$cty == "12117" | p24ct$cty == "12119"), ]) + 
                tm_borders(lwd = 0, col="white") + 
                tm_polygons("car100k", palette = "plasma", breaks = c(0,25,50,75,100,125,Inf), 
                title = legend_title, colorNA = "lightgrey") + 
                tm_compass(position = 0.02) + 
                tm_scale_bar(position = 0.75, width = 0.15) + 
                tm_layout(legend.title.size = 0.7, legend.text.size = 0.5, legend.position=c(0.05,0.64), panel.labels = "B")   


p11 <- tm_shape(p24ct[which(p24ct$st=="01"),])+ 
                tm_borders(lwd = 0, col="white") + 
                tm_polygons("car100k", palette = "plasma", breaks = c(0,25,50,75,100,125,Inf), 
                title = legend_title, colorNA = "lightgrey") + 
                tm_compass(position = 0.02) + 
                tm_scale_bar(position = 0.75, width = 0.15) + 
                tm_layout(legend.title.size = 0.7, legend.text.size = 0.5, legend.position = c(0.05,0.64), panel.labels = "C") 


p12 <- tm_shape(p24ct[which(p24ct$st=="31"),])+ tm_borders(lwd=0) + 
                tm_borders(lwd = 0, col="white") + 
                tm_polygons("car100k", palette = "plasma", breaks = c(0, 25, 50, 75, 100, 125, Inf), 
                title = legend_title, colorNA = "lightgrey") + 
                tm_compass(position = 0.15) + 
                tm_scale_bar(position = 0.7, width = 0.15) + 
                tm_layout(legend.title.size = 0.6, legend.text.size = 0.5, panel.labels = "D") 

#arrange plots into facets
p_p24<-tmap_arrange(p9,p10,p11,p12,ncol=2,nrow=2)
#figure for primary care
tmap_save(p_p24, "/YOUR_PATH/p24_map.png", dpi=400, width=8, height=7)


           