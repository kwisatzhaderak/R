#5332    28.2074333333   -82.3457
#5326    26.704073       -80.234264
#5242    60.8902147059   -150.410054412
#5080    30.351032       -81.494914


##Visualizing cluster results

require(stringr)
require(ggmap)

#For the uids we chose:
#bb@mris.com, cav@lnf.com, cci@vcn.com
#grab their geocoded actions
geotuples = read.delim("~/R/Data/3exampleemails.txt", header = FALSE, sep = "}" )
geotuples = as.character(geotuples[,1])
geotuples = gsub('\\{|\\}','',geotuples)
geotuples = strsplit(geotuples,"\\),\\(")
geotuples = lapply(geotuples, function(x){gsub('\\(|\\)', '', x)})
geotuples = lapply(geotuples, function(x){gsub(',[a-z]+@[a-z]+.com', '',x)})
geotuples = lapply(geotuples, function(x){unlist(strsplit(x,","))})


#Now put them into a mapable format
user_points = numeric(0)
for(i in 1:length(geotuples)){

  geodata = geotuples[[i]]

  iterations = length(geodata)/3
  uid_index = seq(from=1, by = 5, length.out = iterations)
  actn_index = seq(from=2, by = 5, length.out = iterations)
  lat_index = seq(from=3, by = 5, length.out = iterations)
  lon_index = seq(from=4, by = 5, length.out = iterations)
  wgt_index = seq(from=5, by = 5, length.out = iterations)

  uid = geodata[uid_index]
  actn = geodata[actn_index]
  lat = as.numeric(as.character(geodata[lat_index]))
  lon = as.numeric(as.character(geodata[lon_index]))
  wgt = as.numeric(as.character(geodata[wgt_index]))

  points = data.frame(cbind(uid, actn, lat, lon, wgt, deparse.level = 1))
  #centroids = read.delim(file='ExampleResults.tsv')
  
  user_points = rbind(points,user_points)
  rm(uid)
  rm(lat)
  rm(lon)
  rm(uid_index)
  rm(lat_index)
  rm(lon_index)
}

user_points$lat = jitter(as.numeric(as.character(user_points$lat)), factor = 2)
user_points$lon = jitter(as.numeric(as.character(user_points$lon)), factor = 2)
user_points$uid = factor(user_points$uid)
user_points$actn = factor(user_points$actn)
user_points$wgt = as.numeric(user_points$wgt)/max(as.numeric(user_points$wgt))

user_points = user_points[is.na(user_points$uid)==F,]

#Now that we have all the lat/lons, let's use ggplot2 to get our maps together
base_map <- get_googlemap(center = c(lon = -98.3532, lat = 32.5649),
              zoom = 4, size = c(640, 640), scale = 2,
              format = c("png8", "gif", "jpg", "jpg-baseline", "png32"),
              maptype = c("terrain"),
              language = "en-EN", 
              #region, 
              #markers = user_points, 
              #path = user_points,
              #visible, style, 
              sensor = FALSE, messaging = FALSE,
              urlonly = FALSE, filename = "ggmapTemp",
              color = c("color", "bw"))

#color_index = c('dark_red','dark_blue','dark_green')[as.numeric(user_points$uid)]

#5332    28.2074333333   -82.3457
#5326    26.704073       -80.234264
#5080    30.351032       -81.494914
#24186   29.8393663529   -95.3112245882


user_centroids = data.frame(rbind(
  cbind('5332','COI',28.207433333,-82.3457),
  cbind('5326','COI',26.704073, -80.234264),
  cbind('5080','COI',30.351032, -81.494914),
  cbind('24186','COI',29.8393663529,-95.3112245882)))
names(user_centroids) = c('uid','actn','lat','lon')
user_centroids$lat = as.numeric(as.character(user_centroids$lat))
user_centroids$lon = as.numeric(as.character(user_centroids$lon))


#Get shape identities. We want bids = B, regs = R, saved_search = S, and pdp_view = V
user_points$shape = ifelse(user_points$actn == 'bid', 66,  #B
                           ifelse(user_points$actn == 'pdp_view', 86, #V
                                  ifelse(user_points$actn == 'registration', 82, #R
                                         ifelse(user_points$actn == 'saved_search', 83, 19)))) #S or circle
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggmap(base_map) +
  #scale_shape_discrete('actn',labels = c('bid','pdp_view','registration','saved_search'))
  scale_shape_identity() +
  geom_point(aes(x = lon, y = lat, colour = uid, shape = shape),alpha = 0.8, data = user_points, size = 5, )+
  scale_colour_manual(name ="user_id", labels = c('24186','5080','5326','5332'), values=c("#0072B2", "#D55E00", "#CC79A7", "#009E73"))+
  #scale_colour_discrete('user_id',labels = c('24186','5080','5326','5332'))+
  geom_point(aes(x = lon, y = lat),alpha = 1, colour = 'black', data = user_centroids, size = 3)
  #geom_point(aes(x = lon, y = lat), data = user_points$uid[as.numeric(user_points$uid)==2,], size = 3, colour = 'dark_blue')+
  #geom_point(aes(x = lon, y = lat), data = user_points$uid[as.numeric(user_points$uid)==3,], size = 3, colour = 'dark_green')
  #geom_path(aes(x = lon, y = lat), data = user_points)

#Get in closer on each point
#Now that we have all the lat/lons, let's use ggplot2 to get our maps together
map_24186_a <- get_googlemap(center = c(lon = -95.3112245882, lat = 29.8393663529),
                          zoom = 9, size = c(640, 640), scale = 2,
                          format = c("png8", "gif", "jpg", "jpg-baseline", "png32"),
                          maptype = c("terrain"),
                          language = "en-EN", 
                          #region, 
                          #markers = user_points, 
                          #path = user_points,
                          #visible, style, 
                          sensor = FALSE, messaging = FALSE,
                          urlonly = FALSE, filename = "ggmapTemp",
                          color = c("color", "bw"))

map_24186_b <- get_googlemap(center = c(lon = -118.25, lat = 34.05),
                             zoom = 8, size = c(640, 640), scale = 2,
                             format = c("png8", "gif", "jpg", "jpg-baseline", "png32"),
                             maptype = c("terrain"),
                             language = "en-EN", 
                             #region, 
                             #markers = user_points, 
                             #path = user_points,
                             #visible, style, 
                             sensor = FALSE, messaging = FALSE,
                             urlonly = FALSE, filename = "ggmapTemp",
                             color = c("color", "bw"))

user_points_24186 = user_points[user_points$uid == 24186,]
centroid_24186 = user_centroids[user_centroids$uid ==24186,]

ggmap(map_24186_a) +
  #scale_shape_discrete('actn',labels = c('bid','pdp_view','registration','saved_search'))
  scale_shape_identity() +
  geom_point(aes(x = lon, y = lat, colour = actn, shape = shape),alpha = 1, data = user_points_24186, size = 8, )+
  scale_colour_manual(name ="Action", labels = unique(levels(user_points_24186$actn)), values=c("#0072B2", "#D55E00", "#000000", "#009E73"))+
  #scale_colour_discrete('user_id',labels = c('24186','5080','5326','5332'))+
  geom_point(aes(x = lon, y = lat),alpha = 1, colour = 'black', data = centroid_24186, size = 3)
#geom_point(aes(x = lon, y = lat), data = user_points$uid[as.numeric(user_points$uid)==2,], size = 3, colour = 'dark_blue')+
#geom_point(aes(x = lon, y = lat), data = user_points$uid[as.numeric(user_points$uid)==3,], size = 3, colour = 'dark_green')
#geom_path(aes(x = lon, y = lat), data = user_points)

ggmap(map_24186_b) +
  #scale_shape_discrete('actn',labels = c('bid','pdp_view','registration','saved_search'))
  scale_shape_identity() +
  geom_point(aes(x = lon, y = lat, colour = actn, shape = shape),alpha = 1, data = user_points_24186, size = 8, )+
  scale_colour_manual(name ="Action", labels = unique(levels(user_points_24186$actn)), values=c("#0072B2", "#D55E00", "#000000", "#009E73"))+
  #scale_colour_discrete('user_id',labels = c('24186','5080','5326','5332'))+
  geom_point(aes(x = lon, y = lat),alpha = 1, colour = 'black', data = centroid_24186, size = 3)
#geom_point(aes(x = lon, y = lat), data = user_points$uid[as.numeric(user_points$uid)==2,], size = 3, colour = 'dark_blue')+
#geom_point(aes(x = lon, y = lat), data = user_points$uid[as.numeric(user_points$uid)==3,], size = 3, colour = 'dark_green')
#geom_path(aes(x = lon, y = lat), data = user_points)

#And now show the properties we matched for them:
properties_24186 = data.frame(rbind(
                         cbind(24186, "Beck: CRE",	29.87839,	-95.192313, NA, 16),
                         cbind(24186,	"Beck: CRE",	29.9978413,	-90.1508062, NA, 16),
                         cbind(24186,	"Beck: CRE",	33.714787,	-85.157044, NA, 16),
                         cbind(24186,	"Beck: CRE",	29.7353,	-95.521, NA, 16),
                         cbind(24186,	"Beck: notes",	29.7423,	-95.4893, NA, 16),
                         cbind(24186,	"Beck: notes",	29.6301,	-95.2146, NA, 16),
                         cbind(24186,	"Beck: notes",	29.4610,	-98.5699, NA, 16),
                         cbind(24186, "Beck: Center", centroid_24186$lat, centroid_24186$lon, NA, 17 )))
names(properties_24186) = c('uid','actn','lat','lon', 'wgt','shape' )
properties_24186 = rbind (user_points_24186,properties_24186)
properties_24186$lat = as.numeric(as.character(properties_24186$lat))
properties_24186$lon = as.numeric(as.character(properties_24186$lon))


ggmap(map_24186_a) +
  #scale_shape_discrete('actn',labels = c('bid','pdp_view','registration','saved_search'))
  scale_shape_identity() +
  geom_point(aes(x = lon, y = lat, colour = actn, shape = shape),alpha = 1, data = properties_24186, size = 8, )+
  scale_colour_manual(name ="Action", labels = properties_24186$actn, values=c("#0072B2", "#D55E00", "#000000", "green", 'dark green','red'))
  #scale_colour_discrete('user_id',labels = c('24186','5080','5326','5332'))+
  #geom_point(aes(x = lon, y = lat),alpha = 1, colour = 'red', data = centroid_24186, size = 4) +
  #scale_colour_manual(name ="Recommendation", labels = factor(unique(levels(user_points_24186$ProductType))), values=c("green", "light green", "dark green", 'red'))+
  #geom_point(aes(x = lon, y = lat),alpha = 1, colour = 'dark green ', data = properties_24186, size = 4)

#geom_point(aes(x = lon, y = lat), data = user_points$uid[as.numeric(user_points$uid)==2,], size = 3, colour = 'dark_blue')+
#geom_point(aes(x = lon, y = lat), data = user_points$uid[as.numeric(user_points$uid)==3,], size = 3, colour = 'dark_green')
#geom_path(aes(x = lon, y = lat), data = user_points)

#do some analysis on data types, etc
aggregate(. ~ uid + actn, data = user_points[,c('uid','actn','uid')], FUN = function(x){length((x))})
