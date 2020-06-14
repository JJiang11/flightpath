data = load("SIA317.rda")
data1 = st_transform(data,st_crs(lonlat2UTM(st_coordinates(st_geometry(data[1])))))
data_line = st_cast(summarize(data1,do_union = FALSE),"LINESTRING")
plot(data_line)
smooth_line = smooth(data_line,method = "ksmooth")
plot(smooth_line)
plot(data1,add=TRUE)
#nrst_pts = st_cast(st_nearest_points(data1,smooth_line),"POINT")
#snapped = nrst_pts[seq.int(2L,length(nrst_pts),2L)]
#plot(snapped)
#plot(smooth_line,add=TRUE)
snaps = lapply(1:nrow(data1), function(i){
  ohsnap = st_snap(data1[i,],smooth_line,tolerance=1)
  print(ohsnap$geometry)
  return(ohsnap)
})
bound = do.call("rbind",snaps)
plot(bound)

join = lapply(1:nrow(e), function(i){
  if(e[i,])
})
