#' @title create_path
#' @name create_path
#' @description convert point flight data into a chained path
#' @param data sf of points in order, joined with nonspatial flight data representing one aircraft over time
#' @param smooth boolean, default TRUE. which activates the smoothing algorithm from smoothr
#' @param method optional, smoothing algorithm to be used, default ksmooth. See smoothr documentation.
#' @param smoothness optional, degree of smoothness, default 1. See smoothr documentation.
#'
#' @return sf of linestrings and interpolated nonspatial data.
#'
#' @examples
#' \dontrun{create_path(get_live_data("username","password",duration=100,icao="000000"))}
#'
#' @export create_path
#' @import smoothr, FNN, dplyr

create_path = function(data, smooth = TRUE, method = "ksmooth", ...){
  #reproject to mercator for compatibility with st_snap
  #get crs based on first coordinate point
  crs = data[1] %>%
    st_geometry() %>%
    st_coordinates() %>%
    lonlat2UTM() %>%
    st_crs
  #reproject
  data1 = st_transform(data,st_crs(lonlat2UTM(st_coordinates(st_geometry(data[1])))))
  #remove duplicate columns
  data1 = data1 %>% distinct(time_position,.keep_all=TRUE)
  #create multipoint and then linestring
  data_line = st_cast(summarize(data1,do_union = FALSE),"LINESTRING")
  #begin geo operations
  #if the user wants it smoothened
  if(smooth){
    smooth_line = smooth(data_line, method = method, ...)
  }
  else{
    #line is already smooth enough, no interp necessary
    #(e.g. plane is travelling fast and straight)
    smooth_line = data_line
  }
  #plot(smooth_line)
  #plot(data1,add=TRUE)
  #nrst_pts = st_cast(st_nearest_points(data1,smooth_line),"POINT")
  #snapped = nrst_pts[seq.int(2L,length(nrst_pts),2L)]
  #plot(snapped)
  #plot(smooth_line,add=TRUE)

  #create list of snap sfs by applying st_snap for each observation
  snaps = lapply(1:nrow(data1), function(i){
    ohsnap = st_snap(data1[i,],smooth_line,tolerance=1)
    return(ohsnap)
  })
  #rbind snaps into single dataframe
  snaps = do.call("rbind",snaps)
  #calculate nearest-neighbor to determine proper buffer sizes
  nn = get.knn(st_coordinates(snaps),k=1)

  #set buffer radii to distance to nearest neighbor divided by three.
  #this is because we want line segments closest to observations to inherit attributes directly
  #and line-segments in-between buffers (farthest) to average attributes from its two neighbor observations
  #this form of interpolation will improve accuracy when converting point data to linestring data.
  #we also want to avoid overlapping buffers. buffer sizes will shrink as points become denser
  bufo = st_buffer(snaps,nn$nn.dist/3)

  #split the smooth line by buffers.
  #the resulting # of generated line segments should be (# of points)*2 - 1
  segs = st_collection_extract(lwgeom::st_split(smooth_line,bufo),"LINESTRING")

  #obtain sparse matrix with indices matching segs to the buffers they touch
  #end segs (and all odd-numbered segs) should only touch one buffer
  #even segs represent the in-between segments and should touch two buffers
  #adjust dist if this is not the case
  withins = st_is_within_distance(segs,bufo,dist=10)

  #all non-spatial data at this point is contained in bufo.
  #take the average of all numeric attributes among the one or two
  #observations indicated by the index/indices in withins
  lin_ref = lapply(1:nrow(withins), function(i){
    summarize_if(bufo[withins[[i]],],is.numeric,mean)
  })
  #bind these results together in a single dataframe
  lin_ref = do.call("rbind",lin_ref)
  #replace the buffer(polygon & multipolygon) geometry with segment geometry
  lin_ref$geometry = st_geometry(segs)
  #return sf
  return(lin_ref)
}
