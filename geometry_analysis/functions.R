

fetchn_w_annotation <- function(anno, conn, anno_type, intersect_skids, ...) {
  nl = catmaid::read.neurons.catmaid(paste0("annotation:^", anno, "$"), conn=conn, ...)
  if (!is.null(intersect_skids)) nl = nl[names(nl) %>% as.integer %in% intersect_skids]
  nl[, anno_type] = anno
  nl
}

fetch_neurons <- function(annos, conn, anno_type='annos', ref="FCWB", border_tag=NULL, intersect_skids = NULL, ...) {
  nl = do.call(c, lapply(annos, fetchn_w_annotation, conn, anno_type, intersect_skids, ...))
  
  if (anyDuplicated(attr(nl, 'df')$skid) != 0)  message("More than 1 annotations on a neuron!")
  if (!is.null(ref)) nl = nlapply(nl, xform_brain, sample="FAFB13", reference=ref)
  if (!is.null(border_tag)) nl = nlapply(nl, function(n) segment_arbor(n, border_tag, n$skid))
  nl
}

smooth_spikes <- function(n, sects) {
  ng = as.ngraph(n)
  ns = row.names(n$d[n$d$Z %in% (sects*35),]) %>% as.numeric
  for (node in ns) {
    outg = n$d[ego(ng, 1, node, "out") %>% unlist %>% setdiff(node),] %>% xyzmatrix
    inc = n$d[ego(ng, 1, node, "in") %>% unlist %>% setdiff(node),] %>% xyzmatrix
    
    if (nrow(inc)==0 && nrow(outg)>0) xyz_c = outg[1,]-1
    if (nrow(outg)==0 && nrow(inc)>0) xyz_c = inc[1,]-1
    if (nrow(inc)>0 && nrow(outg)>0) xyz_c = rbind(inc[1,], outg[1,]) %>% colMeans
    # inc_xyz = inc[nabor::knn(inc, outg, k=1)$nn.idx[[1]],]
    # outg_xyz = outg[nabor::knn(outg, inc, k=1)$nn.idx[[1]],]
    n$d[node, c("X", "Y", "Z")] = xyz_c
  }
  n
} 

get_nndist_sd <- function(n1, n2, neuron1, neuron2) {
  # given 2 neurons, n1 and n2, 
  # calculate the minimal of mean distance and sd between them
  min_mean = min(mean(get_nndist(n1, n2)), mean(get_nndist(n2, n1)))
  min_sd = min(sd(get_nndist(n1, n2)), sd(get_nndist(n2, n1)))
  list('dist_mean'=min_mean, 'dist_sd'=min_sd, 'neuron1'=neuron1, 'neuron2'=neuron2)
}

get_dist_summary <- function(nl) {
  # given a neuronlist, 
  # return all pair-wise combinations of neurons in the neuronlist,
  # and calculate mean_dist, sd
  
  combn(nl, 2, simplify=FALSE) %>%
    lapply(function(x) get_nndist_sd(x[[1]], x[[2]], names(x[1]), names(x[2])))
}

get_nblast_score <- function(nl) {
  nl = dotprops(nl, k=2)
  combn(nl, 2, simplify=FALSE) %>%
    lapply(function(x) list("nblast_mean_score"=mean(nblast(x[[1]], x[[2]], normalised=TRUE), 
                                                     nblast(x[[2]], x[[1]], normalised=TRUE)),
                            'neuron1'=names(x[1]), 'neuron2'=names(x[2])))
}

# a list with each column as an element
# columns: 'dist_mean', 'dist_sd', 'type', 'groups', 'neuron1', neuron2',
summarize_pair_wise <- function(nl, glom_list, group_name, FUN=get_dist_summary) {
  # given a neuron list, glom_list, 
  # calculate mean distances (and sd) for each pair
  # summarize results into a table
  result = list()
  for (i in seq_along(glom_list)) {
    g  = glom_list[[i]]
    result[[i]] = subset(nl, std_glom==g) %>% 
      FUN %>%
      lapply(as.data.frame) %>%
      {do.call(rbind, .)} %>%
      mutate(type=glom_list[[i]], groups=group_name)
  }
  do.call(rbind, result)
}

rank_glom_by_diff <- function(data_tbl) {
  dist_mean_rank = group_by(data_tbl, type, groups) %>% summarize(mean_summary=mean(dist_mean))
  glom_dist = c()
  temp_gloms = filter(data_tbl, groups=='LM')$type %>% unique %>% as.character
  for (i in seq_along(temp_gloms)) {
    glom = temp_gloms[[i]]
    glom_dist[glom] = filter(dist_mean_rank, type==glom) %>% 
    {filter(., groups=='LM')$mean_summary - filter(., groups=='FAFB')$mean_summary}
  }
  sort(glom_dist, decreasing=TRUE) %>% names
}

rank_glom_by_nblast <- function(data_tbl) {
  dist_mean_rank = group_by(data_tbl, type, groups) %>% summarize(mean_summary=mean(nblast_mean_score))
  glom_dist = c()
  temp_gloms = filter(data_tbl, groups=='LM')$type %>% unique %>% as.character
  for (i in seq_along(temp_gloms)) {
    glom = temp_gloms[[i]]
    glom_dist[glom] = filter(dist_mean_rank, type==glom) %>% 
    {filter(., groups=='FAFB')$mean_summary - filter(., groups=='LM')$mean_summary}
  }
  sort(glom_dist, decreasing=TRUE) %>% names
}

my_pal <- function(n) {
  if (n<=3) {
    pal=rainbow(3)[c(1,3,2)[1:n]]
  } else if (n<=6) {
    pal=c(rainbow(3), rainbow(4)[1:(n-3)])
  } else {
    pal=c(rainbow(3), rainbow(n-3))
  }
  pal
}

# calyx bounding box in FCWB space for subset---------
bb_rca = boundingbox(subset(FCWBNP.surf, "MB_CA_R"))
bb_rca[1,1] = 179

in_bbox<-function(b) { 
  function(x, y=NULL, z=NULL) 
    with(xyz.coords(x,y,z), 
         b[1,1]<=x & x<=b[2,1] & b[1,2]<=y & y<=b[2,2] & b[1,3]<=z & z<=b[2,3])
}

in_fc_rca = in_bbox(bb_rca)

get_calyx_collaterals <- function(nl, rs=1, spine_iv=TRUE) {
  nlapply(nl, resample, rs) %>%
    nlapply(spine, UseStartPoint=TRUE, invert=spine_iv, rval="neuron") %>%
    nlapply(subset, in_fc_rca)
}

get_calyx_skeletons <- function(nl, rs=1) {
  nlapply(nl, resample, rs) %>%
    nlapply(subset, in_fc_rca)
}

# load glom collections and functions--------
fb_gloms_extra = c("glomerulus DA3", "glomerulus V", "glomerulus VA3", 
                   "glomerulus VA5", "glomerulus VL1")

# load glom collections and functions--------
fb_gloms = c("glomerulus D", "glomerulus DA1", "glomerulus DA2", 
             "glomerulus DC2", "glomerulus DC3", "glomerulus DL1",
             "glomerulus DL2d", "glomerulus DL2v", "glomerulus DL3", 
             "glomerulus DM2", "glomerulus DM5", "glomerulus DM6",
             "glomerulus VA1d", "glomerulus VA1v", "glomerulus VA7m",
             "glomerulus VC3m", "glomerulus VC4",  "glomerulus VM1",
             "glomerulus VM2", "glomerulus VM3", "glomerulus VM5d",
             "glomerulus VM5v", "glomerulus VM7d", "glomerulus VM7v")

# VM7 is VM7d, VA1lm is VA1v
fc_gloms = c("DA1", "DA2", "DL1", 
             "DL2d", "DL2v", "DL3", 
             "DM5", "DM6", "VA1d", 
             "VA1lm", "VA7m", "VC3m",
             "VC4", "VM1", "VM2",
             "VM5d", "VM5v", "VM7")

# VM7 is VM7d, 1 is VM7v, VA1lm is VA1v
gj_gloms = c("D", "DA1", "DC2", 
             "DC3", "DL1", "DL3", 
             "DM2", "DM5", "DM6",
             "VA1d", "VA1lm", "VA7m", 
             "VM2", "VM3", "VM7", 
             "1")

# setup common glom names for all 3 glom name sources (Jefferis 2007, Flycircuit, FAFB)
all_fb_gloms = c(fb_gloms, fb_gloms_extra)
glom_data = gsub("glomerulus ", "", all_fb_gloms) %>% setNames(all_fb_gloms)
fc_t1 = gsub("VA1lm", "VA1v", fc_gloms) %>% {gsub("VM7", "VM7d", .)} %>% setNames(fc_gloms)
glom_data = c(glom_data, fc_t1, c("1"="VM7v"))

glom_data = setdiff(c(fc_gloms, gj_gloms), names(glom_data)) %>% {setNames(., .)} %>% c(glom_data)

fb_std_gloms = glom_data[all_fb_gloms] %>% unname
fc_std_gloms = glom_data[fc_gloms] %>% unname
gj_std_gloms = glom_data[gj_gloms] %>% unname

# load all data files
getwd() %>%
  file.path("data/") %>%
  {list.files(., full.names=TRUE, pattern ="\\.[Rr]")} %>%
  sapply(load,.GlobalEnv)

uPN = subset(pns, glomerulus != "olfactory_multi_glom_PN")
