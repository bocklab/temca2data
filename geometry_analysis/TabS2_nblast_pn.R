
doMC::registerDoMC(7)

# get skids--------
# check with Flycircuit dataset on 2017-03-30
# take out:
# "glomerulus DM3" has multiple neurons in LM data
# Two different tracts of VL2a and VL2p
fb_gloms = c("glomerulus D", "glomerulus DA1", "glomerulus DA2", 
"glomerulus DA4m", 
"glomerulus DC2",
"glomerulus DC3", "glomerulus DL1", "glomerulus DL2d",
"glomerulus DL2v", "glomerulus DL3", "glomerulus DL4",
"glomerulus DM1", "glomerulus DM5",                        
"glomerulus DM6", "glomerulus DP1l", "glomerulus DP1m",
"glomerulus V", "glomerulus VA1d", "glomerulus VA1v",
"glomerulus VA4", "glomerulus VA7m", "glomerulus VC2", 
"glomerulus VC3l", "glomerulus VC3m", "glomerulus VC4",
"glomerulus VM1",
"glomerulus VM2", "glomerulus VM3", "glomerulus VM4",                        
"glomerulus VM5d", "glomerulus VM5v", "glomerulus VM6",
"glomerulus VM7d", "glomerulus VM7v")

fb_pn = subset(pns, glomerulus %in% fb_gloms) %>%
  xform_brain(sample="FAFB13", reference = FCWB) %>%
  mirror_brain(FCWB)

# NBLAST---------
allpndps=flycircuit::load_si_data('allpndps.rds')
t_pns = allpndps[!sapply(attr(allpndps, 'df')$glom, is.na)]
options(nat.default.neuronlist='t_pns')
len = 10

pn_nblast_reverse = lapply(fb_pn, function(sk) nblast_fafb(as.neuronlist(sk), reverse=TRUE, conn=fafb_conn) %>% summary)

summarize_FBnblast <- function(x) {
  t1 = x[1:10, c('muscore','glom')] 
  bind_cols(data.frame(t(t1$muscore)), data.frame(t(t1$glom)))
}

nb_tbl_reverse = lapply(pn_nblast_reverse, summarize_FBnblast) %>%
  {do.call(rbind, .)} %>%
  setNames(c(sapply(seq_len(len), paste0, "_score"), sapply(seq_len(len), paste0, "_glom"))) %>%
  mutate(neuron_name = fb_pn[,'name'], skid=as.integer(names(fb_pn)))

# write.xlsx(nb_tbl_reverse, "170520-STable1_NBLAST_PN_raw.xlsx")

