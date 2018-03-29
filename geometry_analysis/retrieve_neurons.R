
# This is used to re-download neurons from CATMAID
# Normally to replicate the analysis, one doesn't need the script here.
# All neurons and meta-data are pre-saved in the data directory.

# FAFB CATMAID credential is needed for this script, name the CATMAID connection fafb_conn


# retrieve and save all PNs
pn1 = catmaid_query_by_annotation("^right_olfactory_PN$",  type="neuron", conn=fafb_conn)$skid 
pn2 = catmaid_query_by_annotation("^uPN right$",  type="neuron", conn=fafb_conn)$skid
pn_skids = intersect(pn1, pn2)

unigloms = catmaid_query_by_annotation("^uniglom$",  type="annotation", conn=fafb_conn)$name
pns = fetch_neurons(unigloms, conn=fafb_conn, "glomerulus", ref = NULL, intersect_skids = pn_skids) 
multi_glom = fetch_neurons("olfactory_multi_glom_PN", conn=fafb_conn, "glomerulus", ref = NULL) 
pns = c(pns, multi_glom)

# This was used to strip off the synapses
# pns = nlapply(pns, function(n) {n$connectors = NULL; n})

# save(pns, file="pns.RData")

# save meta-data (e.g. annotations) for the PNs 
unigloms = catmaid_query_by_annotation("^uniglom$",  type="annotation", conn=fafb_conn)$name
all_sen = catmaid_query_by_annotation("^sensilla_type$",  type="annotation", conn=fafb_conn)$name
glom_sen = lapply(all_sen, function(sen) catmaid_query_by_annotation(paste0("^", sen, "$"),  type="annotation", conn=fafb_conn)$name)
names(glom_sen) = all_sen
# save(unigloms, all_sen, glom_sen, pn_skids, file = "glom_sen_metaData.RData")


# 180209
# According to Marta C. corrected identification, 
# PN glomerulus DM5 57386 HG (skid 57385) is a multi-glomerular PN
# pns['57385','glomerulus'] = "olfactory_multi_glom_PN"

# skid: 67637
# PN glomerulus DL2d 67638 BH is renamed to PN glomerulus DL2v 67638 BH
# pns['67637','glomerulus'] = "glomerulus DL2v"

# 180209 incoporate identification change from Marta C.
# pns["37250","glomerulus"] = "glomerulus DA4m"
# pns["40749","glomerulus"] = "glomerulus DA4l"
