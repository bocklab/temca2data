
# This is used to download neurons from CATMAID
# FAFB CATMAID credential is needed, name the CATMAID connection fafb_conn
# normally the code should run with the data in the package without CATMAID connection

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