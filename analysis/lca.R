# pacman::p_load(dplyr, ggplot2, reshape, data.table, poLCA)
# 
# lca_data = cluster_data_parties %>%
#   dplyr::select(lei = is_leisure, bus = is_business, ni = nights)
# 
# lca = poLCA(cbind(lei +1, bus +1, ni)~1,
#             data = lca_data,  
#             nclass = 2, maxiter = 1000)
