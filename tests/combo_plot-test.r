# read in test data 
data(joined_data)

# only look at wells 1-8
wells <- c("M-10", "M-11", "M-12", "M-14")
params <- c("Arsenic, dissolved","Boron, dissolved")
gw_data <- subset(joined_data, location_id %in% wells & param_name %in% params)

bck <- c(as.POSIXct("2008-01-01", format="%Y-%m-%d"), as.POSIXct("2009-01-01"), format="%Y-%m-%d")
cmp <- c(as.POSIXct("2010-01-01", format="%Y-%m-%d"), as.POSIXct("2011-01-01"), format="%Y-%m-%d")

ind_by_loc(gw_data)
ind_by_loc(gw_data, back_date = bck)
ind_by_loc(gw_data, limit1 = "intra_upl", limit2 = "inter_upl")
ind_by_loc(gw_data, back_date = bck, comp_date = cmp, limit1 = "intra_upl", limit2 = "inter_upl")


ind_by_param(gw_data)
ind_by_param(gw_data, back_date = bck)
ind_by_param(gw_data, limit1 = "intra_upl", limit2 = "inter_upl")
ind_by_param(gw_data, back_date = bck, comp_date = cmp, limit1 = "intra_upl", limit2 = "inter_upl")

multi_by_param(gw_data)
multi_by_loc(gw_data)

d_ply(gw_data, .(location_id), multi_by_param, .print=TRUE)
d_ply(gw_data, .(param_name), multi_by_loc, .print=TRUE)
