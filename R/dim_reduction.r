# data(cardinal)
# params <- c("Arsenic, dissolved","Boron, dissolved",
#             "Molybdenum, dissolved","Vanadium, dissolved",
#             "Calcium, dissolved", "Potassium, dissolved",
#             "Chloride, total","Alkalinity, total (lab)",
#             "Total Dissolved Solids","Magnesium, dissolved",
#             "Sodium, dissolved","Sulfate, total",
#             "Fluoride, total","Barium, dissolved",
#             "Cadmium, dissolved","Chromium, dissolved",
#             "Iron, dissolved","Lead, dissolved",
#             "Manganese, dissolved","Strontium, dissolved",
#             "Selenium, dissolved","Mercury, dissolved",
#             "Beryllium, dissolved")
# cardinal <- subset(cardinal, param_name %in% params)
# card <- reshape2::dcast(cardinal, value.var="analysis_result", 
#                         location_id + sample_date ~ param_name)
# card <- na.omit(card)
# card <- remove_dup(card)
# 
# colors <- rainbow(length(unique(card$location_id)))
# names(colors) <- unique(card$location_id)
# 
# ecb <- function(x, y){
#   plot(x, t='n')
#   text(x, labels=card$location_id, col=colors[card$location_id])
# }
# 
# tsne_card <- tsne(card[,3:25], epoch_callback=ecb, perplexity=50)
# 
# pca_card <- princomp(card[,3:25])$scores[,1:2]
# plot(pca_card, t='n')
# text(pca_card, labels=card$location_id, col=colors[card$location_id])
# 
# # categorize into leachate and groundwater
# leachate <- c("CA-1","CA-2","CA-3","CA-4","CA-5","Jules Verne","Leachate",
#               "Outfall 019","Underdrain","Face of Dam","Impact basin")
# card$group <- ifelse(card$location_id %in% leachate, "leachate","groundwater")
# 
# colors2 <- rainbow(length(unique(card$group)))
# names(colors2) <- unique(card$group)
# 
# ecb2 <- function(x,y){
#   plot(x, t='n')
#   text(x, labels=card$group, col=colors[card$group])
# }
# 
# tsne_card2 <- tsne(card[,3:25], initial_dims=20, epoch_callback=ecb2, perplexity=10)
