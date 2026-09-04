
##Alle Funktionen in kofdata

#library(timeseriesdb)
#library(tstools)
#con <- kofutils::kof_dbconnect()

#keys <- c(
#  "ch.kof.ggu.ng08.fx.q_ql_ass_bs.balance.d11",
#  "ch.kof.inu.ng08.fx.q_ql_ass_bs.balance.d11",
#  "ch.kof.aiu.ng08.fx.q_ql_ass_bs.balance.d11")
#tsl <- timeseriesdb::db_ts_read(con, keys)
#db_collection_add_ts(con, "probeset1", keys, user ="ddiaz")

#db_collection_get_keys(con, "probeset1", "ddiaz")

library(kofdata)

cantons(full = F) #funktioniert 
cantons() #funktioniert 

file <- download_cached_file("ddiaz", "fb0f129e08ff4405bd792af3a9afbd11", "probeset1") #funktioniert nicht 

get_dataset("probeset1", "fb0f129e08ff4405bd792af3a9afbd11", show_progress = TRUE) #funtioniert 

get_legacy_key("ch.kof.ggu.ng08.fx.q_ql_ass_bs.balance.d11") #funktioniert 

get_metadata(keys, "fr") #funktioniert, aber warum keine Metadaten bei INU und AIU?

get_remaining_quota("fb0f129e08ff4405bd792af3a9afbd11") #funktioniert 

get_time_series(keys, "fb0f129e08ff4405bd792af3a9afbd11") #funktioniert

list_available_sets() #funktioniert so nicht 

cachedfiles <- list_cached_files("ddiaz", "fb0f129e08ff4405bd792af3a9afbd11") #funktioniert nicht (username not found)

list_keys_in_set("probeset1") #funktioniert so nicht 

list_public_keys() #funktioniert so nicht 

start_key_explorer() #funktioniert nicht. kommt auf Webseite mit Fehlermeldung "This service is deprecated and out of service."

translate_legacy_keys("chggu_i_f8_s01")






















