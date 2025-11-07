setAs("report_mig", "report_mig_interannual", function(from) {
    start_year = new("ref_year")
    end_year = new("ref_year")
    start_year@year_selected = min(get_year(from@timestep))
    end_year@year_selected = max(get_year(from@timestep))
    report_mig_interannual = new("report_mig_interannual")
    report_mig_interannual@dc = from@dc
    report_mig_interannual@taxa = from@taxa
    report_mig_interannual@stage = from@stage
    report_mig_interannual@start_year = start_year
    report_mig_interannual@end_year = end_year
    return(report_mig_interannual)
})


setAs("report_mig_interannual", "report_mig_mult", function(from) {
    report_mig_mult = new("report_mig_mult")
    report_mig_mult@dc = from@dc
    report_mig_mult@taxa = from@taxa
    report_mig_mult@stage = from@stage
    report_mig_mult@timestep@dateDebut = strptime(stringr::str_c(from@start_year@year_selected,
        "-01-01"), format = "%Y-%m-%d")
    report_mig_mult@timestep@nb_step = 364
    return(report_mig_mult)
})


setAs("report_mig", "report_mig_mult", function(from) {
    bMM = new("report_mig_mult")
    bMM@dc = from@dc
    bMM@taxa = from@taxa
    bMM@stage = from@stage
    bMM@timestep = from@timestep
    bMM@coef_conversion = from@coef_conversion
    bMM@data = from@data
    bMM@time.sequence = from@time.sequence
    bMM@calcdata = from@calcdata
    return(bMM)
})

setAs("report_mig_interannual", "report_annual", function(from) {
    r_ann = new("report_annual")
    r_ann@dc = from@dc
    r_ann@taxa = from@taxa
    r_ann@stage = from@stage
    r_ann@start_year = from@start_year
    r_ann@end_year = from@end_year
    return(r_ann)
})
