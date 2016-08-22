plotPredictions <- function(prediction_results, title="Prediction of results", n_sims=1){
    require(reshape2)
    require(ggplot2)


    nhl_predict_expand<-list()

    for (t in c(1:30)){
        nhl_predict_expand[[rownames(prediction_results)[t]]]<-unlist(lapply(c(1:30), function(y) rep(y, prediction_results[t,y])))
    }

    df_results<-as.data.frame(t(as.data.frame(nhl_predict_expand)))
    df_results$Team<-rownames(df_results)
    df_results_m<-melt(df_results, "Team")

    p<-ggplot(df_results_m, aes(x=value)) +
        geom_density(alpha=0.3, adjust=1.5) +
        facet_grid(Team~., scales="free") +
        theme(strip.text.y = element_text(angle=0), strip.background = element_blank()) +
        scale_y_continuous(breaks=NULL) +
        xlab("End-Of-Season Position") +
        ylab("Probability") +
        ggtitle(paste0(title, " after ", n_sims, " simulations"))

    return(p)
}
