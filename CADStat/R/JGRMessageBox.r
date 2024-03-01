# A general Java message box contains information and error message
#' @export
"JGRMessageBox"<-function (w.title="BiologicalInferences: Error",msg="Hello!") {
    a<-.jnew("org.neptuneinc.cadstat.plots.BiologicalInferencesGeneralBox")
    .jcall(a,"Ljavax/swing/JFrame;","getMyGUI",w.title, msg)
}