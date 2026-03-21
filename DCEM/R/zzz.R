.onAttach<-function(libname, pkgname){
package_cite_info<-citation(pkgname)
print_cite_info<-paste(c(format(package_cite_info,"citation")), collapse="\n\n")
packageStartupMessage(print_cite_info)
message = "Please cite us as citation(DCEM)"
#
# message1 = "-----------"
# message2 = "print()"
# message3 = ""
#
#
# packageStartupMessage(message)

# m1 = "  ----------------/       |--|\\~~\\          /~~/|--|     |_____________|      _______________        ____          "
# m2 = " | |-------------/       |  | \\~~\\        /~~/ |  |     |_____________|      _______________        //   \\\\      "
# m3 = " | |                     |  |  \\~~\\      /~~/  |  |     | |                       | |              //      \\\\    "
# m4 = " | |                     |  |   \\~~\\    /~~/   |  |     | |                       | |             //         \\\\"
# m5 = " | |~~~~~~~~~~|          |  |    \\~~\\  /~~/    |  |     | |                       | |            //____________\\\\"
# m6 = " | |~~~~~~~~~~|          |  |     \\__\\/__/     |  |     |-----------| |           | |           //______________\\\\"
# m7 = " | |                     |  |                  |  |     |-----------| |           | |            //                  \\\\"
# m8 = " | |                     |  |                  |  |                 | |           | |           //                     \\\\"
# m9 = " | |                     |  |                  |  |                 | |           | |          //                       \\\\"
# m10 = " | |-------------/       |  |                  |  |     |-----------| |           | |             "
# m11 = " ----------------/       |__|                  |__|     |-----------| |           | |        "


# cite_message = "To cite DCEM, use: citation('DCEM') or as:"
# paper_cit = paste("Parichit Sharma, Hasan Kurban, Mehmet Dalkilic", "DCEM: An R package for clustering big data via data-centric modification of Expectation Maximization,",
#                   "SoftwareX, 17, 100944,", "<https://doi.org/10.1016/j.softx.2021.100944>")
#
# message = paste(m1, "\n", m2, "\n", m3, "\n", m4, "\n", m5, "\n", m6, "\n", m7, "\n",
#        m8, "\n", m9, "\n", m10, "\n", m11, "\n", cite_message, "\n\n" , paper_cit, "")

packageStartupMessage(message)

}
