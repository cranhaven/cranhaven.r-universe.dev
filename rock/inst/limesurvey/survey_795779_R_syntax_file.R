data <- read.csv("survey_795779_R_data_file.csv", quote = "'\"", na.strings=c("", "\"\""), stringsAsFactors=FALSE, fileEncoding="UTF-8-BOM")


<br />
<b>Notice</b>:  Undefined index: interviewtime in <b>/srv/www/htdocs/limesurvey/application/core/plugins/ExportR/RSyntaxWriter.php</b> on line <b>64</b><br />
<br />
<b>Notice</b>:  Undefined index: SPSStype in <b>/srv/www/htdocs/limesurvey/application/core/plugins/ExportR/RSyntaxWriter.php</b> on line <b>76</b><br />
<br />
<b>Notice</b>:  Undefined index: 795779X9184time in <b>/srv/www/htdocs/limesurvey/application/core/plugins/ExportR/RSyntaxWriter.php</b> on line <b>64</b><br />
<br />
<b>Notice</b>:  Undefined index: 795779X9184X113577time in <b>/srv/www/htdocs/limesurvey/application/core/plugins/ExportR/RSyntaxWriter.php</b> on line <b>64</b><br />
<br />
<b>Notice</b>:  Undefined index: 795779X9184X113578time in <b>/srv/www/htdocs/limesurvey/application/core/plugins/ExportR/RSyntaxWriter.php</b> on line <b>64</b><br />
<br />
<b>Notice</b>:  Undefined index: 795779X9184X113579time in <b>/srv/www/htdocs/limesurvey/application/core/plugins/ExportR/RSyntaxWriter.php</b> on line <b>64</b><br />
<br />
<b>Notice</b>:  Undefined index: 795779X9185time in <b>/srv/www/htdocs/limesurvey/application/core/plugins/ExportR/RSyntaxWriter.php</b> on line <b>64</b><br />
<br />
<b>Notice</b>:  Undefined index: SPSStype in <b>/srv/www/htdocs/limesurvey/application/core/plugins/ExportR/RSyntaxWriter.php</b> on line <b>76</b><br />
<br />
<b>Notice</b>:  Undefined index: 795779X9185X113580time in <b>/srv/www/htdocs/limesurvey/application/core/plugins/ExportR/RSyntaxWriter.php</b> on line <b>64</b><br />
<br />
<b>Notice</b>:  Undefined index: 795779X9185X113581time in <b>/srv/www/htdocs/limesurvey/application/core/plugins/ExportR/RSyntaxWriter.php</b> on line <b>64</b><br />
<br />
<b>Notice</b>:  Undefined index: 795779X9185X113583time in <b>/srv/www/htdocs/limesurvey/application/core/plugins/ExportR/RSyntaxWriter.php</b> on line <b>64</b><br />
<br />
<b>Notice</b>:  Undefined index: 795779X9186time in <b>/srv/www/htdocs/limesurvey/application/core/plugins/ExportR/RSyntaxWriter.php</b> on line <b>64</b><br />
<br />
<b>Notice</b>:  Undefined index: SPSStype in <b>/srv/www/htdocs/limesurvey/application/core/plugins/ExportR/RSyntaxWriter.php</b> on line <b>76</b><br />
<br />
<b>Notice</b>:  Undefined index: 795779X9186X113586time in <b>/srv/www/htdocs/limesurvey/application/core/plugins/ExportR/RSyntaxWriter.php</b> on line <b>64</b><br />
<br />
<b>Notice</b>:  Undefined index: 795779X9186X113587time in <b>/srv/www/htdocs/limesurvey/application/core/plugins/ExportR/RSyntaxWriter.php</b> on line <b>64</b><br />
<br />
<b>Notice</b>:  Undefined index: 795779X9186X113588time in <b>/srv/www/htdocs/limesurvey/application/core/plugins/ExportR/RSyntaxWriter.php</b> on line <b>64</b><br />
<br />
<b>Notice</b>:  Undefined index: SPSStype in <b>/srv/www/htdocs/limesurvey/application/core/plugins/ExportR/RSyntaxWriter.php</b> on line <b>76</b><br />
<br />
<b>Notice</b>:  Undefined index: SPSStype in <b>/srv/www/htdocs/limesurvey/application/core/plugins/ExportR/RSyntaxWriter.php</b> on line <b>76</b><br />
<br />
<b>Notice</b>:  Undefined index: SPSStype in <b>/srv/www/htdocs/limesurvey/application/core/plugins/ExportR/RSyntaxWriter.php</b> on line <b>76</b><br />
<br />
<b>Notice</b>:  Undefined index: SPSStype in <b>/srv/www/htdocs/limesurvey/application/core/plugins/ExportR/RSyntaxWriter.php</b> on line <b>76</b><br />
<br />
<b>Notice</b>:  Undefined index: SPSStype in <b>/srv/www/htdocs/limesurvey/application/core/plugins/ExportR/RSyntaxWriter.php</b> on line <b>76</b><br />
<br />
<b>Notice</b>:  Undefined index: SPSStype in <b>/srv/www/htdocs/limesurvey/application/core/plugins/ExportR/RSyntaxWriter.php</b> on line <b>76</b><br />
# LimeSurvey Field type: F
data[, 1] <- as.numeric(data[, 1])
attributes(data)$variable.labels[1] <- "id"
names(data)[1] <- "id"
# LimeSurvey Field type: DATETIME23.2
data[, 2] <- as.character(data[, 2])
attributes(data)$variable.labels[2] <- "submitdate"
names(data)[2] <- "submitdate"
# LimeSurvey Field type: F
data[, 3] <- as.numeric(data[, 3])
attributes(data)$variable.labels[3] <- "lastpage"
names(data)[3] <- "lastpage"
# LimeSurvey Field type: A
data[, 4] <- as.character(data[, 4])
attributes(data)$variable.labels[4] <- "startlanguage"
names(data)[4] <- "startlanguage"
# LimeSurvey Field type: A
data[, 5] <- as.character(data[, 5])
attributes(data)$variable.labels[5] <- "Seed"
names(data)[5] <- "q_"
# LimeSurvey Field type: DATETIME23.2
data[, 6] <- as.character(data[, 6])
attributes(data)$variable.labels[6] <- "startdate"
names(data)[6] <- "startdate"
# LimeSurvey Field type: DATETIME23.2
data[, 7] <- as.character(data[, 7])
attributes(data)$variable.labels[7] <- "datestamp"
names(data)[7] <- "datestamp"
# LimeSurvey Field type: A
data[, 8] <- as.character(data[, 8])
attributes(data)$variable.labels[8] <- ""
names(data)[8] <- "iterationId"
# LimeSurvey Field type: A
data[, 9] <- as.character(data[, 9])
attributes(data)$variable.labels[9] <- ""
names(data)[9] <- "batchId"
# LimeSurvey Field type: A
data[, 10] <- as.character(data[, 10])
attributes(data)$variable.labels[10] <- ""
names(data)[10] <- "populationId"
# LimeSurvey Field type: F
data[, 11] <- as.numeric(data[, 11])
attributes(data)$variable.labels[11] <- "Imagine that this is a very ambiguous question that in fact asks multiple things in one go but isn\'t clear when you should provide which answer, then how would you feel?"
data[, 11] <- factor(data[, 11], levels=c(1,2,3),labels=c("Three cows", "Mostly disagree", "Very much like me"))
names(data)[11] <- "item1"
# LimeSurvey Field type: A
data[, 12] <- as.character(data[, 12])
attributes(data)$variable.labels[12] <- "What does \"ambiguous\" mean to you?"
names(data)[12] <- "item1mq1"
# LimeSurvey Field type: A
data[, 13] <- as.character(data[, 13])
attributes(data)$variable.labels[13] <- "What does \"feel\" mean to you?"
names(data)[13] <- "item1mq2"
# LimeSurvey Field type: F
data[, 14] <- as.numeric(data[, 14])
attributes(data)$variable.labels[14] <- "What is the reason that you don\'t intrinsically like filling out endless questionnaires? Like, what the hell is wrong with you?!?"
data[, 14] <- factor(data[, 14], levels=c(1,2,3),labels=c("Three cows", "A chair", "19 or further"))
names(data)[14] <- "item2"
# LimeSurvey Field type: A
data[, 15] <- as.character(data[, 15])
attributes(data)$variable.labels[15] <- "Why did you give the answer that you gave?"
names(data)[15] <- "item2mq1"
# LimeSurvey Field type: A
data[, 16] <- as.character(data[, 16])
attributes(data)$variable.labels[16] <- "What does \'intrinsically\' really mean, anyway?"
names(data)[16] <- "item2mq2"
<br />
<b>Notice</b>:  Undefined index: SPSStype in <b>/srv/www/htdocs/limesurvey/application/core/plugins/ExportR/RSyntaxWriter.php</b> on line <b>91</b><br />
<br />
<b>Notice</b>:  Undefined index: LStype in <b>/srv/www/htdocs/limesurvey/application/core/plugins/ExportR/RSyntaxWriter.php</b> on line <b>95</b><br />
<br />
<b>Notice</b>:  Undefined index: LStype in <b>/srv/www/htdocs/limesurvey/application/core/plugins/ExportR/RSyntaxWriter.php</b> on line <b>95</b><br />
<br />
<b>Notice</b>:  Undefined index: SPSStype in <b>/srv/www/htdocs/limesurvey/application/core/plugins/ExportR/RSyntaxWriter.php</b> on line <b>100</b><br />
Unknown type