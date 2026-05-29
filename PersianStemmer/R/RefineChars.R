RefineChars <-
function(texts){
  # removes all unicode chars except Latin, Persian or General Punctuation characters
  texts <- gsub(paste("[\u00A0-\u00AA]",
                      "[\u00AC-\u00BA]",
                      "[\u00BC-\u00D6]",
                      "[\u00D8-\u00F6]",
                      "[\u00F8-\u05FF]",
                      "[\u0700-\u1FFF]",
                      "[\u2070-\uFFFF]",
                      "[\u0600-\u060B]",
                      "[\u060D-\u061A]",
                      "[\u061C-\u061E]",
                      "[\u064B-\u065F]",
                      "[\u066E-\u0670]",
                      "[\u0679-\u067D]",
                      "[\u067F-\u0685]",
                      "[\u0687-\u0697]",
                      "[\u0699-\u06A8]",
                      "[\u06B0-\u06BD]",
                      "[\u06D6-\u06EF]",
                      "[\u06FA-\u06FE]",
                      "\u007F","\u0621",
                      "\u0640","\u0674",
                      "\u06BF",sep = "|"),'',texts)
  # standardizes "alef"
  texts <- gsub(paste("\u0623","\u0625",
                      "[\u0671-\u0673]",
                      "\u0675",sep = "|"),'\u0627', texts)
  # standardizes "he"
  texts <- gsub(paste("\u0629","\u06BE",
                      "[\u06C0-\u06C3]",
                      "\u06D5","\u06FF",sep = "|"),'\u0647', texts)
  # standardizes "vav"
  texts <- gsub(paste("\u0624","\u06CF", 
                      "[\u0676-\u0677]",
                      "[\u06C4-\u06CB]",sep = "|"),'\u0648', texts)
  # standardizes "ye"
  texts <- gsub(paste("[\u063D-\u063F]",
                      "\u0649","\u064A",
                      "[\u06cc-\u06ce]",
                      "[\u06d0-\u06d3]",
                      "\u0620","\u0626",
                      "\u0678",sep = "|"),'\u06CC', texts)
  # standardizes "kaf"
  texts <- gsub(paste("[\u06A9-\u06AE]",
                      "\u063B","\u063C",
                      "\u0643",sep = "|"),'\u06A9', texts)
  # replaces English and Arabic numbers with Persian numbers
  texts <- gsub('1|\u0661','\u06F1', texts) #1
  texts <- gsub('2|\u0662','\u06F2', texts) #2
  texts <- gsub('3|\u0663','\u06F3', texts) #3
  texts <- gsub('4|\u0664','\u06F4', texts) #4
  texts <- gsub('5|\u0665','\u06F5', texts) #5
  texts <- gsub('6|\u0666','\u06F6', texts) #6
  texts <- gsub('7|\u0667','\u06F7', texts) #7
  texts <- gsub('8|\u0668','\u06F8', texts) #8
  texts <- gsub('9|\u0669','\u06F9', texts) #9
  texts <- gsub('0|\u0660','\u06F0', texts) #0
  # removes extra spaces
  return(trim(gsub(" {2,}"," ", texts)))
}
