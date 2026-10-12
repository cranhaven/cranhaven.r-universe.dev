which_or_NA <- function(v) {
  ind <- which(v)
  if (length(ind) == 0) {ind <- NA_integer_}
  return(ind)
}

convert_unicode_to_ascii <- function(string) {
  old <- paste0("\u00C0\u00C1\u00C2\u00C3\u00C8\u00C9\u00CA\u00CC\u00CD\u00D2",
                "\u00D3\u00D4\u00D5\u00D9\u00DA\u00DD\u00E0\u00E1\u00E2\u00E3",
                "\u00E8\u00E9\u00EA\u00EC\u00ED\u00F2\u00F3\u00F4\u00F5\u00F9",
                "\u00FA\u00FD\u0102\u0103\u0110\u0111\u0128\u0129\u0168\u0169",
                "\u01A0\u01A1\u01AF\u01B0\u1EA0\u1EA1\u1EA2\u1EA3\u1EA4\u1EA5",
                "\u1EA6\u1EA7\u1EA8\u1EA9\u1EAA\u1EAB\u1EAC\u1EAD\u1EAE\u1EAF",
                "\u1EB0\u1EB1\u1EB2\u1EB3\u1EB4\u1EB5\u1EB6\u1EB7\u1EB8\u1EB9",
                "\u1EBA\u1EBB\u1EBC\u1EBD\u1EBE\u1EBF\u1EC0\u1EC1\u1EC2\u1EC3",
                "\u1EC4\u1EC5\u1EC6\u1EC7\u1EC8\u1EC9\u1ECA\u1ECB\u1ECC\u1ECD",
                "\u1ECE\u1ECF\u1ED0\u1ED1\u1ED2\u1ED3\u1ED4\u1ED5\u1ED6\u1ED7",
                "\u1ED8\u1ED9\u1EDA\u1EDB\u1EDC\u1EDD\u1EDE\u1EDF\u1EE0\u1EE1",
                "\u1EE2\u1EE3\u1EE4\u1EE5\u1EE6\u1EE7\u1EE8\u1EE9\u1EEA\u1EEB",
                "\u1EEC\u1EED\u1EEE\u1EEF\u1EF0\u1EF1\u1EF2\u1EF3\u1EF4\u1EF5",
                "\u1EF6\u1EF7\u1EF8\u1EF9\u00d0")
  new <- paste0("AAAAEEEIIO",
                "OOOUUYaaaa",
                "eeeiioooou",
                "uyAaDdIIUu",
                "OoUuAaAaAa",
                "AaAaAaAaAa",
                "AaAaAaAaEe",
                "EeEeEeEeEe",
                "EeEeIiIiOo",
                "OoOoOoOoOo",
                "OoOoOoOoOo",
                "OoUuUuUuUu",
                "UuUuUuYyYy",
                "YyYyD")
  return(chartr(old, new, string))
}
