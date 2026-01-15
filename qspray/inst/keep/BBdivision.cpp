// Rcpp::List BBdivisionRcpp(
//   Rcpp::List Powers, Rcpp::StringVector coeffs, Rcpp::List LTsf, 
//   Rcpp::List gs, Rcpp::List LTgs, int d
// ) {
//   int ngs    = gs.size();
//   int nterms = LTsf.size(); 
//   qspray cur = makeQspray(Powers, coeffs);
//   for(int k = 0; k < nterms; k++) {
//     Rcpp::List LTcur = LTsf(k);
//     int i = 0;
//     while(i < ngs) {
//       Rcpp::List g  = gs(i);
//       qspray gspray = makeQspray(g["powers"], g["coeffs"]);
//       Rcpp::List LTg = LTgs(i);
//       while(divides(LTg, LTcur)) {
//         qspray qtnt = quotient(LTcur, LTg);
//         cur = subtract(cur, prod(qtnt, gspray));
//         if(cur.size() == 0) {
//           return(retval(cur));
//         }
//         LTcur = leadingTerm(cur, d);
//       }
//       i++;
//     }
//   }
//   return retval(cur);
// }
