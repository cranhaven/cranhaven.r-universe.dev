#include <qpdf/QPDF.hh>
#include <qpdf/QPDFPageDocumentHelper.hh>
#include <qpdf/QPDFWriter.hh>
#include <qpdf/QUtil.hh>
#include <string>
#include <cpp11.hpp>

using namespace cpp11;

static void read_pdf_with_password(char const* infile, char const* password,
                                   QPDF* pdf) {
  try {
    // If the password is an empty string, pass NULL to processFile
    pdf->processFile(infile,
                     (password && strlen(password) > 0) ? password : NULL);
  } catch (const std::exception& e) {
    cpp11::stop("Failed to read PDF file: %s", e.what());
  }
}

[[cpp11::register]] int cpp_pdf_length(char const* infile,
                                       const cpp11::strings& password) {
  QPDF pdf;
  std::string password_str =
      password.size() > 0 ? cpp11::r_string(password.at(0)) : "";
  read_pdf_with_password(infile, password_str.c_str(), &pdf);
  QPDFObjectHandle root = pdf.getRoot();
  QPDFObjectHandle pages = root.getKey("/Pages");
  QPDFObjectHandle count = pages.getKey("/Count");
  return count.getIntValue();
}

[[cpp11::register]] cpp11::strings cpp_pdf_split(char const* infile, std::string outprefix, const cpp11::strings& password){
  QPDF inpdf;
  std::string password_str =
      password.size() > 0 ? cpp11::r_string(password.at(0)) : "";
  read_pdf_with_password(infile, password_str.c_str(), &inpdf);
  std::vector<QPDFPageObjectHelper> pages =  QPDFPageDocumentHelper(inpdf).getAllPages();
  cpp11::writable::strings output(pages.size());
  size_t i, countlen;
  for (i = 0; i < pages.size(); i++) {
    countlen = ceil(log10(pages.size() + 1));
    std::string outfile = outprefix + "_" + QUtil::int_to_string(i+1, countlen) + ".pdf";
    output.at(i) = outfile;
    QPDF outpdf;
    outpdf.emptyPDF();
    QPDFPageDocumentHelper(outpdf).addPage(pages.at(i), false);
    QPDFWriter outpdfw(outpdf, outfile.c_str());
    outpdfw.setStaticID(true); // for testing only
    outpdfw.setStreamDataMode(qpdf_s_preserve);
    outpdfw.write();
  }
  return output;
}

[[cpp11::register]] cpp11::strings cpp_pdf_select(char const* infile, char const* outfile,
                                     cpp11::integers which, const cpp11::strings& password){
  QPDF inpdf;
  std::string password_str =
      password.size() > 0 ? cpp11::r_string(password.at(0)) : "";
  read_pdf_with_password(infile, password_str.c_str(), &inpdf);
  std::vector<QPDFPageObjectHelper> pages =  QPDFPageDocumentHelper(inpdf).getAllPages();
  QPDF outpdf;
  outpdf.emptyPDF();
  for (int i = 0; i < which.size(); i++) {
    int index = which.at(i) -1; //zero index
    QPDFPageDocumentHelper(outpdf).addPage(pages.at(index), false);
  }
  QPDFWriter outpdfw(outpdf, outfile);
  outpdfw.setStaticID(true); // for testing only
  outpdfw.setStreamDataMode(qpdf_s_preserve);
  outpdfw.write();
  return cpp11::writable::strings({outfile});
}

[[cpp11::register]] cpp11::strings cpp_pdf_combine(cpp11::strings infiles,
                                                   char const* outfile,
                                                   const cpp11::strings& password) {
  QPDF outpdf;
  outpdf.emptyPDF();
  for (int i = 0; i < infiles.size(); i++) {
    QPDF inpdf;
    std::string infile_str = cpp11::r_string(infiles.at(i));
    std::string password_str =
        password.size() > 0 ? cpp11::r_string(password.at(0)) : "";
    read_pdf_with_password(infile_str.c_str(), password_str.c_str(), &inpdf);
    std::vector<QPDFPageObjectHelper> pages =
        QPDFPageDocumentHelper(inpdf).getAllPages();
    size_t j;
    for (j = 0; j < pages.size(); j++) {
      QPDFPageDocumentHelper(outpdf).addPage(pages.at(j), false);
    }
  }
  QPDFWriter outpdfw(outpdf, outfile);
  outpdfw.setStaticID(true);  // for testing only
  outpdfw.setStreamDataMode(qpdf_s_preserve);
  outpdfw.write();
  return cpp11::writable::strings({outfile});
}

[[cpp11::register]] cpp11::strings cpp_pdf_compress(char const* infile,
                                                    char const* outfile,
                                                    bool linearize,
                                                    const cpp11::strings& password) {
  QPDF inpdf;
  std::string password_str =
      password.size() > 0 ? cpp11::r_string(password.at(0)) : "";
  read_pdf_with_password(infile, password_str.c_str(), &inpdf);
  QPDFWriter outpdfw(inpdf, outfile);
  outpdfw.setStaticID(true);  // for testing only
  outpdfw.setStreamDataMode(qpdf_s_compress);
  outpdfw.setLinearization(linearize);
  outpdfw.write();
  return cpp11::writable::strings({outfile});
}

[[cpp11::register]] cpp11::strings cpp_pdf_rotate_pages(char const* infile, char const* outfile,
                                           cpp11::integers which, int angle, bool relative,
                                           const cpp11::strings& password){
  QPDF inpdf;
  std::string password_str =
      password.size() > 0 ? cpp11::r_string(password.at(0)) : "";
  read_pdf_with_password(infile, password_str.c_str(), &inpdf);
  std::vector<QPDFPageObjectHelper> pages =  QPDFPageDocumentHelper(inpdf).getAllPages();
  int npages = pages.size();
  QPDF outpdf;
  outpdf.emptyPDF();
  for (int i = 0; i < npages; i++) {
    if (std::find(which.begin(), which.end(), i + 1) != which.end()) {
      pages.at(i).rotatePage(angle, relative);
    }
    QPDFPageDocumentHelper(outpdf).addPage(pages.at(i), false);
  }
  QPDFWriter outpdfw(outpdf, outfile);
  outpdfw.setStaticID(true); // for testing only
  outpdfw.setStreamDataMode(qpdf_s_preserve);
  outpdfw.write();
  return cpp11::writable::strings({outfile});
}

[[cpp11::register]] cpp11::strings cpp_pdf_overlay(char const* infile, char const* stampfile,
                                      char const* outfile, const cpp11::strings& password){
  QPDF inpdf;
  QPDF stamppdf;
  std::string password_str =
      password.size() > 0 ? cpp11::r_string(password.at(0)) : "";
  read_pdf_with_password(infile, password_str.c_str(), &inpdf);
  read_pdf_with_password(stampfile, password_str.c_str(), &stamppdf);

  // Code from: https://github.com/qpdf/qpdf/blob/release-qpdf-8.4.0/examples/pdf-overlay-page.cc
  QPDFPageObjectHelper stamp_page_1 =  QPDFPageDocumentHelper(stamppdf).getAllPages().at(0);
  QPDFObjectHandle foreign_fo = stamp_page_1.getFormXObjectForPage();
  QPDFObjectHandle stamp_fo = inpdf.copyForeignObject(foreign_fo);
  std::vector<QPDFPageObjectHelper> pages = QPDFPageDocumentHelper(inpdf).getAllPages();
  for (std::vector<QPDFPageObjectHelper>::iterator iter = pages.begin(); iter != pages.end(); ++iter) {
    QPDFPageObjectHelper& ph = *iter;
    QPDFObjectHandle resources = ph.getAttribute("/Resources", true);
    int min_suffix = 1;
    std::string name = resources.getUniqueResourceName("/Fx", min_suffix);
    std::string content =
      ph.placeFormXObject(
        stamp_fo, name, ph.getTrimBox().getArrayAsRectangle());
    if (! content.empty()) {
      resources.mergeResources(
        QPDFObjectHandle::parse("<< /XObject << >> >>"));
      resources.getKey("/XObject").replaceKey(name, stamp_fo);
      ph.addPageContents(
        QPDFObjectHandle::newStream(&inpdf, "q\n"), true);
      ph.addPageContents(
        QPDFObjectHandle::newStream(&inpdf, "\nQ\n" + content), false);
    }
  }
  QPDFWriter w(inpdf, outfile);
  w.setStaticID(true);        // for testing only
  w.write();
  return cpp11::writable::strings({outfile});
}

