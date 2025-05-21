#include <tesseract/baseapi.h> // tesseract
#include <allheaders.h> // leptonica

#include <memory>
#include <list>
#include <string>
#include <vector>

#include <cpp11.hpp>

inline void tess_finalizer(tesseract::TessBaseAPI* engine) {
  engine->End();
  delete engine;
}

typedef cpp11::external_pointer<tesseract::TessBaseAPI, tess_finalizer> TessPtr;

inline void set_tesseract_options(tesseract::TessBaseAPI* engine,
                                  cpp11::list options) {
  for (int i = 0; i < options.size(); ++i) {
    std::string key = cpp11::as_cpp<std::string>(options.names()[i]);
    std::string value = cpp11::as_cpp<std::string>(options[i]);
    engine->SetVariable(key.c_str(), value.c_str());
  }
}

inline TessPtr make_tess_ptr(tesseract::TessBaseAPI* engine,
                             cpp11::list options = cpp11::list()) {
  set_tesseract_options(engine, options);
  return TessPtr(engine);
}
