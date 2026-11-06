// Native Rcpp bindings to the LadybugDB v0.20 C API.

#ifdef __clang__
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-warning-option"
#endif

#include <Rcpp.h>

#ifdef __clang__
# pragma clang diagnostic pop
#endif

#include <lbug.h>

#include <algorithm>
#include <cmath>
#include <cstdint>
#include <cstring>
#include <limits>
#include <sstream>
#include <string>
#include <vector>

using namespace Rcpp;

// ---------------------------------------------------------------------------
// Error and handle helpers
// ---------------------------------------------------------------------------

static std::string take_last_error(const std::string& fallback) {
  char* message = lbug_get_last_error();
  if (message == nullptr) return fallback;
  std::string out(message);
  lbug_destroy_string(message);
  return out.empty() ? fallback : out;
}

static std::string result_error(lbug_query_result* result,
                                const std::string& fallback) {
  if (result != nullptr && result->_query_result != nullptr) {
    char* message = lbug_query_result_get_error_message(result);
    if (message != nullptr) {
      std::string out(message);
      lbug_destroy_string(message);
      if (!out.empty()) return out;
    }
  }
  return take_last_error(fallback);
}

static void require_success(lbug_state state, const std::string& fallback) {
  if (state != LbugSuccess) Rcpp::stop(take_last_error(fallback));
}

struct LbDatabase {
  lbug_database handle{};
  uint64_t active_connections = 0;
  bool closed = false;

  ~LbDatabase() {
    if (!closed) {
      lbug_database_destroy(&handle);
      closed = true;
    }
  }
};

struct LbConnection {
  lbug_connection handle{};
  LbDatabase* database = nullptr;
  uint64_t active_results = 0;
  bool closed = false;

  explicit LbConnection(LbDatabase* db) : database(db) {}

  void close() {
    if (closed) return;
    lbug_connection_destroy(&handle);
    closed = true;
    if (database != nullptr && database->active_connections > 0) {
      database->active_connections--;
    }
  }

  ~LbConnection() { close(); }
};

struct LbResult {
  lbug_query_result handle{};
  LbConnection* connection = nullptr;
  uint64_t position = 0;
  bool closed = false;

  explicit LbResult(LbConnection* conn) : connection(conn) {
    if (connection != nullptr) connection->active_results++;
  }

  void close() {
    if (closed) return;
    lbug_query_result_destroy(&handle);
    closed = true;
    if (connection != nullptr && connection->active_results > 0) {
      connection->active_results--;
    }
  }

  ~LbResult() { close(); }
};

static LbDatabase* checked_database(SEXP pointer, bool allow_closed = false) {
  if (TYPEOF(pointer) != EXTPTRSXP || R_ExternalPtrAddr(pointer) == nullptr) {
    Rcpp::stop("Invalid LadybugDB database handle.");
  }
  XPtr<LbDatabase> database(pointer);
  if (!allow_closed && database->closed) Rcpp::stop("Database has been closed.");
  return database.get();
}

static LbConnection* checked_connection(SEXP pointer, bool allow_closed = false) {
  if (TYPEOF(pointer) != EXTPTRSXP || R_ExternalPtrAddr(pointer) == nullptr) {
    Rcpp::stop("Invalid LadybugDB connection handle.");
  }
  XPtr<LbConnection> connection(pointer);
  if (!allow_closed && connection->closed) Rcpp::stop("Connection has been closed.");
  if (!allow_closed && connection->database != nullptr && connection->database->closed) {
    Rcpp::stop("The database owning this connection has been closed.");
  }
  return connection.get();
}

static LbResult* checked_result(SEXP pointer, bool allow_closed = false) {
  if (TYPEOF(pointer) != EXTPTRSXP || R_ExternalPtrAddr(pointer) == nullptr) {
    Rcpp::stop("Invalid LadybugDB result handle.");
  }
  XPtr<LbResult> result(pointer);
  if (!allow_closed && result->closed) Rcpp::stop("Query result has been closed.");
  return result.get();
}

static SEXP wrap_result(LbResult* result) {
  return XPtr<LbResult>(result, true);
}

static SEXP finish_result(LbResult* result, lbug_state state,
                          const std::string& fallback) {
  if (state != LbugSuccess || !lbug_query_result_is_success(&result->handle)) {
    std::string message = result_error(&result->handle, fallback);
    delete result;
    Rcpp::stop(message);
  }
  return wrap_result(result);
}

// ---------------------------------------------------------------------------
// Database and connection lifecycle
// ---------------------------------------------------------------------------

// [[Rcpp::export]]
SEXP lb_database_open(std::string path, Rcpp::List config) {
  LbDatabase* database = new LbDatabase();
  lbug_system_config native = lbug_default_system_config();

  if (config.containsElementNamed("buffer_pool_size"))
    native.buffer_pool_size = Rcpp::as<uint64_t>(config["buffer_pool_size"]);
  if (config.containsElementNamed("max_num_threads"))
    native.max_num_threads = Rcpp::as<uint64_t>(config["max_num_threads"]);
  if (config.containsElementNamed("enable_compression"))
    native.enable_compression = Rcpp::as<bool>(config["enable_compression"]);
  if (config.containsElementNamed("read_only"))
    native.read_only = Rcpp::as<bool>(config["read_only"]);
  if (config.containsElementNamed("max_db_size"))
    native.max_db_size = Rcpp::as<uint64_t>(config["max_db_size"]);
  if (config.containsElementNamed("auto_checkpoint"))
    native.auto_checkpoint = Rcpp::as<bool>(config["auto_checkpoint"]);
  if (config.containsElementNamed("checkpoint_threshold"))
    native.checkpoint_threshold = Rcpp::as<uint64_t>(config["checkpoint_threshold"]);
  if (config.containsElementNamed("throw_on_wal_replay_failure"))
    native.throw_on_wal_replay_failure =
      Rcpp::as<bool>(config["throw_on_wal_replay_failure"]);
  if (config.containsElementNamed("enable_checksums"))
    native.enable_checksums = Rcpp::as<bool>(config["enable_checksums"]);
  if (config.containsElementNamed("enable_multi_writes"))
    native.enable_multi_writes = Rcpp::as<bool>(config["enable_multi_writes"]);
  if (config.containsElementNamed("enable_default_hash_index"))
    native.enable_default_hash_index =
      Rcpp::as<bool>(config["enable_default_hash_index"]);
#if defined(__APPLE__)
  if (config.containsElementNamed("thread_qos"))
    native.thread_qos = Rcpp::as<uint32_t>(config["thread_qos"]);
#endif

  lbug_state state = lbug_database_init(path.c_str(), native, &database->handle);
  if (state != LbugSuccess) {
    std::string message = take_last_error("Failed to open database: " + path);
    delete database;
    if (message.find("version") != std::string::npos ||
        message.find("Version") != std::string::npos) {
      message += " Export the database with its original LadybugDB version, then import it with "
                 "v0.20.4 (see `vignette(\"storage-migration\")`).";
    }
    Rcpp::stop(message);
  }
  return XPtr<LbDatabase>(database, true);
}

// [[Rcpp::export]]
void lb_database_close(SEXP pointer) {
  LbDatabase* database = checked_database(pointer, true);
  if (database->closed) return;
  if (database->active_connections > 0) {
    Rcpp::stop("Cannot close database while %llu connection(s) are still open.",
               static_cast<unsigned long long>(database->active_connections));
  }
  lbug_database_destroy(&database->handle);
  database->closed = true;
}

// [[Rcpp::export]]
bool lb_database_is_open(SEXP pointer) {
  return !checked_database(pointer, true)->closed;
}

// [[Rcpp::export]]
std::string lb_database_version() {
  char* version = lbug_get_version();
  if (version == nullptr) return "unknown";
  std::string out(version);
  lbug_destroy_string(version);
  return out;
}

// [[Rcpp::export]]
double lb_database_storage_version() {
  return static_cast<double>(lbug_get_storage_version());
}

// [[Rcpp::export]]
SEXP lb_connection_create(SEXP database_pointer, int num_threads) {
  LbDatabase* database = checked_database(database_pointer);
  LbConnection* connection = new LbConnection(database);
  lbug_state state = lbug_connection_init(&database->handle, &connection->handle);
  if (state != LbugSuccess) {
    std::string message = take_last_error("Failed to create connection.");
    delete connection;
    Rcpp::stop(message);
  }
  database->active_connections++;
  if (num_threads > 0) {
    state = lbug_connection_set_max_num_thread_for_exec(
      &connection->handle, static_cast<uint64_t>(num_threads));
    if (state != LbugSuccess) {
      std::string message = take_last_error("Failed to set connection thread count.");
      delete connection;
      Rcpp::stop(message);
    }
  }
  return XPtr<LbConnection>(connection, true);
}

// [[Rcpp::export]]
void lb_connection_close(SEXP pointer) {
  LbConnection* connection = checked_connection(pointer, true);
  if (connection->closed) return;
  if (connection->active_results > 0) {
    Rcpp::stop("Cannot close connection while %llu query result(s) are still open. "
               "Call `lb_close()` on each result first.",
               static_cast<unsigned long long>(connection->active_results));
  }
  connection->close();
}

// [[Rcpp::export]]
bool lb_connection_is_open(SEXP pointer) {
  return !checked_connection(pointer, true)->closed;
}

// [[Rcpp::export]]
void lb_connection_interrupt_c(SEXP pointer) {
  LbConnection* connection = checked_connection(pointer);
  lbug_connection_interrupt(&connection->handle);
}

// [[Rcpp::export]]
void lb_connection_set_timeout_c(SEXP pointer, double milliseconds) {
  LbConnection* connection = checked_connection(pointer);
  require_success(
    lbug_connection_set_query_timeout(&connection->handle,
      static_cast<uint64_t>(milliseconds)),
    "Failed to set query timeout."
  );
}

// ---------------------------------------------------------------------------
// R values to prepared-statement values
// ---------------------------------------------------------------------------

static bool is_na_scalar(SEXP value) {
  if (Rf_length(value) != 1) return false;
  switch (TYPEOF(value)) {
    case LGLSXP: return LOGICAL(value)[0] == NA_LOGICAL;
    case INTSXP: return INTEGER(value)[0] == NA_INTEGER;
    case REALSXP: return R_IsNA(REAL(value)[0]);
    case STRSXP: return STRING_ELT(value, 0) == NA_STRING;
    default: return false;
  }
}

static lbug_value* r_to_lbug_value(SEXP value, const std::string& parameter);

static void destroy_values(std::vector<lbug_value*>& values) {
  for (lbug_value* value : values) {
    if (value != nullptr) lbug_value_destroy(value);
  }
}

static lbug_value* finish_list_value(std::vector<lbug_value*>& values,
                                     const std::string& parameter) {
  lbug_value* out = nullptr;
  lbug_state state = lbug_value_create_list(values.size(),
    values.empty() ? nullptr : values.data(), &out);
  destroy_values(values);
  if (state != LbugSuccess || out == nullptr) {
    Rcpp::stop("Failed to create list parameter '%s': %s", parameter.c_str(),
               take_last_error("incompatible nested value types").c_str());
  }
  return out;
}

static lbug_value* make_list_value(const std::vector<SEXP>& input,
                                   const std::string& parameter) {
  std::vector<lbug_value*> values;
  values.reserve(input.size());
  try {
    for (SEXP item : input) values.push_back(r_to_lbug_value(item, parameter));
  } catch (...) {
    destroy_values(values);
    throw;
  }
  return finish_list_value(values, parameter);
}

static lbug_value* r_to_lbug_value(SEXP value, const std::string& parameter) {
  if (Rf_isNull(value) || is_na_scalar(value)) return lbug_value_create_null();

  if (Rf_inherits(value, "Date")) {
    double days = Rcpp::as<double>(value);
    if (!R_FINITE(days)) return lbug_value_create_null();
    lbug_date_t date{static_cast<int32_t>(days)};
    return lbug_value_create_date(date);
  }

  if (Rf_inherits(value, "POSIXct")) {
    double seconds = Rcpp::as<double>(value);
    if (!R_FINITE(seconds)) return lbug_value_create_null();
    lbug_timestamp_t timestamp{
      static_cast<int64_t>(std::llround(seconds * 1000000.0))};
    return lbug_value_create_timestamp(timestamp);
  }

  if (Rf_inherits(value, "integer64") && TYPEOF(value) == REALSXP &&
      Rf_length(value) == 1) {
    int64_t integer = 0;
    std::memcpy(&integer, REAL(value), sizeof(integer));
    if (integer == std::numeric_limits<int64_t>::min()) return lbug_value_create_null();
    return lbug_value_create_int64(integer);
  }

  if (Rf_isFactor(value) && Rf_length(value) == 1) {
    CharacterVector text = Rcpp::as<CharacterVector>(value);
    return lbug_value_create_string(CHAR(text[0]));
  }

  if (TYPEOF(value) == RAWSXP) {
    RawVector bytes(value);
    // The C API has no BLOB constructor. Its documented STRING -> BLOB cast
    // accepts the same escaped byte representation as BLOB('\\xAA...').
    static const char hex[] = "0123456789ABCDEF";
    std::string escaped;
    escaped.reserve(bytes.size() * 4);
    for (R_xlen_t i = 0; i < bytes.size(); ++i) {
      escaped += "\\x";
      escaped += hex[(bytes[i] >> 4) & 0x0f];
      escaped += hex[bytes[i] & 0x0f];
    }
    return lbug_value_create_string(escaped.c_str());
  }

  if (TYPEOF(value) == VECSXP) {
    List list(value);
    SEXP names_sexp = Rf_getAttrib(value, R_NamesSymbol);
    CharacterVector names = Rf_isNull(names_sexp)
      ? CharacterVector(0) : CharacterVector(names_sexp);
    bool named = !Rf_isNull(names_sexp) && names.size() == list.size() && list.size() > 0;
    if (named) {
      for (R_xlen_t i = 0; i < names.size(); ++i) {
        if (names[i] == NA_STRING || Rcpp::as<std::string>(names[i]).empty()) {
          named = false;
          break;
        }
      }
    }
    if (named) {
      std::vector<std::string> name_storage;
      std::vector<const char*> field_names;
      std::vector<lbug_value*> field_values;
      name_storage.reserve(list.size());
      field_names.reserve(list.size());
      field_values.reserve(list.size());
      try {
        for (R_xlen_t i = 0; i < list.size(); ++i) {
          name_storage.push_back(Rcpp::as<std::string>(names[i]));
          field_values.push_back(r_to_lbug_value(list[i], parameter));
        }
      } catch (...) {
        destroy_values(field_values);
        throw;
      }
      for (const std::string& name : name_storage) field_names.push_back(name.c_str());
      lbug_value* out = nullptr;
      lbug_state state = lbug_value_create_struct(list.size(), field_names.data(),
                                                   field_values.data(), &out);
      destroy_values(field_values);
      if (state != LbugSuccess || out == nullptr) {
        Rcpp::stop("Failed to create struct parameter '%s': %s", parameter.c_str(),
                   take_last_error("invalid struct value").c_str());
      }
      return out;
    }
    std::vector<SEXP> elements;
    elements.reserve(list.size());
    for (R_xlen_t i = 0; i < list.size(); ++i) elements.push_back(list[i]);
    return make_list_value(elements, parameter);
  }

  if (Rf_length(value) > 1 &&
      (TYPEOF(value) == LGLSXP || TYPEOF(value) == INTSXP ||
       TYPEOF(value) == REALSXP || TYPEOF(value) == STRSXP)) {
    std::vector<lbug_value*> elements;
    elements.reserve(Rf_xlength(value));
    try {
      for (R_xlen_t i = 0; i < Rf_xlength(value); ++i) {
        lbug_value* element = nullptr;
        switch (TYPEOF(value)) {
          case LGLSXP:
            element = LOGICAL(value)[i] == NA_LOGICAL
              ? lbug_value_create_null()
              : lbug_value_create_bool(LOGICAL(value)[i] == TRUE);
            break;
          case INTSXP:
            element = INTEGER(value)[i] == NA_INTEGER
              ? lbug_value_create_null()
              : lbug_value_create_int64(static_cast<int64_t>(INTEGER(value)[i]));
            break;
          case REALSXP:
            element = R_IsNA(REAL(value)[i])
              ? lbug_value_create_null()
              : lbug_value_create_double(REAL(value)[i]);
            break;
          case STRSXP:
            element = STRING_ELT(value, i) == NA_STRING
              ? lbug_value_create_null()
              : lbug_value_create_string(CHAR(STRING_ELT(value, i)));
            break;
        }
        elements.push_back(element);
      }
    } catch (...) {
      destroy_values(elements);
      throw;
    }
    return finish_list_value(elements, parameter);
  }

  if (TYPEOF(value) == LGLSXP && Rf_length(value) == 1)
    return lbug_value_create_bool(LOGICAL(value)[0] == TRUE);
  if (TYPEOF(value) == INTSXP && Rf_length(value) == 1)
    return lbug_value_create_int64(static_cast<int64_t>(INTEGER(value)[0]));
  if (TYPEOF(value) == REALSXP && Rf_length(value) == 1)
    return lbug_value_create_double(REAL(value)[0]);
  if (TYPEOF(value) == STRSXP && Rf_length(value) == 1) {
    const char* text = CHAR(STRING_ELT(value, 0));
    if (Rf_inherits(value, "json")) return lbug_value_create_json(text);
    return lbug_value_create_string(text);
  }

  Rcpp::stop("Unsupported value for parameter '%s'.", parameter.c_str());
  return nullptr;
}

// ---------------------------------------------------------------------------
// Query execution
// ---------------------------------------------------------------------------

// [[Rcpp::export]]
SEXP lb_connection_execute(SEXP connection_pointer, std::string query) {
  LbConnection* connection = checked_connection(connection_pointer);
  LbResult* result = new LbResult(connection);
  lbug_state state = lbug_connection_query(&connection->handle, query.c_str(),
                                            &result->handle);
  return finish_result(result, state, "Query failed.");
}

// [[Rcpp::export]]
SEXP lb_connection_execute_params(SEXP connection_pointer, std::string query,
                                  Rcpp::List parameters) {
  LbConnection* connection = checked_connection(connection_pointer);
  lbug_prepared_statement statement{};
  lbug_state state = lbug_connection_prepare(&connection->handle, query.c_str(), &statement);
  if (state != LbugSuccess || !lbug_prepared_statement_is_success(&statement)) {
    char* message = lbug_prepared_statement_get_error_message(&statement);
    std::string error = message == nullptr ? take_last_error("Failed to prepare query.")
                                           : std::string(message);
    if (message != nullptr) lbug_destroy_string(message);
    lbug_prepared_statement_destroy(&statement);
    Rcpp::stop(error);
  }

  CharacterVector names = parameters.names();
  for (R_xlen_t i = 0; i < parameters.size(); ++i) {
    std::string name = Rcpp::as<std::string>(names[i]);
    lbug_value* value = nullptr;
    try {
      value = r_to_lbug_value(parameters[i], name);
      if (value == nullptr) {
        Rcpp::stop("Failed to construct parameter '%s'.", name.c_str());
      }
      state = lbug_prepared_statement_bind_value(&statement, name.c_str(), value);
      lbug_value_destroy(value);
      value = nullptr;
      if (state != LbugSuccess) {
        Rcpp::stop("Failed to bind parameter '%s': %s", name.c_str(),
                   take_last_error("type mismatch").c_str());
      }
    } catch (...) {
      if (value != nullptr) lbug_value_destroy(value);
      lbug_prepared_statement_destroy(&statement);
      throw;
    }
  }

  LbResult* result = new LbResult(connection);
  state = lbug_connection_execute(&connection->handle, &statement, &result->handle);
  lbug_prepared_statement_destroy(&statement);
  return finish_result(result, state, "Failed to execute prepared query.");
}

// ---------------------------------------------------------------------------
// Result metadata and conversion
// ---------------------------------------------------------------------------

static const char* type_name(lbug_data_type_id type) {
  // JSON exists in LadybugDB 0.20.4 as id 60 but is omitted from lbug.h's
  // public lbug_data_type_id enumeration.
  if (static_cast<int>(type) == 60) return "JSON";
  switch (type) {
    case LBUG_ANY: return "ANY";
    case LBUG_NODE: return "NODE";
    case LBUG_REL: return "REL";
    case LBUG_RECURSIVE_REL: return "RECURSIVE_REL";
    case LBUG_SERIAL: return "SERIAL";
    case LBUG_BOOL: return "BOOL";
    case LBUG_INT64: return "INT64";
    case LBUG_INT32: return "INT32";
    case LBUG_INT16: return "INT16";
    case LBUG_INT8: return "INT8";
    case LBUG_UINT64: return "UINT64";
    case LBUG_UINT32: return "UINT32";
    case LBUG_UINT16: return "UINT16";
    case LBUG_UINT8: return "UINT8";
    case LBUG_INT128: return "INT128";
    case LBUG_DOUBLE: return "DOUBLE";
    case LBUG_FLOAT: return "FLOAT";
    case LBUG_DATE: return "DATE";
    case LBUG_TIMESTAMP: return "TIMESTAMP";
    case LBUG_TIMESTAMP_SEC: return "TIMESTAMP_SEC";
    case LBUG_TIMESTAMP_MS: return "TIMESTAMP_MS";
    case LBUG_TIMESTAMP_NS: return "TIMESTAMP_NS";
    case LBUG_TIMESTAMP_TZ: return "TIMESTAMP_TZ";
    case LBUG_INTERVAL: return "INTERVAL";
    case LBUG_DECIMAL: return "DECIMAL";
    case LBUG_INTERNAL_ID: return "INTERNAL_ID";
    case LBUG_STRING: return "STRING";
    case LBUG_BLOB: return "BLOB";
    case LBUG_LIST: return "LIST";
    case LBUG_ARRAY: return "ARRAY";
    case LBUG_STRUCT: return "STRUCT";
    case LBUG_MAP: return "MAP";
    case LBUG_UNION: return "UNION";
    case LBUG_POINTER: return "POINTER";
    case LBUG_UUID: return "UUID";
    default: return "UNKNOWN";
  }
}

static lbug_data_type_id value_type(lbug_value* value) {
  lbug_logical_type logical{};
  lbug_value_get_data_type(value, &logical);
  lbug_data_type_id type = lbug_data_type_get_id(&logical);
  lbug_data_type_destroy(&logical);
  return type;
}

static std::string int64_string(int64_t value) {
  std::ostringstream stream;
  stream << value;
  return stream.str();
}

static std::string uint64_string(uint64_t value) {
  std::ostringstream stream;
  stream << value;
  return stream.str();
}

static SEXP int64_scalar(int64_t value, const std::string& mode) {
  if (mode == "character") return Rf_mkString(int64_string(value).c_str());
  if (mode == "integer64") {
    NumericVector out(1);
    std::memcpy(REAL(out), &value, sizeof(value));
    out.attr("class") = "integer64";
    return out;
  }
  return Rf_ScalarReal(static_cast<double>(value));
}

static SEXP uint64_scalar(uint64_t value, const std::string& mode) {
  // bit64 has no unsigned representation, so character is the only exact
  // policy that is safe across the full UINT64 domain.
  if (mode == "character" || mode == "integer64") {
    return Rf_mkString(uint64_string(value).c_str());
  }
  return Rf_ScalarReal(static_cast<double>(value));
}

static SEXP value_to_r(lbug_value* value, const std::string& bigint);

static SEXP internal_id_to_r(lbug_value* value) {
  lbug_internal_id_t id{};
  require_success(lbug_value_get_internal_id(value, &id),
                  "Failed to read internal identifier.");
  return List::create(
    Named("table") = uint64_string(id.table_id),
    Named("offset") = uint64_string(id.offset)
  );
}

static SEXP struct_to_r(lbug_value* value, const std::string& bigint) {
  uint64_t size = 0;
  require_success(lbug_value_get_struct_num_fields(value, &size),
                  "Failed to read struct size.");
  List out(size);
  CharacterVector names(size);
  for (uint64_t i = 0; i < size; ++i) {
    char* name = nullptr;
    require_success(lbug_value_get_struct_field_name(value, i, &name),
                    "Failed to read struct field name.");
    names[i] = name == nullptr ? "" : name;
    if (name != nullptr) lbug_destroy_string(name);
    lbug_value field{};
    require_success(lbug_value_get_struct_field_value(value, i, &field),
                    "Failed to read struct field.");
    out[i] = value_to_r(&field, bigint);
    lbug_value_destroy(&field);
  }
  out.names() = names;
  return out;
}

static SEXP union_to_r(lbug_value* value, const std::string& bigint) {
  // A union stores only its active value as child zero. The 0.20.4 C API does
  // not expose the active field's tag, so preserve the value explicitly and
  // let callers query union_tag() in Cypher when they also need its label.
  lbug_value active{};
  require_success(lbug_value_get_struct_field_value(value, 0, &active),
                  "Failed to read UNION value.");
  List out = List::create(Named("value") = value_to_r(&active, bigint));
  out.attr("class") = "lb_union";
  lbug_value_destroy(&active);
  return out;
}

static SEXP value_to_r(lbug_value* value, const std::string& bigint) {
  if (lbug_value_is_null(value)) return R_NilValue;
  lbug_data_type_id type = value_type(value);
  switch (type) {
    case LBUG_BOOL: {
      bool out = false;
      require_success(lbug_value_get_bool(value, &out), "Failed to read BOOL.");
      return Rf_ScalarLogical(out ? TRUE : FALSE);
    }
    case LBUG_INT8: {
      int8_t out = 0;
      require_success(lbug_value_get_int8(value, &out), "Failed to read INT8.");
      return Rf_ScalarInteger(out);
    }
    case LBUG_INT16: {
      int16_t out = 0;
      require_success(lbug_value_get_int16(value, &out), "Failed to read INT16.");
      return Rf_ScalarInteger(out);
    }
    case LBUG_INT32: {
      int32_t out = 0;
      require_success(lbug_value_get_int32(value, &out), "Failed to read INT32.");
      return Rf_ScalarInteger(out);
    }
    case LBUG_INT64:
    case LBUG_SERIAL: {
      int64_t out = 0;
      require_success(lbug_value_get_int64(value, &out), "Failed to read INT64.");
      return int64_scalar(out, bigint);
    }
    case LBUG_UINT8: {
      uint8_t out = 0;
      require_success(lbug_value_get_uint8(value, &out), "Failed to read UINT8.");
      return Rf_ScalarInteger(out);
    }
    case LBUG_UINT16: {
      uint16_t out = 0;
      require_success(lbug_value_get_uint16(value, &out), "Failed to read UINT16.");
      return Rf_ScalarInteger(out);
    }
    case LBUG_UINT32: {
      uint32_t out = 0;
      require_success(lbug_value_get_uint32(value, &out), "Failed to read UINT32.");
      return Rf_ScalarReal(static_cast<double>(out));
    }
    case LBUG_UINT64: {
      uint64_t out = 0;
      require_success(lbug_value_get_uint64(value, &out), "Failed to read UINT64.");
      return uint64_scalar(out, bigint);
    }
    case LBUG_INT128: {
      lbug_int128_t integer{};
      char* text = nullptr;
      require_success(lbug_value_get_int128(value, &integer), "Failed to read INT128.");
      require_success(lbug_int128_t_to_string(integer, &text), "Failed to format INT128.");
      SEXP out = Rf_mkString(text == nullptr ? "" : text);
      if (text != nullptr) lbug_destroy_string(text);
      return out;
    }
    case LBUG_FLOAT: {
      float out = 0;
      require_success(lbug_value_get_float(value, &out), "Failed to read FLOAT.");
      return Rf_ScalarReal(out);
    }
    case LBUG_DOUBLE: {
      double out = 0;
      require_success(lbug_value_get_double(value, &out), "Failed to read DOUBLE.");
      return Rf_ScalarReal(out);
    }
    case LBUG_STRING: {
      char* text = nullptr;
      require_success(lbug_value_get_string(value, &text), "Failed to read STRING.");
      SEXP out = Rf_mkString(text == nullptr ? "" : text);
      if (text != nullptr) lbug_destroy_string(text);
      return out;
    }
    case LBUG_UUID: {
      char* text = nullptr;
      require_success(lbug_value_get_uuid(value, &text), "Failed to read UUID.");
      SEXP out = Rf_mkString(text == nullptr ? "" : text);
      if (text != nullptr) lbug_destroy_string(text);
      return out;
    }
    case LBUG_BLOB: {
      uint8_t* bytes = nullptr;
      uint64_t size = 0;
      require_success(lbug_value_get_blob(value, &bytes, &size), "Failed to read BLOB.");
      RawVector out(size);
      if (size > 0 && bytes != nullptr) std::memcpy(RAW(out), bytes, size);
      if (bytes != nullptr) lbug_destroy_blob(bytes);
      return out;
    }
    case LBUG_DATE: {
      lbug_date_t date{};
      require_success(lbug_value_get_date(value, &date), "Failed to read DATE.");
      NumericVector out = NumericVector::create(static_cast<double>(date.days));
      out.attr("class") = "Date";
      return out;
    }
    case LBUG_TIMESTAMP: {
      lbug_timestamp_t timestamp{};
      require_success(lbug_value_get_timestamp(value, &timestamp),
                      "Failed to read TIMESTAMP.");
      NumericVector out = NumericVector::create(timestamp.value / 1000000.0);
      out.attr("class") = CharacterVector::create("POSIXct", "POSIXt");
      out.attr("tzone") = "UTC";
      return out;
    }
    case LBUG_TIMESTAMP_TZ: {
      lbug_timestamp_tz_t timestamp{};
      require_success(lbug_value_get_timestamp_tz(value, &timestamp),
                      "Failed to read TIMESTAMP_TZ.");
      NumericVector out = NumericVector::create(timestamp.value / 1000000.0);
      out.attr("class") = CharacterVector::create("POSIXct", "POSIXt");
      out.attr("tzone") = "UTC";
      return out;
    }
    case LBUG_TIMESTAMP_NS: {
      lbug_timestamp_ns_t timestamp{};
      require_success(lbug_value_get_timestamp_ns(value, &timestamp),
                      "Failed to read TIMESTAMP_NS.");
      NumericVector out = NumericVector::create(timestamp.value / 1000000000.0);
      out.attr("class") = CharacterVector::create("POSIXct", "POSIXt");
      out.attr("tzone") = "UTC";
      return out;
    }
    case LBUG_TIMESTAMP_MS: {
      lbug_timestamp_ms_t timestamp{};
      require_success(lbug_value_get_timestamp_ms(value, &timestamp),
                      "Failed to read TIMESTAMP_MS.");
      NumericVector out = NumericVector::create(timestamp.value / 1000.0);
      out.attr("class") = CharacterVector::create("POSIXct", "POSIXt");
      out.attr("tzone") = "UTC";
      return out;
    }
    case LBUG_TIMESTAMP_SEC: {
      lbug_timestamp_sec_t timestamp{};
      require_success(lbug_value_get_timestamp_sec(value, &timestamp),
                      "Failed to read TIMESTAMP_SEC.");
      NumericVector out = NumericVector::create(static_cast<double>(timestamp.value));
      out.attr("class") = CharacterVector::create("POSIXct", "POSIXt");
      out.attr("tzone") = "UTC";
      return out;
    }
    case LBUG_INTERVAL: {
      lbug_interval_t interval{};
      require_success(lbug_value_get_interval(value, &interval), "Failed to read INTERVAL.");
      NumericVector out = NumericVector::create(
        Named("months") = interval.months,
        Named("days") = interval.days,
        Named("microseconds") = static_cast<double>(interval.micros)
      );
      out.attr("class") = "lb_interval";
      return out;
    }
    case LBUG_DECIMAL: {
      char* text = nullptr;
      require_success(lbug_value_get_decimal_as_string(value, &text),
                      "Failed to read DECIMAL.");
      SEXP out = Rf_mkString(text == nullptr ? "" : text);
      if (text != nullptr) lbug_destroy_string(text);
      return out;
    }
    case LBUG_INTERNAL_ID:
      return internal_id_to_r(value);
    case LBUG_LIST:
    case LBUG_ARRAY: {
      uint64_t size = 0;
      if (type == LBUG_LIST) {
        require_success(lbug_value_get_list_size(value, &size), "Failed to read list size.");
      } else {
        lbug_logical_type logical{};
        lbug_value_get_data_type(value, &logical);
        lbug_state state = lbug_data_type_get_num_elements_in_array(&logical, &size);
        lbug_data_type_destroy(&logical);
        require_success(state, "Failed to read array size.");
      }
      List out(size);
      for (uint64_t i = 0; i < size; ++i) {
        lbug_value child{};
        require_success(lbug_value_get_list_element(value, i, &child),
                        "Failed to read list element.");
        out[i] = value_to_r(&child, bigint);
        lbug_value_destroy(&child);
      }
      return out;
    }
    case LBUG_MAP: {
      uint64_t size = 0;
      require_success(lbug_value_get_map_size(value, &size), "Failed to read map size.");
      List keys(size), values(size);
      for (uint64_t i = 0; i < size; ++i) {
        lbug_value key{}, item{};
        require_success(lbug_value_get_map_key(value, i, &key), "Failed to read map key.");
        require_success(lbug_value_get_map_value(value, i, &item), "Failed to read map value.");
        keys[i] = value_to_r(&key, bigint);
        values[i] = value_to_r(&item, bigint);
        lbug_value_destroy(&key);
        lbug_value_destroy(&item);
      }
      return List::create(Named("keys") = keys, Named("values") = values);
    }
    case LBUG_STRUCT:
      return struct_to_r(value, bigint);
    case LBUG_UNION:
      return union_to_r(value, bigint);
    case LBUG_RECURSIVE_REL: {
      lbug_value nodes{}, relationships{};
      require_success(lbug_value_get_recursive_rel_node_list(value, &nodes),
                      "Failed to read path nodes.");
      require_success(lbug_value_get_recursive_rel_rel_list(value, &relationships),
                      "Failed to read path relationships.");
      List out = List::create(
        Named("nodes") = value_to_r(&nodes, bigint),
        Named("relationships") = value_to_r(&relationships, bigint)
      );
      out.attr("class") = "lb_path";
      lbug_value_destroy(&nodes);
      lbug_value_destroy(&relationships);
      return out;
    }
    case LBUG_NODE: {
      lbug_value id{}, label{};
      require_success(lbug_node_val_get_id_val(value, &id), "Failed to read node id.");
      require_success(lbug_node_val_get_label_val(value, &label), "Failed to read node label.");
      uint64_t properties = 0;
      require_success(lbug_node_val_get_property_size(value, &properties),
                      "Failed to read node properties.");
      List out(2 + properties);
      CharacterVector names(2 + properties);
      names[0] = "_ID";
      names[1] = "_LABEL";
      out[0] = internal_id_to_r(&id);
      out[1] = value_to_r(&label, bigint);
      for (uint64_t i = 0; i < properties; ++i) {
        char* name = nullptr;
        lbug_value property{};
        require_success(lbug_node_val_get_property_name_at(value, i, &name),
                        "Failed to read node property name.");
        names[2 + i] = name == nullptr ? "" : name;
        if (name != nullptr) lbug_destroy_string(name);
        require_success(lbug_node_val_get_property_value_at(value, i, &property),
                        "Failed to read node property.");
        out[2 + i] = value_to_r(&property, bigint);
        lbug_value_destroy(&property);
      }
      out.names() = names;
      lbug_value_destroy(&id);
      lbug_value_destroy(&label);
      return out;
    }
    case LBUG_REL: {
      lbug_value source{}, destination{}, id{}, label{};
      require_success(lbug_rel_val_get_src_id_val(value, &source), "Failed to read rel source.");
      require_success(lbug_rel_val_get_dst_id_val(value, &destination),
                      "Failed to read rel destination.");
      require_success(lbug_rel_val_get_id_val(value, &id), "Failed to read rel id.");
      require_success(lbug_rel_val_get_label_val(value, &label), "Failed to read rel label.");
      uint64_t properties = 0;
      require_success(lbug_rel_val_get_property_size(value, &properties),
                      "Failed to read rel properties.");
      List out(4 + properties);
      CharacterVector names(4 + properties);
      names[0] = "_SRC";
      names[1] = "_DST";
      names[2] = "_LABEL";
      names[3] = "_ID";
      out[0] = internal_id_to_r(&source);
      out[1] = internal_id_to_r(&destination);
      out[2] = value_to_r(&label, bigint);
      out[3] = internal_id_to_r(&id);
      for (uint64_t i = 0; i < properties; ++i) {
        char* name = nullptr;
        lbug_value property{};
        require_success(lbug_rel_val_get_property_name_at(value, i, &name),
                        "Failed to read rel property name.");
        names[4 + i] = name == nullptr ? "" : name;
        if (name != nullptr) lbug_destroy_string(name);
        require_success(lbug_rel_val_get_property_value_at(value, i, &property),
                        "Failed to read rel property.");
        out[4 + i] = value_to_r(&property, bigint);
        lbug_value_destroy(&property);
      }
      out.names() = names;
      lbug_value_destroy(&source);
      lbug_value_destroy(&destination);
      lbug_value_destroy(&id);
      lbug_value_destroy(&label);
      return out;
    }
    default: {
      char* text = lbug_value_to_string(value);
      SEXP out = Rf_mkString(text == nullptr ? "" : text);
      if (text != nullptr) lbug_destroy_string(text);
      return out;
    }
  }
}

static std::vector<lbug_data_type_id> result_types(LbResult* result) {
  uint64_t columns = lbug_query_result_get_num_columns(&result->handle);
  std::vector<lbug_data_type_id> types(columns);
  for (uint64_t column = 0; column < columns; ++column) {
    lbug_logical_type logical{};
    require_success(lbug_query_result_get_column_data_type(&result->handle, column, &logical),
                    "Failed to read result column type.");
    types[column] = lbug_data_type_get_id(&logical);
    lbug_data_type_destroy(&logical);
  }
  return types;
}

static CharacterVector result_names(LbResult* result) {
  uint64_t columns = lbug_query_result_get_num_columns(&result->handle);
  CharacterVector names(columns);
  for (uint64_t column = 0; column < columns; ++column) {
    char* name = nullptr;
    require_success(lbug_query_result_get_column_name(&result->handle, column, &name),
                    "Failed to read result column name.");
    names[column] = name == nullptr ? "" : name;
    if (name != nullptr) lbug_destroy_string(name);
  }
  return names;
}

static SEXP allocate_column(lbug_data_type_id type, uint64_t rows,
                            const std::string& bigint) {
  if (static_cast<int>(type) == 60) return CharacterVector(rows, NA_STRING);
  switch (type) {
    case LBUG_BOOL:
      return LogicalVector(rows, NA_LOGICAL);
    case LBUG_INT8:
    case LBUG_INT16:
    case LBUG_INT32:
    case LBUG_UINT8:
    case LBUG_UINT16:
      return IntegerVector(rows, NA_INTEGER);
    case LBUG_INT64:
    case LBUG_SERIAL:
      if (bigint == "character") return CharacterVector(rows, NA_STRING);
      if (bigint == "integer64") {
        NumericVector out(rows, NA_REAL);
        int64_t missing = std::numeric_limits<int64_t>::min();
        for (uint64_t i = 0; i < rows; ++i) std::memcpy(REAL(out) + i, &missing, sizeof(missing));
        out.attr("class") = "integer64";
        return out;
      }
      return NumericVector(rows, NA_REAL);
    case LBUG_UINT64:
      if (bigint == "character" || bigint == "integer64") {
        return CharacterVector(rows, NA_STRING);
      }
      return NumericVector(rows, NA_REAL);
    case LBUG_UINT32:
    case LBUG_FLOAT:
    case LBUG_DOUBLE:
      return NumericVector(rows, NA_REAL);
    case LBUG_INT128:
    case LBUG_STRING:
    case LBUG_UUID:
    case LBUG_DECIMAL:
      return CharacterVector(rows, NA_STRING);
    case LBUG_DATE: {
      NumericVector out(rows, NA_REAL);
      out.attr("class") = "Date";
      return out;
    }
    case LBUG_TIMESTAMP:
    case LBUG_TIMESTAMP_TZ:
    case LBUG_TIMESTAMP_NS:
    case LBUG_TIMESTAMP_MS:
    case LBUG_TIMESTAMP_SEC: {
      NumericVector out(rows, NA_REAL);
      out.attr("class") = CharacterVector::create("POSIXct", "POSIXt");
      out.attr("tzone") = "UTC";
      return out;
    }
    default:
      return List(rows);
  }
}

static void set_column_value(SEXP column, uint64_t row, lbug_value* value,
                             lbug_data_type_id type, const std::string& bigint) {
  if (lbug_value_is_null(value)) return;
  SEXP scalar = value_to_r(value, bigint);
  switch (TYPEOF(column)) {
    case LGLSXP:
      LOGICAL(column)[row] = LOGICAL(scalar)[0];
      break;
    case INTSXP:
      INTEGER(column)[row] = INTEGER(scalar)[0];
      break;
    case REALSXP:
      if (Rf_inherits(column, "integer64")) {
        std::memcpy(REAL(column) + row, REAL(scalar), sizeof(int64_t));
      } else {
        REAL(column)[row] = Rcpp::as<double>(scalar);
      }
      break;
    case STRSXP:
      SET_STRING_ELT(column, row, STRING_ELT(scalar, 0));
      break;
    case VECSXP:
      SET_VECTOR_ELT(column, row, scalar);
      break;
  }
}

static void restore_position(LbResult* result, uint64_t position) {
  lbug_query_result_reset_iterator(&result->handle);
  result->position = 0;
  while (result->position < position && lbug_query_result_has_next(&result->handle)) {
    lbug_flat_tuple tuple{};
    require_success(lbug_query_result_get_next(&result->handle, &tuple),
                    "Failed to restore result iterator.");
    lbug_flat_tuple_destroy(&tuple);
    result->position++;
  }
}

// [[Rcpp::export]]
double lb_result_num_tuples(SEXP pointer) {
  LbResult* result = checked_result(pointer);
  return static_cast<double>(lbug_query_result_get_num_tuples(&result->handle));
}

// [[Rcpp::export]]
Rcpp::CharacterVector lb_result_column_names(SEXP pointer) {
  return result_names(checked_result(pointer));
}

// [[Rcpp::export]]
Rcpp::CharacterVector lb_result_column_types(SEXP pointer) {
  LbResult* result = checked_result(pointer);
  std::vector<lbug_data_type_id> types = result_types(result);
  CharacterVector out(types.size());
  for (size_t i = 0; i < types.size(); ++i) out[i] = type_name(types[i]);
  return out;
}

// [[Rcpp::export]]
bool lb_result_is_open(SEXP pointer) {
  return !checked_result(pointer, true)->closed;
}

// [[Rcpp::export]]
void lb_result_close(SEXP pointer) {
  checked_result(pointer, true)->close();
}

// [[Rcpp::export]]
bool lb_result_has_next_c(SEXP pointer) {
  LbResult* result = checked_result(pointer);
  return lbug_query_result_has_next(&result->handle);
}

// [[Rcpp::export]]
void lb_result_reset_c(SEXP pointer) {
  LbResult* result = checked_result(pointer);
  lbug_query_result_reset_iterator(&result->handle);
  result->position = 0;
}

// [[Rcpp::export]]
Rcpp::List lb_result_fetch(SEXP pointer, double n, bool preserve_position,
                           std::string bigint) {
  LbResult* result = checked_result(pointer);
  uint64_t saved_position = result->position;
  if (preserve_position) restore_position(result, 0);

  uint64_t total = lbug_query_result_get_num_tuples(&result->handle);
  uint64_t available = total > result->position ? total - result->position : 0;
  uint64_t requested = (!R_FINITE(n) || n < 0 ||
      n >= static_cast<double>(available))
    ? available : static_cast<uint64_t>(n);
  uint64_t rows = std::min(available, requested);
  uint64_t columns = lbug_query_result_get_num_columns(&result->handle);
  std::vector<lbug_data_type_id> types = result_types(result);

  List out(columns);
  out.names() = result_names(result);
  for (uint64_t column = 0; column < columns; ++column) {
    out[column] = allocate_column(types[column], rows, bigint);
  }

  uint64_t row = 0;
  while (row < rows && lbug_query_result_has_next(&result->handle)) {
    lbug_flat_tuple tuple{};
    require_success(lbug_query_result_get_next(&result->handle, &tuple),
                    "Failed to fetch result row.");
    for (uint64_t column = 0; column < columns; ++column) {
      lbug_value value{};
      require_success(lbug_flat_tuple_get_value(&tuple, column, &value),
                      "Failed to fetch result value.");
      set_column_value(out[column], row, &value, types[column], bigint);
      lbug_value_destroy(&value);
    }
    lbug_flat_tuple_destroy(&tuple);
    result->position++;
    row++;
  }

  if (preserve_position) restore_position(result, saved_position);
  return out;
}

// [[Rcpp::export]]
Rcpp::List lb_result_info_c(SEXP pointer) {
  LbResult* result = checked_result(pointer);
  return List::create(
    Named("column_names") = result_names(result),
    Named("column_types") = lb_result_column_types(pointer),
    Named("num_columns") = static_cast<double>(
      lbug_query_result_get_num_columns(&result->handle)),
    Named("num_tuples") = static_cast<double>(
      lbug_query_result_get_num_tuples(&result->handle)),
    Named("rows_fetched") = static_cast<double>(result->position),
    Named("complete") = !lbug_query_result_has_next(&result->handle),
    Named("has_next_result") = lbug_query_result_has_next_query_result(&result->handle)
  );
}

// [[Rcpp::export]]
Rcpp::List lb_result_summary_c(SEXP pointer) {
  LbResult* result = checked_result(pointer);
  lbug_query_summary summary{};
  require_success(lbug_query_result_get_query_summary(&result->handle, &summary),
                  "Failed to read query summary.");
  List out = List::create(
    Named("compiling_time_ms") = lbug_query_summary_get_compiling_time(&summary),
    Named("execution_time_ms") = lbug_query_summary_get_execution_time(&summary)
  );
  lbug_query_summary_destroy(&summary);
  return out;
}

// [[Rcpp::export]]
SEXP lb_result_next_result_c(SEXP pointer) {
  LbResult* parent = checked_result(pointer);
  if (!lbug_query_result_has_next_query_result(&parent->handle)) return R_NilValue;
  LbResult* result = new LbResult(parent->connection);
  lbug_state state = lbug_query_result_get_next_query_result(&parent->handle, &result->handle);
  return finish_result(result, state, "Failed to retrieve the next query result.");
}

// ---------------------------------------------------------------------------
// Arrow C Data Interface
// ---------------------------------------------------------------------------

static void arrow_schema_finalizer(SEXP pointer) {
  ArrowSchema* schema = static_cast<ArrowSchema*>(R_ExternalPtrAddr(pointer));
  if (schema != nullptr) {
    if (schema->release != nullptr) schema->release(schema);
    delete schema;
    R_ClearExternalPtr(pointer);
  }
}

static void arrow_array_finalizer(SEXP pointer) {
  ArrowArray* array = static_cast<ArrowArray*>(R_ExternalPtrAddr(pointer));
  if (array != nullptr) {
    if (array->release != nullptr) array->release(array);
    delete array;
    R_ClearExternalPtr(pointer);
  }
}

static SEXP external_arrow_schema(ArrowSchema* schema) {
  SEXP pointer = PROTECT(R_MakeExternalPtr(schema, R_NilValue, R_NilValue));
  // Windows headers define TRUE as an int; R-devel requires Rboolean here.
  R_RegisterCFinalizerEx(pointer, arrow_schema_finalizer,
                         static_cast<Rboolean>(1));
  UNPROTECT(1);
  return pointer;
}

static SEXP external_arrow_array(ArrowArray* array) {
  SEXP pointer = PROTECT(R_MakeExternalPtr(array, R_NilValue, R_NilValue));
  // Windows headers define TRUE as an int; R-devel requires Rboolean here.
  R_RegisterCFinalizerEx(pointer, arrow_array_finalizer,
                         static_cast<Rboolean>(1));
  UNPROTECT(1);
  return pointer;
}

// [[Rcpp::export]]
Rcpp::List lb_arrow_allocate_c() {
  ArrowSchema* schema = new ArrowSchema{};
  ArrowArray* array = new ArrowArray{};
  return List::create(
    Named("array") = external_arrow_array(array),
    Named("schema") = external_arrow_schema(schema)
  );
}

// [[Rcpp::export]]
Rcpp::List lb_result_fetch_arrow_c(SEXP pointer, double n, bool preserve_position) {
  LbResult* result = checked_result(pointer);
  uint64_t saved_position = result->position;
  if (preserve_position) restore_position(result, 0);

  uint64_t total = lbug_query_result_get_num_tuples(&result->handle);
  uint64_t available = total > result->position ? total - result->position : 0;
  uint64_t bounded_available = std::min<uint64_t>(
    available, static_cast<uint64_t>(std::numeric_limits<int64_t>::max()));
  int64_t requested = (!R_FINITE(n) || n < 0 ||
      n >= static_cast<double>(bounded_available))
    ? static_cast<int64_t>(bounded_available)
    : static_cast<int64_t>(n);

  ArrowSchema* schema = new ArrowSchema{};
  ArrowArray* array = new ArrowArray{};
  lbug_state schema_state = lbug_query_result_get_arrow_schema(&result->handle, schema);
  if (schema_state != LbugSuccess) {
    delete schema;
    delete array;
    Rcpp::stop(take_last_error("Failed to export Arrow schema."));
  }
  lbug_state array_state = lbug_query_result_get_next_arrow_chunk(
    &result->handle, requested, array);
  if (array_state != LbugSuccess) {
    if (schema->release != nullptr) schema->release(schema);
    delete schema;
    delete array;
    Rcpp::stop(take_last_error("Failed to export Arrow result chunk."));
  }
  result->position += static_cast<uint64_t>(array->length);
  if (preserve_position) restore_position(result, saved_position);

  return List::create(
    Named("array") = external_arrow_array(array),
    Named("schema") = external_arrow_schema(schema),
    Named("num_rows") = static_cast<double>(array->length)
  );
}

// [[Rcpp::export]]
SEXP lb_connection_create_arrow_table_c(SEXP connection_pointer, std::string table,
                                        SEXP array_pointer, SEXP schema_pointer) {
  LbConnection* connection = checked_connection(connection_pointer);
  if (TYPEOF(array_pointer) != EXTPTRSXP || TYPEOF(schema_pointer) != EXTPTRSXP) {
    Rcpp::stop("Invalid Arrow C Data pointers.");
  }
  ArrowArray* array = static_cast<ArrowArray*>(R_ExternalPtrAddr(array_pointer));
  ArrowSchema* schema = static_cast<ArrowSchema*>(R_ExternalPtrAddr(schema_pointer));
  if (array == nullptr || schema == nullptr || array->release == nullptr ||
      schema->release == nullptr) {
    Rcpp::stop("Arrow data has already been released or consumed.");
  }
  LbResult* result = new LbResult(connection);
  lbug_state state = lbug_connection_create_arrow_table(
    &connection->handle, table.c_str(), schema, array, 1, &result->handle);
  return finish_result(result, state, "Failed to create Arrow-backed table.");
}

// [[Rcpp::export]]
SEXP lb_connection_drop_arrow_table_c(SEXP connection_pointer, std::string table) {
  LbConnection* connection = checked_connection(connection_pointer);
  LbResult* result = new LbResult(connection);
  lbug_state state = lbug_connection_drop_arrow_table(
    &connection->handle, table.c_str(), &result->handle);
  return finish_result(result, state, "Failed to drop Arrow-backed table.");
}
