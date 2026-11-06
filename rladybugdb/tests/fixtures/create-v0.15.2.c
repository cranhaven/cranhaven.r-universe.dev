/* Rebuild the compatibility fixture with the LadybugDB v0.15.2 C library. */
#include <lbug.h>
#include <stdio.h>

static int run(lbug_connection* connection, const char* query) {
    lbug_query_result result = {0};
    lbug_state state = lbug_connection_query(connection, query, &result);
    if (state != LbugSuccess || !lbug_query_result_is_success(&result)) {
        char* message = lbug_query_result_get_error_message(&result);
        fprintf(stderr, "%s\n", message == NULL ? "query failed" : message);
        if (message != NULL) lbug_destroy_string(message);
        lbug_query_result_destroy(&result);
        return 1;
    }
    lbug_query_result_destroy(&result);
    return 0;
}

int main(int argc, char** argv) {
    if (argc != 2) return 2;
    lbug_database database = {0};
    lbug_connection connection = {0};
    lbug_system_config config = lbug_default_system_config();
    if (lbug_database_init(argv[1], config, &database) != LbugSuccess) return 3;
    if (lbug_connection_init(&database, &connection) != LbugSuccess) return 4;
    if (run(&connection,
            "CREATE NODE TABLE Compatibility (id INT64, value STRING, PRIMARY KEY(id))") ||
        run(&connection, "CREATE (:Compatibility {id: 1, value: 'from 0.15.2'})")) {
        return 5;
    }
    lbug_connection_destroy(&connection);
    lbug_database_destroy(&database);
    return 0;
}
