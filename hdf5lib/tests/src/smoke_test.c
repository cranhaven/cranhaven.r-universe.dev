#include <R.h>
#include <Rinternals.h>
#include <stdio.h>  // snprintf
#include <stdlib.h> // malloc, free

#include <hdf5.h>
#include <hdf5_hl.h>


/* Variables accessible to `close_and_free()` */
hid_t file_id, dset_id, dset2_id, dtype_id, space_id, plist_id;
char  *version_str_read;


/*
 * If an error occurs, this function is called to release
 * h5 file handles and dynamically allocated memory.
 * Also called prior to normal exit.
 */
void close_and_free() {
  if (dtype_id >= 0) H5Tclose(dtype_id);
  if (dset_id  >= 0) H5Dclose(dset_id);
  if (dset2_id >= 0) H5Dclose(dset2_id);
  if (space_id >= 0) H5Sclose(space_id);
  if (plist_id >= 0) H5Pclose(plist_id);
  if (file_id  >= 0) H5Fclose(file_id);
  if (version_str_read != NULL) free(version_str_read);
}



/*
 * Internal test function callable from R via .Call()
 * - Gets HDF5 library version.
 * - Writes version to dataset "/version_str" using HL API.
 * - Writes a compressed dataset "/compressed_data" using LL API + zlib.
 * - Reads version back from dataset "/version_str" using LL API.
 * - Reads compressed data back and verifies it.
 * - Returns the read-back version string as an R character vector SEXP.
 */
SEXP C_smoke_test(SEXP sexp_filename) {

    const char *filename = CHAR(STRING_ELT(sexp_filename, 0));
    char        version_str_write[64];
    unsigned    majnum, minnum, relnum;
    
    version_str_read = NULL;
    
    // Data for zlib compression test
    int         zlib_data_write[5] = {10, 20, 30, 40, 50};
    int         zlib_data_read[5]  = {0, 0, 0, 0, 0};
    hsize_t     dims[1] = {5};
    hsize_t     chunk_dims[1] = {5};

    // HDF5 object identifiers
    file_id  = H5I_INVALID_HID;
    dset_id  = H5I_INVALID_HID; // For string
    dset2_id = H5I_INVALID_HID; // For zlib
    dtype_id = H5I_INVALID_HID;
    space_id = H5I_INVALID_HID;
    plist_id = H5I_INVALID_HID;
    
    herr_t      status      = 0;
    size_t      dtype_size  = 0;
    SEXP        sexp_result = R_NilValue; // Default return

    // --- Get HDF5 Version ---
    if (H5get_libversion(&majnum, &minnum, &relnum) < 0) {
        Rf_error("C_smoke_test: H5get_libversion failed");
        return R_NilValue;
    }
    snprintf(version_str_write, sizeof(version_str_write), "%u.%u.%u", majnum, minnum, relnum);

    // === WRITE PHASE ===

    // --- 1. Create the file ---
    file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if (file_id < 0) {
        close_and_free();
        Rf_error("C_smoke_test: H5Fcreate failed for file: %s", filename);
    }

    // --- 2. Write version string using High-Level API ---
    status = H5LTmake_dataset_string(file_id, "/version_str", version_str_write);
    if (status < 0) {
        close_and_free();
        Rf_error("C_smoke_test: H5LTmake_dataset_string failed");
    }

    // --- 3. Write compressed data using Low-Level API (ZLIB TEST) ---
    
    // Create dataspace for the 1D array
    space_id = H5Screate_simple(1, dims, NULL);
    if (space_id < 0) {
        close_and_free();
        Rf_error("C_smoke_test (zlib): H5Screate_simple failed");
    }

    // Create dataset creation property list
    plist_id = H5Pcreate(H5P_DATASET_CREATE);
    if (plist_id < 0) {
        close_and_free();
        Rf_error("C_smoke_test (zlib): H5Pcreate failed");
    }

    // Set chunking (required for filters)
    status = H5Pset_chunk(plist_id, 1, chunk_dims);
    if (status < 0) {
        close_and_free();
        Rf_error("C_smoke_test (zlib): H5Pset_chunk failed");
    }

    // Set zlib/deflate compression filter.
    // THIS IS THE ACTUAL TEST. It will fail if zlib is not available.
    status = H5Pset_deflate(plist_id, 9); // Level 9 compression
    if (status < 0) {
        close_and_free();
        Rf_error("C_smoke_test (zlib): H5Pset_deflate failed. zlib filter is not available.");
    }

    // Create the new dataset with compression properties
    dset2_id = H5Dcreate2(file_id, "/compressed_data", H5T_NATIVE_INT, 
                         space_id, H5P_DEFAULT, plist_id, H5P_DEFAULT);
    if (dset2_id < 0) {
        close_and_free();
        Rf_error("C_smoke_test (zlib): H5Dcreate2 failed");
    }

    // Write the integer data
    status = H5Dwrite(dset2_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, 
                      H5P_DEFAULT, zlib_data_write);
    if (status < 0) {
        close_and_free();
        Rf_error("C_smoke_test (zlib): H5Dwrite failed");
    }

    // Close zlib-test objects
    H5Sclose(space_id); space_id = H5I_INVALID_HID;
    H5Pclose(plist_id); plist_id = H5I_INVALID_HID;
    H5Dclose(dset2_id); dset2_id = H5I_INVALID_HID;


    // --- 4. Close the file ---
    status = H5Fclose(file_id);
    file_id = H5I_INVALID_HID; // Reset ID after close
    if (status < 0) {
        Rf_warning("C_smoke_test: H5Fclose (after write) failed");
    }

    
    // === READ PHASE ===

    // --- 5. Open the file read-only ---
    file_id = H5Fopen(filename, H5F_ACC_RDONLY, H5P_DEFAULT);
    if (file_id < 0) {
        close_and_free();
        Rf_error("C_smoke_test: H5Fopen failed for file: %s", filename);
    }

    // --- 6. Read version string using Low-Level API ---
    dset_id = H5Dopen2(file_id, "/version_str", H5P_DEFAULT);
    if (dset_id < 0) {
        close_and_free();
        Rf_error("C_smoke_test: H5Dopen2 failed for dataset '/version_str'");
    }
    dtype_id = H5Dget_type(dset_id);
    if (dtype_id < 0) {
        close_and_free();
        Rf_error("C_smoke_test: H5Dget_type failed");
    }
    if (H5Tget_class(dtype_id) != H5T_STRING) {
        close_and_free();
         Rf_error("C_smoke_test: Dataset '/version_str' is not a string type");
    }
    dtype_size = H5Tget_size(dtype_id);
    if (dtype_size == 0) {
        close_and_free();
         Rf_error("C_smoke_test: H5Tget_size returned 0");
    }
    version_str_read = (char *)malloc(dtype_size + 1);
    if (version_str_read == NULL) {
        close_and_free();
        Rf_error("C_smoke_test: Failed to allocate memory for reading");
    }
    
    // Create a C string datatype for reading
    hid_t mem_dtype_id = H5Tcopy(H5T_C_S1);
    H5Tset_size(mem_dtype_id, dtype_size + 1);
    H5Tset_strpad(mem_dtype_id, H5T_STR_NULLTERM);

    status = H5Dread(dset_id, mem_dtype_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, version_str_read);
    H5Tclose(mem_dtype_id); // Close the temporary memory datatype
    if (status < 0) {
        close_and_free();
        Rf_error("C_smoke_test: H5Dread for '/version_str' failed");
    }
    version_str_read[dtype_size] = '\0';


    // --- 7. Read compressed data (ZLIB TEST) ---
    dset2_id = H5Dopen2(file_id, "/compressed_data", H5P_DEFAULT);
    if (dset2_id < 0) {
        close_and_free();
        Rf_error("C_smoke_test (zlib): H5Dopen2 for '/compressed_data' failed");
    }

    status = H5Dread(dset2_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, 
                     H5P_DEFAULT, zlib_data_read);
    if (status < 0) {
        close_and_free();
        Rf_error("C_smoke_test (zlib): H5Dread for '/compressed_data' failed");
    }

    // Verify zlib data
    for (int i = 0; i < 5; i++) {
        if (zlib_data_write[i] != zlib_data_read[i]) {
            close_and_free();
            Rf_error("C_smoke_test (zlib): Data mismatch at index %d! Wrote %d, Read %d",
                     i, zlib_data_write[i], zlib_data_read[i]);
        }
    }
    // If we get here, zlib test passed!

    // --- 8. Prepare Return Value ---
    sexp_result = PROTECT(Rf_allocVector(STRSXP, 1));
    SET_STRING_ELT(sexp_result, 0, Rf_mkChar(version_str_read));

    close_and_free();
    UNPROTECT(1); // For sexp_result
    return sexp_result;
}
