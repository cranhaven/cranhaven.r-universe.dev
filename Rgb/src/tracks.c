// Memory and time efficient subsetting of chromosomal tracks (chrom, start, end)
// Author : Sylvain Mareschal <mareschal@ovsa.fr>

#include "tracks.h"

// Routine registration (executed at package loading)
void attribute_visible R_init_Rgb(DllInfo *info) {
	// External() calls
	R_ExternalMethodDef externalMethods[] = {
		{"track", (DL_FUNC) &track, -1},
		{"checktrack", (DL_FUNC) &checktrack, -1},
		{NULL, NULL, 0}
	};
	
	// Register all entry points
	R_registerRoutines(info, NULL, NULL, NULL, externalMethods);
	
	// Prevent access to all non-registered entry points on Linux, except "attribute_visible" ones
	// Remember to update "src/Rgb-win.def" file for Windows !
	R_useDynamicSymbols(info, FALSE);
}

// Counts vectors in an argList, unfolding the first level of data.frames
int argsColCount(SEXP args) {
	SEXP column;
	int colCount = 0;
	for(; args != R_NilValue; args = CDR(args)) {
		column = CAR(args);
		switch(TYPEOF(column)) {
			case INTSXP: case REALSXP: case LGLSXP: case STRSXP:
				// Individual vectors
				colCount++;
				break;
			case VECSXP:
				// List
				for(R_len_t i = 0; i < length(column); i++) {
					switch(TYPEOF(VECTOR_ELT(column, i))) {
						case INTSXP: case REALSXP: case LGLSXP: case STRSXP:
							colCount++;
							break;
						default:
							error("Unhandled column type (sub level)");
					}
				}
				break;
			default:
				error("Unhandled column type (top level)");
		}
	}
	return colCount;
}

// Collect vectors in an argList, unfolding the first level of data.frames
void argsColCollect(SEXP args, int colCount, SEXP** colSexp, SEXP* colNames) {
	int j = 0;
	SEXP column;
	SEXP subNames;
	(*colSexp) = Calloc(colCount, SEXP);
	PROTECT((*colNames) = allocVector(STRSXP, colCount));
	for(; args != R_NilValue; args = CDR(args)) {
		column = CAR(args);
		switch(TYPEOF(column)) {
			case INTSXP: case REALSXP: case LGLSXP: case STRSXP:
				// Individual vectors
				(*colSexp)[j] = column;
				if(isNull(TAG(args))) {
					SET_STRING_ELT((*colNames), j, mkChar("<unknown>"));
				} else {
					SET_STRING_ELT((*colNames), j, PRINTNAME(TAG(args)));
				}
				j++;
				break;
			case VECSXP:
				// List
				subNames = getAttrib(column, R_NamesSymbol);
				for(R_len_t i = 0; i < length(column); i++) {
					switch(TYPEOF(VECTOR_ELT(column, i))) {
						case INTSXP: case REALSXP: case LGLSXP: case STRSXP:
							(*colSexp)[j] = VECTOR_ELT(column, i);
							SET_STRING_ELT((*colNames), j, STRING_ELT(subNames, i));
							j++;
							break;
						default:
							error("Unhandled column type (sub level)");
					}
				}
				break;
			default:
				error("Unhandled column type (top level)");
		}
	}
	UNPROTECT(1);
}

// Collect vectors in an argList, unfolding the first level of data.frames
void coordCollect(int colCount, SEXP* colSexp, SEXP colNames, SEXP* chrom, SEXP* start, SEXP* end) {
	const char* colName;
	(*chrom) = NULL;
	(*start) = NULL;
	(*end) = NULL;
	
	for(int i = 0; i < colCount; i++) {
		colName = CHAR(STRING_ELT(colNames, i));
		if(strcmp(colName, "chrom") == 0) {
			if((*chrom) != NULL) {
				error("'chrom' is provided twice");
			}
			(*chrom) = colSexp[i];
		} else if(strcmp(colName, "start") == 0) {
			if((*start) != NULL) {
				error("'start' is provided twice");
			}
			(*start) = colSexp[i];
		} else if(strcmp(colName, "end") == 0) {
			if((*end) != NULL) {
				error("'end' is provided twice");
			}
			(*end) = colSexp[i];
		}
	}
	
	if((*chrom) == NULL) {
		error("'chrom' was not explicitely provided");
	}
	if((*start) == NULL) {
		error("'start' was not explicitely provided");
	}
	if((*end) == NULL) {
		error("'end' was not explicitely provided");
	}
	
	if(TYPEOF((*chrom)) != INTSXP) {
		error("'chrom' must be an integer or factor vector");
	}
	if(!isInteger((*start))) {
		error("'start' must be an integer vector");
	}
	if(!isInteger((*end))) {
		error("'end' must be an integer vector");
	}
}

// Returns the proper integer chrom target (as is or through factor levels)
// Negative values should skip further treatments (level not found in factor)
int chromTarget(SEXP chrom, SEXP targetChrom) {
	int output;
	if(isFactor(chrom)) {
		// Convert 'targetChrom' to a character vector
		if(isFactor(targetChrom)) { targetChrom = PROTECT(asCharacterFactor(targetChrom));
		} else                    { targetChrom = PROTECT(coerceVector(targetChrom, STRSXP));
		}
		
		if(LENGTH(targetChrom) != 1 || STRING_ELT(targetChrom, 0) == NA_STRING) {
			error("As 'chrom' is factor, target 'chrom' must be a single non-NA character-coercible value");
		}
		
		// From character to integer position in the index (0+)
		SEXP levels = PROTECT(getAttrib(chrom, R_LevelsSymbol));
		for(int i = 0; i < LENGTH(levels); i++) {
			if(strcmp(CHAR(STRING_ELT(levels, i)), CHAR(STRING_ELT(targetChrom, 0))) == 0) {
				// Early exit when found
				output = i;
				UNPROTECT(2);
				return output;
			}
		}
		
		// Was not found in levels
		output = -1;
		UNPROTECT(2);
	} else {
		// Integer chrom
		targetChrom = PROTECT(coerceVector(targetChrom, INTSXP));
		if(LENGTH(targetChrom) != 1 || INTEGER(targetChrom)[0] == NA_INTEGER || INTEGER(targetChrom)[0] < 0) {
			error("As 'chrom' is integer, target 'chrom' must be a single non-NA integer-coercible strictly positive value");
		}
		
		// Position in the index (0+)
		output = INTEGER(targetChrom)[0] - 1;
		UNPROTECT(1);
	}
	return output;
}

// Returns the checked target start
int startTarget(SEXP targetStart) {
	targetStart = coerceVector(targetStart, INTSXP);
	if(LENGTH(targetStart) != 1 || INTEGER(targetStart)[0] == NA_INTEGER) {
		error("target 'start' must be a single non-NA integer-coercible value");
	}
	return INTEGER(targetStart)[0];
}

// Returns the checked target end
int endTarget(SEXP targetEnd) {
	targetEnd = coerceVector(targetEnd, INTSXP);
	if(LENGTH(targetEnd) != 1 || INTEGER(targetEnd)[0] == NA_INTEGER) {
		error("target 'end' must be a single non-NA integer-coercible value");
	}
	return INTEGER(targetEnd)[0];
}

SEXP checktrack(SEXP args) {
	
	// Skip function name
	args = CDR(args);
	
	// Extract columns from arguments
	SEXP colNames;
	SEXP* colSexp;
	int colCount = argsColCount(args);
	argsColCollect(args, colCount, &colSexp, &colNames);
	PROTECT(colNames);
	
	// Length consistency
	int rowCount = LENGTH(colSexp[0]);
	for(int i = 1; i < colCount; i++) {
		if(LENGTH(colSexp[i]) != rowCount) {
			error("All columns must have same lengths");
		}
	}
	
	// Coordinate columns extraction and check
	SEXP chrom, start, end;
	coordCollect(colCount, colSexp, colNames, &chrom, &start, &end);
	
	// Order check
	int naChrom = 0;
	int naStart = 0;
	for(int i = 1; i < LENGTH(chrom); i++) {
		// NA chrom
		if(INTEGER(chrom)[i] == NA_INTEGER) {
			naChrom = 1;
			continue;
		} else if(naChrom) {
			error("'chrom' NA not ordered (row %d)", i);
		}
		
		if(INTEGER(chrom)[i-1] == INTEGER(chrom)[i]) {
			// NA start
			if(INTEGER(start)[i] == NA_INTEGER) {
				naStart = 1;
				continue;
			} else if(naStart) {
				error("'start' NA not ordered (row %d)", i);
			}
			
			// Unordered starts
			if(INTEGER(start)[i-1] > INTEGER(start)[i]) {
				error("'start' is not ordered (row %d)", i);
			}
		} else if(INTEGER(chrom)[i-1] < INTEGER(chrom)[i]) {
			// Ordered chrom gap
			naStart = 0;
		} else {
			// Unordered chrom gap
			error("'chrom' is not ordered (row %d)", i);
		}
	}
	
	// Output
	SEXP output = PROTECT(allocVector(LGLSXP, 1));
	LOGICAL(output)[0] = TRUE;
	
	// Unprotect variables
	Free(colSexp);
	UNPROTECT(2);
	
	return output;
}

SEXP track(SEXP args) {
	
	// Skip function name
	args = CDR(args);
	
	// Execution mode
	SEXP mode = CAR(args);
	args = CDR(args);
	
	// Targets
	SEXP targetChrom = CAR(args);
	args = CDR(args);
	int targetStart_int = startTarget(CAR(args));
	args = CDR(args);
	int targetEnd_int = endTarget(CAR(args));
	args = CDR(args);
	
	// Index
	SEXP index = CAR(args);
	args = CDR(args);
	
	// Extract columns from arguments
	SEXP colNames;
	SEXP* colSexp;
	int colCount = argsColCount(args);
	argsColCollect(args, colCount, &colSexp, &colNames);
	PROTECT(colNames);
	
	// Length consistency
	int rowCount = LENGTH(colSexp[0]);
	for(int i = 1; i < colCount; i++) {
		if(LENGTH(colSexp[i]) != rowCount) {
			error("All columns must have same lengths");
		}
	}
	
	// Coordinate columns extraction and check
	SEXP chrom, start, end;
	coordCollect(colCount, colSexp, colNames, &chrom, &start, &end);
	
	// Chrom consistency
	int targetChrom_int = chromTarget(chrom, targetChrom);
	
	// Index search for the chromosome range ('start' is IN the range, 'end' is not)
	int chromRangeStart, chromRangeEnd;
	if(rowCount > 0) {
		if(targetChrom_int != -1) {
			// Test index length
			if(LENGTH(index) <= targetChrom_int) {
				error("The index seems corrupted (looking for a non-existing value)");
			}
		
			// Get range end from the index
			chromRangeEnd = INTEGER(index)[ targetChrom_int ];
		
			if(chromRangeEnd != NA_INTEGER) {
				// Get the end of the previous non NA chromosome
				int i = targetChrom_int - 1;
				while(i >= 0 && INTEGER(index)[i] == NA_INTEGER) { i--; }
				if(i < 0) {
					chromRangeStart = 0;
				} else {
					chromRangeStart = INTEGER(index)[i];
				}
			} else {
				// Level found but not used
				chromRangeStart = -1;
				chromRangeEnd = -1;
			}
		} else {
			// Not found in levels
			chromRangeStart = -1;
			chromRangeEnd = -1;
		}
	} else {
		// No row to search in
		chromRangeStart = -1;
		chromRangeEnd = -1;
	}
	
	// Hit count
	int outputLength = 0;
	if(chromRangeStart != -1) {
		for(int i = chromRangeStart; i < chromRangeEnd && INTEGER(start)[i] <= targetEnd_int; i++) {
			if(INTEGER(end)[i] >= targetStart_int) {
				outputLength++;
			}
		}
	}
	
	// R object to return
	SEXP output;
	
	if(strcmp(CHAR(STRING_ELT(mode, 0)), "size") == 0) {
		// Count only
		PROTECT(output = allocVector(INTSXP, 1));
		INTEGER(output)[0] = outputLength;
		
		// Memory release
		Free(colSexp);
	
		// Unprotect variables (last action !)
		UNPROTECT(2);
	} else if(strcmp(CHAR(STRING_ELT(mode, 0)), "sub") == 0) {	
		// Data frame
		PROTECT(output = allocVector(VECSXP, colCount));
	
		// Data frame row names storage
		SEXP outRowNames;
		PROTECT(outRowNames = allocVector(INTSXP, outputLength));
	
		// Data frame column allocation
		for(int i = 0; i < colCount; i++) {
			switch(TYPEOF(colSexp[i])) {
				case INTSXP:
					SET_VECTOR_ELT(output, i, allocVector(INTSXP, outputLength));
					break;
				case REALSXP:
					SET_VECTOR_ELT(output, i, allocVector(REALSXP, outputLength));
					break;
				case LGLSXP:
					SET_VECTOR_ELT(output, i, allocVector(LGLSXP, outputLength));
					break;
				case STRSXP:
					SET_VECTOR_ELT(output, i, allocVector(STRSXP, outputLength));
					break;
				default:
					error("Unhandled column type");
			}
			DUPLICATE_ATTRIB(VECTOR_ELT(output, i), colSexp[i]);
		}
	
		// Data frame filling
		if(chromRangeStart != -1) {
			for(int i = chromRangeStart, j = 0; i < chromRangeEnd && INTEGER(start)[i] <= targetEnd_int; i++) {
				if(INTEGER(end)[i] >= targetStart_int) {
					for(int k = 0; k < colCount; k++) {
						switch(TYPEOF(colSexp[k])) {
							case INTSXP:
								INTEGER(VECTOR_ELT(output, k))[j] = INTEGER(colSexp[k])[i];
								break;
							case REALSXP:
								REAL(VECTOR_ELT(output, k))[j] = REAL(colSexp[k])[i];
								break;
							case LGLSXP:
								LOGICAL(VECTOR_ELT(output, k))[j] = LOGICAL(colSexp[k])[i];
								break;
							case STRSXP:
								SET_STRING_ELT(VECTOR_ELT(output, k), j, STRING_ELT(colSexp[k], i));
								break;
							default:
								error("Unhandled column type");
						}
					}
					INTEGER(outRowNames)[j] = j + 1;
					j++;
				}
			}
		}
	
		// Data frame class
		SEXP outClass;
		PROTECT(outClass = allocVector(STRSXP, 1));
		SET_STRING_ELT(outClass, 0, mkChar("data.frame"));
		setAttrib(output, R_ClassSymbol, outClass);
	
		// Data frame col names
		setAttrib(output, R_NamesSymbol, colNames);
	
		// Data frame row names attribution
		setAttrib(output, R_RowNamesSymbol, outRowNames);
		
		// Memory release
		Free(colSexp);
	
		// Unprotect variables (last action !)
		UNPROTECT(4);
	} else {
		error("'mode' argument not handled ('size' or 'sub')");
	}
	
	return output;
}
