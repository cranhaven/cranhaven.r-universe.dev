DROP TABLE IF EXISTS #cohort_dates;

SELECT subject_id,
	cohort_date,
	-- LEAD will ignore values that are same (e.g. if cohort_start_date = cohort_end_date)
	ROW_NUMBER() OVER(PARTITION BY subject_id
	                  ORDER BY cohort_date ASC) cohort_date_seq
INTO #cohort_dates
FROM (
	SELECT subject_id,
		      cohort_start_date cohort_date
	FROM {@source_cohort_database_schema != ''} ? {@source_cohort_database_schema.@source_cohort_table} : {@source_cohort_table}
	WHERE cohort_definition_id IN (@cohort_ids)

	UNION ALL -- we need all dates, even if duplicates

	SELECT subject_id,
		      cohort_end_date cohort_date
	FROM {@source_cohort_database_schema != ''} ? {@source_cohort_database_schema.@source_cohort_table} : {@source_cohort_table}
	WHERE cohort_definition_id IN (@cohort_ids)
	) all_dates;
	
	

SELECT
	subject_id,
	cohort_date candidate_start_date,
	cohort_date_seq,
	LEAD(cohort_date, 1) OVER (
		PARTITION BY subject_id ORDER BY cohort_date, cohort_date_seq ASC
		) candidate_end_date
INTO #candidate_periods
FROM #cohort_dates
GROUP BY subject_id,
	cohort_date,
	cohort_date_seq;
DROP TABLE IF EXISTS #cohort_dates;


SELECT DISTINCT cohort.*,
	candidate_start_date,
	candidate_end_date
INTO #can_chrt_dte
FROM {@source_cohort_database_schema != ''} ? {@source_cohort_database_schema.@source_cohort_table} : {@source_cohort_table} cohort
INNER JOIN #candidate_periods candidate ON cohort.subject_id = candidate.subject_id
	AND candidate_start_date >= cohort_start_date
	AND candidate_end_date <= cohort_end_date
WHERE cohort.cohort_definition_id IN (@cohort_ids);
DROP TABLE IF EXISTS #candidate_periods;

DELETE FROM {@target_cohort_database_schema != ''} ? {@target_cohort_database_schema.@target_cohort_table} : {@target_cohort_table}
WHERE cohort_definition_id = @new_cohort_id;

INSERT INTO {@target_cohort_database_schema != ''} ? {@target_cohort_database_schema.@target_cohort_table} : {@target_cohort_table}
SELECT CAST(@new_cohort_id AS BIGINT) cohort_definition_id,
       CAST(subject_id AS BIGINT) subject_id,
       CAST(candidate_start_date AS DATE) cohort_start_date, 
       CAST(candidate_end_date AS DATE) cohort_end_date
FROM #can_chrt_dte
GROUP BY subject_id,
	candidate_start_date,
	candidate_end_date
HAVING COUNT(*) = @number_of_cohorts;


DROP TABLE IF EXISTS #cohort_dates;
DROP TABLE IF EXISTS #candidate_periods;
DROP TABLE IF EXISTS #can_chrt_dte;
