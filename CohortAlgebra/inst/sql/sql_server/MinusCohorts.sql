DROP TABLE IF EXISTS #cohort_dates;
DROP TABLE IF EXISTS #can_cht_date;
DROP TABLE IF EXISTS @temp_table_2;

INSERT INTO @temp_table_1
SELECT *
FROM {@source_cohort_database_schema != ''} ? {@source_cohort_database_schema.@source_cohort_table} : {@source_cohort_table}
WHERE cohort_definition_id IN (@first_cohort_id);


SELECT subject_id,
	cohort_date,
	-- LEAD will ignore values that are same (e.g. if cohort_start_date = cohort_end_date)
	ROW_NUMBER() OVER(PARTITION BY subject_id
	                  ORDER BY cohort_date ASC) cohort_date_seq
INTO #cohort_dates
FROM (-- we need all dates, even if duplicates hence UNION ALL
	SELECT subject_id,
		cohort_start_date cohort_date
	FROM @temp_table_1
	WHERE cohort_definition_id IN (@first_cohort_id, -999)

	UNION ALL 

	SELECT subject_id,
		cohort_end_date cohort_date
	FROM @temp_table_1
	WHERE cohort_definition_id IN (@first_cohort_id, -999)
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
INTO #can_cht_date
FROM @temp_table_1 cohort
INNER JOIN #candidate_periods candidate ON cohort.subject_id = candidate.subject_id
	AND candidate_start_date >= cohort_start_date
	AND candidate_end_date <= cohort_end_date;
DROP TABLE IF EXISTS #candidate_periods;	
		
SELECT
	subject_id,
	candidate_start_date,
	candidate_end_date
INTO @temp_table_2
FROM #can_cht_date
GROUP BY subject_id,
	candidate_start_date,
	candidate_end_date
HAVING COUNT(*) = 1;
DROP TABLE IF EXISTS #can_cht_date;	

DELETE FROM {@target_cohort_database_schema != ''} ? {@target_cohort_database_schema.@target_cohort_table} : {@target_cohort_table}
WHERE cohort_definition_id = @new_cohort_id;
            

INSERT INTO {@target_cohort_database_schema != ''} ? {@target_cohort_database_schema.@target_cohort_table} : {@target_cohort_table}
SELECT CAST(@new_cohort_id AS BIGINT) cohort_definition_id,
	CAST(mc.subject_id AS BIGINT) subject_id,
	CAST(CASE
		WHEN cs.cohort_end_date IS NULL
			THEN mc.candidate_start_date
		ELSE DATEADD(DAY, 1, mc.candidate_start_date)
		END AS DATE) AS cohort_start_date,
	CAST(CASE
		WHEN ce.cohort_end_date IS NULL
			THEN mc.candidate_end_date
		ELSE DATEADD(DAY, - 1, mc.candidate_end_date)
		END AS DATE) AS cohort_end_date
FROM @temp_table_2 mc
LEFT JOIN @temp_table_1 cs ON mc.subject_id = cs.subject_id
	AND mc.candidate_start_date = cs.cohort_end_date
LEFT JOIN @temp_table_1 ce ON mc.subject_id = ce.subject_id
	AND mc.candidate_end_date = ce.cohort_start_date;
	
DROP TABLE IF EXISTS @temp_table_1;
DROP TABLE IF EXISTS @temp_table_2;
