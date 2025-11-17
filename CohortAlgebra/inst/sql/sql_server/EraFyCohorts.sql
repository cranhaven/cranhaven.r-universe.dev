DROP TABLE IF EXISTS #cohort_era;

-- cohort era logic originally written by @chrisknoll
SELECT subject_id,
	min(cohort_start_date) AS cohort_start_date,
	DATEADD(day, - 1 * @era_constructor_pad, max(cohort_end_date)) AS cohort_end_date
INTO #cohort_era
FROM (
	SELECT subject_id,
		cohort_start_date,
		cohort_end_date,
		sum(is_start) OVER (
			PARTITION BY subject_id ORDER BY cohort_start_date,
				is_start DESC rows unbounded preceding
			) group_idx
	FROM (
		SELECT subject_id,
			cohort_start_date,
			cohort_end_date,
			CASE 
				WHEN max(cohort_end_date) OVER (
						PARTITION BY subject_id ORDER BY cohort_start_date rows BETWEEN unbounded preceding
								AND 1 preceding
						) >= cohort_start_date
					THEN 0
				ELSE 1
				END is_start
		FROM (
			SELECT subject_id,
				cohort_start_date,
				DATEADD(day, @era_constructor_pad, cohort_end_date) AS cohort_end_date
			FROM {@source_cohort_database_schema != ''} ? {@source_cohort_database_schema.@source_cohort_table} : {@source_cohort_table}
			WHERE cohort_definition_id IN (@old_cohort_ids)
			) CR
		) ST
	) GR
GROUP BY subject_id,
	group_idx;
	

{@is_temp_table} ? {
  DROP TABLE IF EXISTS @target_cohort_table;
  
  SELECT  CAST(@new_cohort_id AS BIGINT) cohort_definition_id,
          CAST(subject_id AS BIGINT) subject_id,
          CAST(cohort_start_date AS DATE) cohort_start_date, 
          CAST(cohort_end_date AS DATE) cohort_end_date
  INTO @target_cohort_table
} : {
  DELETE FROM 
  {@target_cohort_database_schema != ''} ? {
    @target_cohort_database_schema.@target_cohort_table
  } : {@target_cohort_table} 
  WHERE cohort_definition_id = @new_cohort_id;
  	
  INSERT INTO 
  {@target_cohort_database_schema != ''} ? {
    @target_cohort_database_schema.@target_cohort_table
  } : {@target_cohort_table} 
  SELECT  CAST(@new_cohort_id AS BIGINT) cohort_definition_id,
          CAST(subject_id AS BIGINT) subject_id,
          CAST(cohort_start_date AS DATE) cohort_start_date, 
          CAST(cohort_end_date AS DATE) cohort_end_date
}
  FROM    
  --HINT DISTRIBUTE ON KEY (subject_id)	
{@cdm_database_schema != ''} ?
{
    (
      SELECT 
          ce.subject_id,
          ce.cohort_start_date,
          CASE WHEN op.observation_period_end_date > ce.cohort_end_date then ce.cohort_end_date
              ELSE op.observation_period_end_date END AS cohort_end_date
      FROM #cohort_era ce
      INNER JOIN @cdm_database_schema.observation_period op
      ON ce.subject_id = op.person_id
      WHERE op.observation_period_start_date <= ce.cohort_start_date
            AND op.observation_period_end_date >= ce.cohort_start_date
      ) f
} :
{ #cohort_era f}
;
 
DROP TABLE IF EXISTS #cohort_era;
