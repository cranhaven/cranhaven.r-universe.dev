DROP TABLE IF EXISTS @temp_output_table;

SELECT CAST((c.cohort_definition_id * 1000) + ri.offset_id AS BIGINT) cohort_definition_id,
        CAST(c.subject_id AS BIGINT) AS subject_id,
        CAST(CASE
            WHEN op.observation_period_start_date >= DATEADD(DAY, ri.offset_start_value, c.@offset_start_anchor) THEN op.observation_period_start_date
                  ELSE DATEADD(DAY, ri.offset_start_value, c.@offset_start_anchor) 
        END AS DATE) cohort_start_date,
        CAST(CASE
            WHEN op.observation_period_end_date <= DATEADD(DAY, ri.offset_end_value, c.@offset_end_anchor) THEN op.observation_period_end_date
                  ELSE DATEADD(DAY, ri.offset_end_value, c.@offset_end_anchor) 
        END AS DATE) cohort_end_date
INTO @temp_output_table
FROM {@source_cohort_database_schema != ''} ? {@source_cohort_database_schema.@source_cohort_table} : {@source_cohort_table} c
CROSS JOIN @reindex_rules_table ri
INNER JOIN @cdm_database_schema.observation_period op
ON c.subject_id = op.person_id
WHERE DATEADD(DAY, ri.offset_start_value, c.@offset_start_anchor) <= DATEADD(DAY, ri.offset_end_value, c.@offset_end_anchor)
    AND op.observation_period_start_date <= DATEADD(DAY, ri.offset_end_value, c.@offset_end_anchor)
    AND op.observation_period_end_date >= DATEADD(DAY, ri.offset_start_value, c.@offset_start_anchor)
    AND c.cohort_definition_id IN (@source_cohort_id)
;

