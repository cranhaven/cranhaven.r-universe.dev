{!@is_temp_table} ? {
DELETE FROM {@target_cohort_database_schema != ''} ? {@target_cohort_database_schema.@target_cohort_table} : {@target_cohort_table}
WHERE cohort_definition_id IN (SELECT DISTINCT new_cohort_id FROM #old_to_new_cohort_id);


INSERT INTO {@target_cohort_database_schema != ''} ? {@target_cohort_database_schema.@target_cohort_table} : {@target_cohort_table}
}
SELECT target.new_cohort_id cohort_definition_id,
        source.subject_id,
        source.cohort_start_date,
        source.cohort_end_date
{@is_temp_table} ? {
INTO @target_cohort_table
}
FROM {@source_cohort_database_schema != ''} ? {@source_cohort_database_schema.@source_cohort_table} : {@source_cohort_table} source
INNER JOIN #old_to_new_cohort_id target
ON source.cohort_definition_id = target.old_cohort_id;

{!@is_temp_table} ? {
UPDATE STATISTICS  {@target_cohort_database_schema != ''} ? {@target_cohort_database_schema.@target_cohort_table} : {@target_cohort_table};
}
