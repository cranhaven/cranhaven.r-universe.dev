# example

bpmn_instance <-
  create_bpmn(
    data.frame(
      "id" = c("id_tsk_1", "id_tsk_2", "id_tsk_3"),
      "name" = c("task_1", "task_2", "task_3"),
      "incoming" = c("id_sf_1", "id_sf_4", "id_sf_5"),
      "outgoing" = c("id_sf_2", "id_sf_5", "id_sf_6"),
      stringsAsFactors = FALSE
    ),
    data.frame(
      "id" = c(
        "id_sf_1",
        "id_sf_2",
        "id_sf_3",
        "id_sf_4",
        "id_sf_5",
        "id_sf_6"
      ),
      "name" = c(
        "sequence_flow_1",
        "sequence_flow_2",
        "sequence_flow_3",
        "sequence_flow_4",
        "sequence_flow_5",
        "sequence_flow_6"
      ),
      "sourceRef" = c(
        "id_se",
        "id_tsk_1",
        "id_gw_1",
        "id_gw_1",
        "id_tsk_2",
        "id_tsk_3"
      ),
      "targetRef" = c(
        "id_tsk_1",
        "id_gw_1",
        "id_ee_1",
        "id_tsk_2",
        "id_tsk_3",
        "id_ee_2"
      ),
      stringsAsFactors = FALSE
    ),
    data.frame(
      "id" = c("id_gw_1"),
      "name" = c("gateway_1"),
      "gatewayType" = c("exclusiveGateway"),
      "gatewayDirection" = c("Diverging"),
      "incoming" = c("id_sf_2"),
      "outgoing_1" = c("id_sf_3"),
      "outgoing_2" = c("id_sf_4"),
      stringsAsFactors = FALSE
    ),
    data.frame(
      "id" = "id_se",
      "name" = "start_event",
      "outgoing" = "id_sf_1",
      stringsAsFactors = FALSE
    ),
    data.frame(
      "id" = c("id_ee_1", "id_ee_2"),
      "name" = c("end_event_1", "end_event_2"),
      "incoming" = c("id_sf_3", "id_sf_6"),
      stringsAsFactors = FALSE
    )
  )

bpmn_instance
