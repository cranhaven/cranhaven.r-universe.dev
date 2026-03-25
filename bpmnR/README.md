# bpmnR

- [x] task 1: make sure package can be installed
- [ ] task 2: what inputs does _create_bpmn()_ expect; rendering bpmn model with _render_bpmn()_
- [ ] task 3: van tekstuele beschrijvingen naar inputs voor _create_bpmn()_

## Task 2
    minimal_subset_attributes_list <-
      list(
        tasks = c("id", "name"),
        sequenceFlows = c("id", "name", "sourceRef", "targetRef"),
        gateways = c("id", "name", "gatewayType", "gatewayDirection"),
        startEvent = c("id", "name"),
        endEvent = c("id", "name")
      )
      
## Task 3

