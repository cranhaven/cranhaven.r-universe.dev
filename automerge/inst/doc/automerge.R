## -----------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
# # From R-universe
# install.packages("automerge", repos = "https://posit-dev.r-universe.dev")
# 
# # From GitHub
# pak::pak("posit-dev/automerge-r")

## -----------------------------------------------------------------------------
library(automerge)

## -----------------------------------------------------------------------------
doc <- am_create()
print(doc)

## -----------------------------------------------------------------------------
am_put(doc, AM_ROOT, "name", "Alice")
am_put(doc, AM_ROOT, "age", 30L)
am_put(doc, AM_ROOT, "active", TRUE)
am_commit(doc, "Initial data")

am_get(doc, AM_ROOT, "name")
am_get(doc, AM_ROOT, "age")

## -----------------------------------------------------------------------------
doc[["email"]] <- "alice@example.com"
doc[["score"]] <- 95.5

doc[["name"]]
doc[["age"]]

# List all keys
names(doc)

## -----------------------------------------------------------------------------
doc2 <- am_create() |>
  am_put(AM_ROOT, "name", "Bob") |>
  am_put(AM_ROOT, "age", 25L) |>
  am_put(AM_ROOT, "active", TRUE) |>
  am_commit("Initial setup")

doc2 |> am_get(AM_ROOT, "name")

am_close(doc2)

## -----------------------------------------------------------------------------
# Create document with nested structure in one call
doc3 <- am_create() |>
  am_put(
    AM_ROOT,
    "company",
    list(
      name = "Acme Corp",
      founded = 2020L,
      employees = list(
        list(name = "Alice", role = "Engineer"),
        list(name = "Bob", role = "Designer")
      ),
      office = list(
        address = list(
          street = "123 Main St",
          city = "Boston",
          zip = 02101L
        ),
        size = 5000.5
      )
    )
  ) |>
  am_commit("Add company data")

# Access nested data (verbose way)
company <- doc3[["company"]]
office <- am_get(doc3, company, "office")
address <- am_get(doc3, office, "address")
am_get(doc3, address, "city")

## -----------------------------------------------------------------------------
# Much simpler - use path-based access
am_get_path(doc3, c("company", "office", "address", "city"))

# Create deep structure using paths
doc4 <- am_create()

am_put_path(doc4, c("config", "database", "host"), "localhost")
am_put_path(doc4, c("config", "database", "port"), 5432L)
am_put_path(doc4, c("config", "cache", "enabled"), TRUE)
am_put_path(doc4, c("config", "cache", "ttl"), 3600L)

# Retrieve values with paths
am_get_path(doc4, c("config", "database", "host"))

am_close(doc3)
am_close(doc4)

## -----------------------------------------------------------------------------
# Your existing R data
config_data <- list(
  app_name = "MyApp",
  version = "1.0.0",
  database = list(
    host = "localhost",
    port = 5432L,
    credentials = list(
      user = "admin",
      password_hash = "..."
    )
  ),
  features = list("auth", "api", "websocket")
)

# Convert to Automerge document
doc5 <- as_automerge(config_data)
am_commit(doc5, "Initial configuration")

# Easy access with paths
am_get_path(doc5, c("database", "port"))

am_close(doc5)

## -----------------------------------------------------------------------------
# Create a document with a list
doc6 <- am_create()
am_put(doc6, AM_ROOT, "items", AM_OBJ_TYPE_LIST)
items <- am_get(doc6, AM_ROOT, "items")

# Insert items
am_insert(doc6, items, 1, "first") # Insert at index 1
am_insert(doc6, items, 2, "second") # Insert at index 2
am_insert(doc6, items, 3, "third") # Insert at index 3

# Or use the "end" marker to append
am_insert(doc6, items, "end", "fourth")
am_put(doc6, items, "end", "fifth")

# Get list length
am_length(doc6, items)

# Access by index
am_get(doc6, items, 1)
am_get(doc6, items, 2)

am_close(doc6)

## -----------------------------------------------------------------------------
doc7 <- am_create()

# Regular string (last-write-wins)
am_put(doc7, AM_ROOT, "title", "My Document")

# Text object (CRDT - supports collaborative editing)
am_put(doc7, AM_ROOT, "content", am_text("Initial content"))
text_obj <- am_get(doc7, AM_ROOT, "content")

# Text supports character-level operations
# For the text "Hello":
#  H e l l o
# 0 1 2 3 4 5  <- positions (0-based, between characters)

am_text_splice(text_obj, 8, 0, "amazing ") # Insert at position 8
am_text_content(text_obj)

# For collaborative editors, use am_text_update() which computes
# and applies the minimal diff in one step:
old_text <- am_text_content(text_obj)
am_text_update(text_obj, old_text, "New content from user input")
am_text_content(text_obj)

am_close(doc7)

## -----------------------------------------------------------------------------
doc8 <- am_create()

# Create a counter
am_put(doc8, AM_ROOT, "score", am_counter(0))

am_counter_increment(doc8, AM_ROOT, "score", 10)
am_counter_increment(doc8, AM_ROOT, "score", 5)
am_counter_increment(doc8, AM_ROOT, "score", -3)

doc8[["score"]]

am_close(doc8)

## -----------------------------------------------------------------------------
doc9 <- am_create()

am_put(doc9, AM_ROOT, "created_at", Sys.time())
am_put(doc9, AM_ROOT, "updated_at", Sys.time())

doc9[["created_at"]]

am_close(doc9)

## -----------------------------------------------------------------------------
# Save to binary format
bytes <- am_save(doc)

# Save to file
temp_file <- tempfile(fileext = ".automerge")
writeBin(bytes, temp_file)

# Load from binary
doc_loaded <- am_load(bytes)

# Or load from file
doc_from_file <- am_load(readBin(temp_file, "raw", 1e6))

# Verify data persisted
doc_from_file[["name"]]

am_close(doc)
am_close(doc_loaded)
am_close(doc_from_file)

## -----------------------------------------------------------------------------
doc_inc <- am_create()
doc_inc[["v1"]] <- "first"
am_commit(doc_inc, "Version 1")

# Full save (resets incremental tracking)
full_bytes <- am_save(doc_inc)

# Make more changes
doc_inc[["v2"]] <- "second"
am_commit(doc_inc, "Version 2")

# Incremental save: only the new changes
inc_bytes <- am_save_incremental(doc_inc)
length(inc_bytes) < length(full_bytes) # TRUE - much smaller

# Apply incremental changes to another copy
doc_inc2 <- am_load(full_bytes)
am_load_incremental(doc_inc2, inc_bytes)
doc_inc2[["v2"]] # "second"

am_close(doc_inc)
am_close(doc_inc2)

## -----------------------------------------------------------------------------
doc10 <- am_create()

# Make changes
doc10[["x"]] <- 1
doc10[["y"]] <- 2

# Check how many uncommitted operations exist
am_pending_ops(doc10)

# Commit with message
am_commit(doc10, "Add x and y coordinates")

# Make more changes
doc10[["z"]] <- 3
am_commit(doc10, "Add z coordinate")

## -----------------------------------------------------------------------------
doc11 <- am_fork(doc10)

# Changes to fork don't affect original
doc11[["w"]] <- 4
doc10[["w"]] # NULL - not in original

# am_clone() is another way to create an independent deep copy
doc11b <- am_clone(doc10)

# Compare documents for equality
am_equal(doc10, doc11b) # TRUE - same state

am_close(doc10)
am_close(doc11)
am_close(doc11b)

## -----------------------------------------------------------------------------
# Create two documents
doc12 <- am_create()
doc12[["source"]] <- "doc12"
doc12[["value1"]] <- 100

doc13 <- am_create()
doc13[["source"]] <- "doc13"
doc13[["value2"]] <- 200

# Merge doc13 into doc12
am_merge(doc12, doc13)

# doc12 now has both values
doc12[["value1"]]
doc12[["value2"]]
doc12[["source"]] # One value wins deterministically for conflicting keys

am_close(doc12)
am_close(doc13)

## -----------------------------------------------------------------------------
# Create two peers
peer1 <- am_create()
peer1[["edited_by"]] <- "peer1"
peer1[["data1"]] <- 100
am_commit(peer1)

peer2 <- am_create()
peer2[["edited_by"]] <- "peer2"
peer2[["data2"]] <- 200
am_commit(peer2)

# Bidirectional sync (documents modified in place)
rounds <- am_sync(peer1, peer2)
rounds

# Both documents now have all data
peer1[["data1"]]
peer1[["data2"]]
peer2[["data1"]]
peer2[["data2"]]

am_close(peer1)
am_close(peer2)

## -----------------------------------------------------------------------------
doc14 <- am_create()
doc14[["title"]] <- "My Project"
am_commit(doc14, "Initial setup", Sys.time())

doc14[["version"]] <- "1.0"
am_commit(doc14, "Set version", Sys.time())

# Get the full history (returns am_change objects directly)
history <- am_get_changes(doc14)
cat("Document has", length(history), "change(s)\n")

# Inspect each change - no parsing needed
for (i in seq_along(history)) {
  cat(
    sprintf(
      "Change %d: seq=%g, message=%s\n",
      i,
      am_change_seq(history[[i]]),
      am_change_message(history[[i]]) %||% "(none)"
    )
  )
}

# Extract many fields from the same change
change <- history[[2]]
am_change_hash(change)     # Unique 32-byte hash
am_change_message(change)  # Commit message
am_change_time(change)     # Timestamp
am_change_seq(change)      # Sequence number
am_change_actor_id(change) # Who made the change
am_change_deps(change)     # Parent change hashes
am_change_size(change)     # Number of operations

am_close(doc14)

## -----------------------------------------------------------------------------
doc_tt <- am_create()
doc_tt[["title"]] <- "My Report"
doc_tt[["status"]] <- "draft"
am_commit(doc_tt, "Initial draft")

# Save a checkpoint before making further changes
checkpoint <- am_get_heads(doc_tt)

# Continue editing
doc_tt[["status"]] <- "review"
doc_tt[["reviewer"]] <- "Bob"
am_commit(doc_tt, "Send for review")

doc_tt[["status"]] <- "rejected"
am_commit(doc_tt, "Rejected by reviewer")

# Oops - let's go back to the draft state
draft <- am_fork(doc_tt, checkpoint)
draft[["status"]]    # "draft"
draft[["reviewer"]]  # NULL - this key didn't exist yet

am_close(doc_tt)
am_close(draft)

## -----------------------------------------------------------------------------
doc_hist <- am_create()

doc_hist[["data"]] <- list(x = 1, y = 2)
am_commit(doc_hist, "Add initial data")

doc_hist[["data"]] <- list(x = 1, y = 2, z = 3)
am_commit(doc_hist, "Add z coordinate")

doc_hist[["data"]] <- list(x = 10, y = 20, z = 30)
am_commit(doc_hist, "Scale all values by 10")

# Review the history
history <- am_get_changes(doc_hist)
for (i in seq_along(history)) {
  cat(sprintf("  [%d] %s\n", i, am_change_message(history[[i]])))
}

# We want to undo the scaling - go back to change 2
target <- history[[2]]
reverted <- am_fork(doc_hist, list(am_change_hash(target)))
from_automerge(reverted)

am_close(doc_hist)
am_close(reverted)

## -----------------------------------------------------------------------------
doc_branch <- am_create()
doc_branch[["text"]] <- am_text("Hello World")
am_commit(doc_branch, "Initial text")

# Save checkpoint
v1_heads <- am_get_heads(doc_branch)

text_obj <- am_get(doc_branch, AM_ROOT, "text")
am_text_update(text_obj, "Hello World", "Hello World - CONFIDENTIAL")
am_commit(doc_branch, "Add confidential marker")

# Create a public version from v1 and take it in a different direction
public <- am_fork(doc_branch, v1_heads)
public_text <- am_get(public, AM_ROOT, "text")
am_text_update(public_text, "Hello World", "Hello World - Public Draft")
am_commit(public, "Public version")

am_text_content(public_text) # "Hello World - Public Draft"

am_close(doc_branch)
am_close(public)

## -----------------------------------------------------------------------------
doc15 <- am_create()
am_put(doc15, AM_ROOT, "content", am_text("Hello World"))
text_obj <- am_get(doc15, AM_ROOT, "content")

# Create a cursor and serialize it
cursor <- am_cursor(text_obj, 5)
cursor_bytes <- am_cursor_to_bytes(cursor)
cursor_str <- am_cursor_to_string(cursor)

# Later: restore the cursor
restored <- am_cursor_from_bytes(cursor_bytes, text_obj)
am_cursor_position(restored)  # 5

# Compare cursors
am_cursor_equal(cursor, restored)  # TRUE

am_close(doc15)

