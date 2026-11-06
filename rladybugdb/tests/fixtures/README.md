# Storage compatibility fixture

`ladybug-v0.15.2.lbug` was created with the LadybugDB 0.15.2 C API. It contains
one `Compatibility` node with `id = 1` and `value = "from 0.15.2"`.

For reproducibility, compile `create-v0.15.2.c` against the 0.15.2 `lbug.h`
and library, then pass the fixture path as its only argument. The binary
fixture is intentionally committed so current-engine tests do not need to
download or execute an old engine.
