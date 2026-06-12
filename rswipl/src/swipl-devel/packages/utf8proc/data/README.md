# UCD inputs for the table generators

The files in this directory feed the generators under `etc/`:

- `etc/gen_uts39.pl` emits the checked-in `uts39_data.{c,h}` compiled
  into `library(unicode_security)`.
- `etc/gen_uniname.pl` emits the checked-in compact Unicode name table
  `uniname_data.{c,h}` (prototype) from `UnicodeData.txt`.

The generators run only on demand (`ninja regen-uts39` /
`ninja regen-uniname` after a UCD version bump); the `.txt` files are
**not** checked in.

## UnicodeData.txt (for `gen_uniname.pl`)

From `https://www.unicode.org/Public/<version>/ucd/`:

- `UnicodeData.txt`

`UnicodeData.txt` carries no version line; pass `--version` to the
generator (the `regen-uniname` target pins it to match the UCD set).

## Fetching

[UTS #39 (Unicode Security Mechanisms)](https://www.unicode.org/reports/tr39/)
— from `https://www.unicode.org/Public/security/<version>/` (or
`security/latest/` for the most recent release):

- `confusables.txt`
- `intentional.txt`
- `IdentifierStatus.txt`
- `IdentifierType.txt`

[UAX #24 (Script_Property, Script_Extensions)](https://www.unicode.org/reports/tr24/)
— from `https://www.unicode.org/Public/<version>/ucd/`:

- `Scripts.txt`
- `ScriptExtensions.txt`
- `PropertyValueAliases.txt` (only the `sc` lines are consumed)

The UTS #39 version may lag the main UCD by one minor release; use the
latest matching pair available on unicode.org.

## Quick fetch (UTS #39 17.0.0 + Unicode 17.0)

    cd packages/utf8proc/data
    base_sec=https://www.unicode.org/Public/security/latest
    base_ucd=https://www.unicode.org/Public/17.0.0/ucd
    for f in confusables.txt intentional.txt \
             IdentifierStatus.txt IdentifierType.txt; do
      curl -sSfO "$base_sec/$f"
    done
    for f in Scripts.txt ScriptExtensions.txt PropertyValueAliases.txt; do
      curl -sSfO "$base_ucd/$f"
    done

After fetching, regenerate from a configured build directory:

    ninja regen-uts39

The generator output is deterministic; re-running on unchanged inputs
produces a zero diff against the committed `uts39_data.{c,h}`.

## License

These data files are distributed by Unicode, Inc. under the terms
of [Unicode Terms of Use](https://www.unicode.org/terms_of_use.html)
and the Unicode data-file license linked from each file's header.
