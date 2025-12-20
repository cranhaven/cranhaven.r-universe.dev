# YAML support for SWI-Prolog

This    package    provides     a      SWI-Prolog     wrapper     around
[libyaml](https://github.com/yaml/libyaml),  a  popular    C-based  YAML
parser that is used by several languages.

An obvious option to support YAML support is  to write it all in Prolog,
as        done        by        a        another        [YAML        add
on](http://www.swi-prolog.org/pack/list?p=yaml). The YAML   spec however
is rather complicated and several versions   of  YAML have been defined.
This makes it attractive to reuse a   widespread library that provides a
YAML parser that is guaranteed to  be   compatible  with YAML support of
several other languages, dispite the fact   that the wrapper is probably
not much longer than a Prolog parser.

The generated data  structure  is   compatible  with  SWI-Prolog's  JSON
libraries as long a no explicit  non-standard YAML tags (e.g., `!!mytype
value` are used. Documents may be  converted   between  JSON and YAML by
reading using json_read_dict/2 and writing using yaml_write/2 or reading
using yaml_read/2 and writing using  json_write_dict/2   as  long as the
YAML can be expressed in JSON.
