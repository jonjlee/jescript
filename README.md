`jescript` aims to be an Erlang interpreter that runs on the JVM.  It is in the very early stages of development.

Build status can be found on this [CloudBees Jenkins instance](https://jlee.ci.cloudbees.com/job/jescript/).

References
==========

**Lexer**:

* [core_scan.erl in Erlang source](https://github.com/erlang/otp/blob/master/lib/compiler/src/core_scan.erl) (definitions for "char", "integer", "float", "atom", "string", and "var")
* [Erlang tutorial](http://www.erlang.org/course/course.html)
* [Escape sequences, Erlang reference manual](http://www.erlang.org/doc/reference_manual/data_types.html#id73169)
* 

**Grammar**:

* [Erlang 4.7.3 Reference Manual DRAFT 0.7 (especially Section E.2, The Main Grammar)](www.erlang.org/download/erl_spec47.ps.gz)
* [core_parse.yrl in Erlang source](https://github.com/erlang/otp/blob/master/lib/compiler/src/core_parse.yrl)
* [Comments on mailing list from Joe Armstrong](http://groups.google.com/group/erlang-programming/browse_thread/thread/56ba059bacd1009b)
* [LL grammar written for Emacs integration](http://cedet.bzr.sourceforge.net/bzr/cedet/code/trunk/annotate/head%3A/semantic/bovine/erlang.by)

**Intermediate Representation**:

* [An introduction to Core Erlang](www.erlang.se/workshop/carlsson.ps)
* [Core Erlang 1.0.3 language specification](http://www.it.uu.se/research/group/hipe/cerl/)
