cl-trivial-templates
====================

This package is for quick and dirty generation of text (mainly source code) from templates.

```lisp
CL-USER> (defparameter template "###a### ###b### ###c###")
CL-USER> (finalize-template!) ; finalization removes all template symbols from template
"  "
CL-USER> (ttt-> :a "foo")
"foo
###a### ###b### ###c###"
CL-USER> (ttt-> :b "bar")
"foo
###a### bar
###b### ###c###"
CL-USER> (finalize-template!)
"foo
 bar
 "
```

So, template is any string, and any substring there matching "###[a-z0-9-]+###"
can be used as an "insertion point".

The whole piece is to facilitate programming by side-effects, which is, of course,
generally a bad practive, but suits perfectly, if all you need is a quick write-only code.