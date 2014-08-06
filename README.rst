My personal emacs configuration.

This configuration defines independent *emacs instances*, each one
with its own customization and initialization code.

In order to determine the active instance emacs uses the value of the
``EMACS_INSTANCE`` environment variable or, if undefined, the value
returned by the function ``system-name``.

Instances are store in the ``instances`` directory.

The ``init.d`` directory contains small lisp modules that initialize or
customize some emacs aspect.

Each instance's ``init.d`` stores symbolic links to the relevant
initialization modules found in the *global* ``init.d``. ``elc`` files
found in the instances's ``init.d`` are run in order.


Notes
=====

In order to use this config adjust the value of the variable
``emacs-startup-dir``. For historical reasons my config is stored in
``~/emacslib`` not in ``~/.emacs.d``.

The ``Makefile`` is a bit outdated and needs some rework, use with
care. In order to recompile and update the autoloads run the command
``M-x arv/startup-byte-recompile`` from within emacs.

``emodman`` is a very simple shell script (needs some work) that eases
enabling and disabling initializacion modules.

