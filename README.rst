.. -*- ispell-local-dictionary: "british" -*-

My personal emacs configuration.

Quickstart
==========

Requirements:

- ``install-info``: required by org


.. code-block:: bash

   mkdir ~/.emacs.d
   cd ~/.emacs.d
   git clone https://github.com/patxoca/dot-emacs.git conf.d
   ln -s conf.d/init.el .
   emacs

.. warning:: Currently the ``conf.d`` directory is hardcoded in the
             configuration and cloning the repository to other
             directory will not work.

It may take some time to download and install all required packages.
