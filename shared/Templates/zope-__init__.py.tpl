# -*- coding: utf-8 -*-

##############################################################################
#
# Copyright (c) 2007 Alexis Roda. All Rights Reserved.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.
#
##############################################################################

"""

$Id: $
"""

try:
    __version__ = int("$Id: $".split(' ')[2])
except :
    __version__ = 0

# from AccessControl import ModuleSecurityInfo
from Globals import ImageFile
import OFS

# from Products.CMFCore.DirectoryView import registerDirectory
# from Products.Formulator import FSForm # registra FSForm amb DirectoryView


from Products.(>>>PRODUCTNAME<<<).(>>>PACKAGE<<<) import (>>>MAINCLASS<<<), manage_add(>>>MAINCLASS<<<)Form, manage_add(>>>MAINCLASS<<<)

# ModuleSecurityInfo('Products').declarePublic('(>>>PRODUCTNAME<<<)')
# ModuleSecurityInfo('Products.(>>>PRODUCTNAME<<<)').declarePublic('SafeConfigParser')


# registra els directoris
#registerDirectory('fssite/admin', globals())

_misc_ns = '(>>>MISCNS<<<)'


def initialize(registrar) :
    registrar.registerClass(
        (>>>MAINCLASS<<<),
        constructors = (manage_add(>>>MAINCLASS<<<)Form, manage_add(>>>MAINCLASS<<<))
        )

    if not hasattr(OFS.misc_.misc_, _misc_ns):
        setattr(OFS.misc_.misc_, _misc_ns, OFS.misc_.Misc_(_misc_ns, {}))

    # registra les icones
    icon_image = ImageFile('www/workflow.gif', globals())
    icon_image.__roles__ = None
    getattr(OFS.misc_.misc_, _misc_ns)['workflow'] = icon_image

    # registrar.registerHelp()


>>>TEMPLATE-DEFINITION-SECTION<<<
("PRODUCTNAME" "Product name: " "" "" "")
("PACKAGE"     "Module name: " "" "" "")
("MAINCLASS"   "Class name: " "" "" "")
("MISCNS"      "Misc namespace: " "" "" "")
