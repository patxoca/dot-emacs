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

from zope.interface import implements

from AccessControl import ClassSecurityInfo
from AccessControl import Permissions
from Globals import DTMLFile
from Globals import InitializeClass
from OFS.SimpleItem import SimpleItem

from Products.(>>>PACKAGE<<<).interfaces import (>>>INTERFACE<<<)


class (>>>CLASSNAME<<<)(SimpleItem) :
    """
    """

    implements((>>>INTERFACE<<<))

    security = ClassSecurityInfo()

    def __init__(self, id, title='') :
        """
        """
        raise NotImplementedError()

InitializeClass((>>>CLASSNAME<<<))

def manage_add(>>>CLASSNAME<<<)(container, id, title, REQUEST=None) :
    """
    """
    raise NotImplementedError()
    obj = (>>>CLASSNAME<<<)(id, title)
    container._setObj(obj.getId(), obj)
    if REQUEST is not None :
        REQUEST.RESPONSE.redirect('manage_main')



>>>TEMPLATE-DEFINITION-SECTION<<<
("CLASSNAME" "Class name: " "" "" "")
("INTERFACE" "Interface name: " "" "" "")
("PACKAGE" "Package name: " "" "" "")
