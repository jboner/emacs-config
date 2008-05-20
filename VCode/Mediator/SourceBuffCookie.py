##############################################################################
# VoiceCode, a programming-by-voice environment
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
#
# (C)2000, National Research Council of Canada
#
##############################################################################

"""interface used for restore-able SourceBuff"""

import debug
from debug import trace
import re, string, sys

from Object import Object, OwnerObject

class SourceBuffCookie(OwnerObject):
    """abstract class which represents a restore-able SourceBuff state

    SourceBuffCookie itself is simply a dummy class which acts as a
    placeholder.  Its only purpose is to serve as ostensible return or 
    argument type for various pure virtual SourceBuff functions.  The
    actual return or argument type will be a subclass of
    SourceBuffCookie which will vary with SourceBuff subclass.

    **INSTANCE ATTRIBUTES**

    *None*

    **CLASS ATTRIBUTES**

    *None*
    """
    def __init__(self, **attrs):
        self.deep_construct(SourceBuffCookie, {}, attrs)

    def rename_buffer_cbk(self, new_buff_name):
        """callback which notifies us that the application
        has renamed the buffer corresponding to this cookie

        **INPUTS**

        *STR* new_buff_name -- new name of the buffer 

        **OUTPUTS**

        *none*
        """
        debug.virtual('SourceBuffCookie.rename_buffer_cbk')

