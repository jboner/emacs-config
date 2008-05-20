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
# (C)2000, David C. Fox
#
##############################################################################

"""abstract class defining interface for an object which receives 
recognition-starting (or onBegin/gotBegin) callbacks, figures out which
application and buffer are active, and tells the GramMgr to activate the
appropriate grammars.
"""

import debug
import string
import re
from Object import Object

import TargetWindow

class WinIDClient(Object):
    """abstract class defining an interface for identifying the active
    editor window among remote windows which appear as subwindows of a 
    SingleWindowDisplay window

    **INSTANCE ATTRIBUTES**

    *INT* window -- the handle of the local window corresponding to the
    SingleWindowDisplay

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, **args):
        """abstract base class, so no arguments

        **INPUTS**

        *none*

        **OUTPUTS**
    
        *none*
        """
        self.deep_construct(WinIDClient,
                            {'window': None},
                            args)
    def local_window(self):
        """return the local window corresponding to the client

        **INPUTS**

        *none*

        **OUTPUTS**

        *INT* -- the window handle of the local window corresponding to
        the client, or None if the client has not yet been identified
        with a local window
        """
        return self.window

    def new_window(self, window, title, editors, instance_name = None):
        """factory which creates a new TargetWindow object for a
        SingleWindowDisplay.

        **INPUTS**

        *INT* window -- the window handle (unique identifier) of the
        window

        *STR* title -- the current title of the window

        *AppMgr* editors -- the AppMgr object

        *STR* instance -- the name of the initial instance belonging to
        the window, or None if there is none initially

        **OUTPUTS**

        *TargetWindow* -- a new TargetWindow object, or None
        if the module is unable to create one
        """
        debug.virtual('WinIDClient.new_window')

# defaults for vim - otherwise ignore
# vim:sw=4

