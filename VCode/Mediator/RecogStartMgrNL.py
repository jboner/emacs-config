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
import RecogStartMgr
import natlink
from natlinkutils import *

class RecogStartGram(GrammarBase):
    """a dummy grammar used only to capture recognition-starting events,
    without interfering with the global setBeginCallback from
    natlinkmain which loads natlink python macros (and reloads modified
    ones)
    """

    gramSpec = """
        <start> exported = {emptyList};
    """

    def __init__(self):
        GrammarBase.__init__(self)
# GrammarBase is a natlinkutils class - it doesn't follow our
# constructor convention, so you shouldn't pass keyword arguments
# through to it.
#        apply(GrammarBase.__init__, [self], args)

    def initialize(self, callback = None):
        self.callback = callback
        self.load(self.gramSpec)
        self.activateAll()

    def gotBegin(self, module_info):
        debug.trace('RecogStartGram.gotBegin', '** invoked, self.callback=%s, self=%s' % (self.callback, self))
        if self.callback != None:
            self.callback(module_info)

class RecogStartMgrNL(RecogStartMgr.RSMBasic):
    """abstract class defining interface for an object which receives 
    recognition-starting (or onBegin/gotBegin) callbacks, figures out which
    application and buffer are active, and tells the GramMgr to activate the
    appropriate grammars.

    **INSTANCE ATTRIBUTES**

    *RecogStartGram* start_gram -- dummy grammar used to capture the
    recognition-starting event without interfering with user-defined
    natlink macros

    *none*

    **CLASS ATTRIBUTES**
    
    *none*
    """

    def __init__(self, **args):
        self.deep_construct(RecogStartMgrNL,
                            {'start_gram': RecogStartGram(),
                            },
                            args)
        self.start_gram.initialize(self.starting)

    def remove_other_references(self):
        self.deactivate()
        self.start_gram.unload()
        del self.start_gram
        RecogStartMgr.RSMBasic.remove_other_references(self)

    def parse_module_info(self, module_info):
        """rearrange natlink's module_info in our format
        
        **INPUTS**

        *(STR, STR, INT)* -- the module name, window title, and window
        handle

        **OUTPUTS**

        *(INT, STR, STR)* -- the window id, title, and module name
        """
        module_path, title, handle = module_info
        module = os.path.basename(module_path)
        module = os.path.splitext(module)[0]
        module = string.lower(module)
        return handle, title, module

    def window_info(self):
        """find the window id, title, and module of the current window

        **INPUTS**

        *none*

        **OUTPUTS**

        *(INT, STR, STR)* -- the window id, title, and module name.  The
        module name should be converted to all lowercase
        """
        return self.parse_module_info(natlink.getCurrentModule())

    def starting(self, module_info):
        debug.trace('RecogStartMgrNL.starting', 'self._recognition_starting=%s, self=%s' % (self._recognition_starting, self))
        apply(self._recognition_starting, self.parse_module_info(module_info))


# defaults for vim - otherwise ignore
# vim:sw=4

