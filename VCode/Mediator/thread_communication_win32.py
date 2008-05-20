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
# (C)2002, National Research Council of Canada
#
##############################################################################

"""classes supporting inter-thread communication
"""

import win32event
from thread_communication import *

class Win32InterThreadEvent(InterThreadEvent):
    """implementation of InterThreadEvent using the win32event module.

    **INSTANCE ATTRIBUTES**

    *PyHandle event* -- Win32 event to raised to notify the main thread

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, event, **args):
        """
        **INPUTS**

        *PyHandle event* -- Win32 event to raised to notify the main thread
        """
        self.deep_construct(Win32InterThreadEvent,
                            {'event': event},
                            args)
    def notify(self):
        """send the message, and return asynchronously

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        win32event.SetEvent(self.event)

class Win32SomeSocketHasDataEvent(SocketHasDataEvent):
    """implementation of SocketHasDataEvent using win32event.
    
    There doesn't seem to be any way to pass data on with a win32event.Event.
    For now, we just let ServerOldMediatorWin32Evt check all sockets.

    **INSTANCE ATTRIBUTES**

    *PyHandle event* -- Win32 event to raised to notify the main thread

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, event, **args):
        """
        **INPUTS**

        *PyHandle event* -- Win32 event to raised to notify the main thread
        """
        self.deep_construct(Win32SomeSocketHasDataEvent,
                            {'event': event},
                            args)
    def notify(self):
        """send the message, and return asynchronously

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        win32event.SetEvent(self.event)


