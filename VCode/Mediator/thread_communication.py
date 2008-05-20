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

import Object
import debug

class InterThreadEvent(Object.Object):
    """abstract interface for sending a dataless message to the main thread.
    Particular implementations may use win32 events or wxPython custom
    events.

    **INSTANCE ATTRIBUTES**

    *none*

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, **args):
        """abstract base class so no arguments
        """
        self.deep_construct(InterThreadEvent,
                            {},
                            args)
    def notify(self):
        """send the message, and return synchronously

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        debug.virtual('InterThreadEvent.notify')

class SocketHasDataEvent(Object.Object):
    """abstract interface for sending a message to the main thread 
    indicating that a particular socket has data waiting to be read.

    The concrete subclass will have a reference to the particular
    socket.
    
    Particular implementations may use win32 events or wxPython custom
    events.

    **INSTANCE ATTRIBUTES**

    *none*

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, **args):
        """abstract base class so no arguments
        """
        self.deep_construct(SocketHasDataEvent,
                            {},
                            args)
    def notify(self):
        """send the message, and return synchronously

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        debug.virtual('SocketHasDataEvent.notify')

class CorrectUtteranceEvent(Object.Object):
    """abstract interface for sending a message to the main thread 
    indicating that it should initiate user correction of a given
    utterance.

    Unlike InterThreadEvent and SocketHasDataEvent, this event is
    currently used for asynchronous communication within the main thread.
    Its purpose is to invoke the modal correction box, while letting the 
    correction grammar's on_results method return immediately, so as to 
    allow speech input to the correction box (or other windows).

    Particular implementations may wxPython custom
    events or other means to communicate with the main thread.

    **INSTANCE ATTRIBUTES**

    *none*

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, **args):
        self.deep_construct(CorrectUtteranceEvent,
                            {},
                            args)
    def notify(self, instance_name, utterance_number):
        """send the message, and return synchronously

        **INPUTS**

        *STR instance_name* -- unique name identifying the editor
        instance

        *INT utterance_number* -- the number assigned by
        ResMgr.interpret_dictation to the utterance to be corrected

        **OUTPUTS**

        *none*
        """
        debug.virtual('CorrectUtteranceEvent.notify')

class CorrectRecentEvent(Object.Object):
    """abstract interface for sending a message to the main thread 
    indicating that it should initiate a dialog for the user to select
    and correct one or more recent utterances.

    Unlike InterThreadEvent and SocketHasDataEvent, this event is
    currently used for asynchronous communication within the main thread.
    Its purpose is to invoke the modal correct recent box, while letting the 
    correction grammar's on_results method return immediately, so as to 
    allow speech input to the correct recent box (or other windows).

    Particular implementations may use wxPython custom
    events or other means to communicate with the main thread.

    **INSTANCE ATTRIBUTES**

    *none*

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, **args):
        self.deep_construct(CorrectRecentEvent,
                            {},
                            args)
    def notify(self, instance_name):
        """send the message, and return synchronously

        **INPUTS**

        *STR instance_name* -- unique name identifying the editor
        instance

        **OUTPUTS**

        *none*
        """
        debug.virtual('CorrectRecentEvent.notify')

class CorrectNthEvent(Object.Object):
    """abstract interface for sending a message to the Correct Recent
    dialog indicating that it should initiate user correction of a given
    utterance.

    Unlike InterThreadEvent and SocketHasDataEvent, this event is
    currently used for asynchronous communication within the main thread.
    Its purpose is to invoke the modal correction box, while letting the 
    correct_n grammar's on_results method return immediately, so as to 
    allow speech input to the correction box (or other windows).

    Particular implementations may wxPython custom
    events or other means to communicate with the main thread.

    **INSTANCE ATTRIBUTES**

    *none*

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, **args):
        self.deep_construct(CorrectNthEvent,
                            {},
                            args)
    def notify(self, recent_chosen):
        """send the message, and return synchronously

        **INPUTS**

        *INT recent_chosen* -- the number of the utterance in the list
        of recent utterances (e.g. 5 if the user says "Correct 5")

        **OUTPUTS**

        *none*
        """
        debug.virtual('CorrectNthEvent.notify')

class ReformatSymbolEvent(Object.Object):
    """Abstract interface for sending a message to the main thread 
    indicating that it should initiate reformatting of a given
    utterance symbol.

    Unlike InterThreadEvent and SocketHasDataEvent, this event is
    currently used for asynchronous communication within the main thread.
    Its purpose is to invoke the modal correction box, while letting the 
    correction grammar's on_results method return immediately, so as to 
    allow speech input to the correction box (or other windows).

    **INSTANCE ATTRIBUTES**

    *wxEvtHandler evt_handler* -- wxWindow or wxEvtHandler to which to
    post the event.

    **CLASS ATTRIBUTES**

    *none*
    """

    def __init__(self, **args):
        self.deep_construct(ReformatSymbolEvent,
                            {},
                            args)
                            
    def notify(self, instance_name, utterance_number):
        """send the message, and return synchronously

        **INPUTS**

        *STR instance_name* -- unique name identifying the editor
        instance

        *INT utterance_number* -- the number assigned by
        ResMgr.interpret_dictation to the utterance to be corrected

        **OUTPUTS**

        *none*
        """
        debug.virtual('ReformatSymbolEvent.notify', self)
