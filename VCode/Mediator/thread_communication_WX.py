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

from thread_communication import *
from wxPython.wx import *

# create a unique event types
wxEVT_SOCKET_DATA = wxNewEventType()
wxEVT_CORRECT_UTTERANCE = wxNewEventType()
wxEVT_CORRECT_RECENT = wxNewEventType()
wxEVT_CORRECT_NTH_RECENT = wxNewEventType()
wxEVT_REFORMAT_RECENT = wxNewEventType()

class GenericEventWX(wxPyEvent):
    def __init__(self, evt_type):
        wxPyEvent.__init__(self)
        self.SetEventType(evt_type)

class SocketDataEventWX(GenericEventWX):
    def __init__(self, socket_ID):
        GenericEventWX.__init__(self, evt_type = wxEVT_SOCKET_DATA)
        self.socket_ID = socket_ID

class UtteranceCorrectionEventWX(GenericEventWX):
    def __init__(self, instance_name, utterance_number):
        GenericEventWX.__init__(self, evt_type = wxEVT_CORRECT_UTTERANCE)
        self.instance_name = instance_name
        self.utterance_number = utterance_number

class RecentCorrectionEventWX(GenericEventWX):
    def __init__(self, instance_name):
        GenericEventWX.__init__(self, evt_type = wxEVT_CORRECT_RECENT)
        self.instance_name = instance_name
        
class RecentReformattingEventWX(GenericEventWX):
    def __init__(self, instance_name):
        GenericEventWX.__init__(self, evt_type = wxEVT_REFORMAT_RECENT)
        self.instance_name = instance_name

class CorrectNthRecentEventWX(GenericEventWX):
    def __init__(self, recent_chosen):
        GenericEventWX.__init__(self, evt_type = wxEVT_CORRECT_NTH_RECENT)
        self.recent_chosen = recent_chosen



class InterThreadEventWX(InterThreadEvent):
    """implementation of InterThreadEvent using the wxPython custom
    events module.

    **INSTANCE ATTRIBUTES**

    *wxEvtHandler evt_handler* -- wxWindow or wxEvtHandler to which to
    post the event.

    *WXTYPE evt_type* -- type/ID of the wxWindow event

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, evt_handler, evt_type, **args):
        """
        **INPUTS**

        *wxEvtHandler evt_handler* -- wxWindow or wxEvtHandler to which to
        post the event.
        """
        self.deep_construct(InterThreadEventWX,
                            {'evt_handler': evt_handler,
                             'evt_type': evt_type},
                            args)
    def notify(self):
        """send the message, and return synchronously

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        event = GenericEventWX(self.evt_type)
        wxPostEvent(self.evt_handler, event)

class SocketHasDataWX(SocketHasDataEvent):
    """implementation of SocketHasDataEvent using wxPython events.
    
    **INSTANCE ATTRIBUTES**

    *wxEvtHandler evt_handler* -- wxWindow or wxEvtHandler to which to
    post the event.

    *STR socket_ID* -- the unique ID of the socket

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, evt_handler, socket_ID, **args):
        """
        **INPUTS**

        *wxEvtHandler evt_handler* -- wxWindow or wxEvtHandler to which to
        post the event.

        *STR socket_ID* -- the unique ID of the socket
        """
        self.deep_construct(SocketHasDataWX,
                            {'evt_handler': evt_handler,
                             'socket_ID': socket_ID},
                            args)
    def notify(self):
        """send the message, and return synchronously

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        event = SocketDataEventWX(self.socket_ID)
        wxPostEvent(self.evt_handler, event)


class CorrectUtteranceEventWX(CorrectUtteranceEvent):
    """implementation of CorrectUtteranceEvent using custom wxPython
    events

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
    def __init__(self, evt_handler, **args):
        """
        **INPUTS**

        *wxEvtHandler evt_handler* -- wxWindow or wxEvtHandler to which to
        post the event.
        """
        self.deep_construct(CorrectUtteranceEventWX,
                            {'evt_handler': evt_handler},
                            args)

    def notify(self, instance_name):
        ##QH, just trying:
        debug.trace('CorrectThat.notify', 'invoked')
        event = CorrectNthRecentEventWX(instance_name)
##        event = UtteranceCorrectionEventWX(instance_name)
        wxPostEvent(self.evt_handler, event)
                                                        
class CorrectRecentEventWX(CorrectRecentEvent):
    """implementation of CorrectRecentEvent using custom wxPython
    events

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
    def __init__(self, evt_handler, **args):
        """
        **INPUTS**

        *wxEvtHandler evt_handler* -- wxWindow or wxEvtHandler to which to
        post the event.
        """
        self.deep_construct(CorrectRecentEventWX,
                            {'evt_handler': evt_handler},
                            args)

    def notify(self, instance_name):
        """send the message, and return synchronously

        **INPUTS**

        *STR instance_name* -- unique name identifying the editor
        instance

        **OUTPUTS**

        *none*
        """
        event = RecentCorrectionEventWX(instance_name)
        wxPostEvent(self.evt_handler, event)

class CorrectNthEventWX(CorrectNthEvent):
    """implementation of CorrectNEvent using custom wxPython
    events

    Unlike InterThreadEvent and SocketHasDataEvent, this event is
    currently used for asynchronous communication within the main thread.
    Its purpose is to invoke the modal correction box, while letting the 
    correct_n grammar's on_results method return immediately, so as to 
    allow speech input to the correction box (or other windows).

    Particular implementations may wxPython custom
    events or other means to communicate with the main thread.

    **INSTANCE ATTRIBUTES**

    *wxEvtHandler evt_handler* -- event handler (usually a wxDialog subclass)
    to which to post the event

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, evt_handler, **args):
        """
        **INPUTS**

        *wxEvtHandler evt_handler* -- event handler (usually a wxDialog 
        subclass) to which to post the event

        *WXTYPE evt_type* -- type/ID of the wxWindow event
        """
        self.deep_construct(CorrectNthEventWX,
                            {'evt_handler': evt_handler},
                            args)

    def notify(self, recent_chosen):
        """send the message, and return synchronously

        **INPUTS**

        *INT recent_chosen* -- the number of the utterance in the list
        of recent utterances (e.g. 5 if the user says "Correct 5")

        **OUTPUTS**

        *none*
        """
        event = CorrectNthRecentEventWX(recent_chosen)
        wxPostEvent(self.evt_handler, event)

class ReformatSymbolEventWX(ReformatSymbolEvent):
    """implementation of ReformatSymbolEvent using custom wxPython
    events

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
    
    def __init__(self, evt_handler, **args):
        """
        **INPUTS**

        *wxEvtHandler evt_handler* -- wxWindow or wxEvtHandler to which to
        post the event.
        """
        self.deep_construct(ReformatSymbolEventWX,
                            {'evt_handler': evt_handler},
                            args)


    def notify(self, instance_name):
        """send the message, and return synchronously

        **INPUTS**

        *STR instance_name* -- unique name identifying the editor
        instance

        **OUTPUTS**

        *none*
        """
        debug.trace('ReformatSymbolEventWX.notify', 'invoked')
        event = RecentReformattingEventWX(instance_name)
        wxPostEvent(self.evt_handler, event)
