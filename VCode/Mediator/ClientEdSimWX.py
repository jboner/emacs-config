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

"""test implementation of a VoiceCode TCP/IP client using EdSim as its
editor, but with a wxPython message loop.
"""


import os, posixpath, pythoncom, re, select, socket
import SocketServer, string, sys, threading, time, whrandom, win32event

import AppState

import debug
import EdSim
import messaging, Object
import util
import Queue

import thread_communication_WX
import tcp_client
from tcp_threads import *
from wxPython.wx import *

debug.config_traces(status="on",
              active_traces={
#                             'get_mess':1,
#                             'send_mess': 1,
#                             'SourceBuffEdSim': 1
                             })

#debug.config_traces(status="on", active_traces='all')


def help():
    print """
Usage: python ClientEdSimWX.py -h

or

python ClientEdSimWX.py [OPTIONS]

where OPTIONS are 

[-m] [-i] [--host host] [--listen listen_port] [--talk talk_port]

runs an EdSim editor simulator as a TCP client to the mediator server
using a simple wxPython GUI

OPTIONS
-------

-h :

   print this help message.

-m :
   allow EdSim to have multiple buffers

-i :
   use client-side indentation

--host host:
  specify the host name or IP address (Defaults to the local host)

--talk talk_port:
  specify the port number to use for the talk connection
    
--listen listen_port:
  specify the port number to use for the listen connection
    """

def EVT_MINE(evt_handler, evt_type, func):
    evt_handler.Connect(-1, -1, evt_type, func)

# create a unique event type
wxEVT_SOCKET_DATA = wxNewEventType()

class ClientEdSimPane(wxPanel, Object.OwnerObject):
    """wxPanel for client EdSim

    **INSTANCE ATTRIBUTES**

    *ClientEdSimFrame parent* -- reference to the parent frame

    *STR client_name* -- name of the client editor, to pass to the
    ClientConnection, which in turn passes it to the mediator server

    *ClientConnection connection* -- the object representing the
    connection to the server

    *wxButton connect_button* -- connect/disconnect button

    *BOOL exiting* -- a flag indicating that we are exiting, used to
    short-circuit certain event handlers which would cause exceptions
    or crashes if they were processed while the application was in the
    middle of exiting
    """

    def __init__(self, parent, ID, client_name, host = None, 
            listen_port = None, talk_port = None, **args):
        self.deep_construct(ClientEdSimPane,
                            {'parent': parent,
                             'client_name': client_name,
                             'connection': tcp_client.ClientConnection(),
                             'connect_button': None,
                             'host': host,
                             'listen_port': listen_port,
                             'talk_port': talk_port,
#                             'text': None,
                             'exiting': 0
                            }, args, exclude_bases = {wxPanel:1}) 
        wxPanel.__init__(self, parent, ID, wxDefaultPosition, wxDefaultSize)
        self.name_parent('parent')

        vbox = wxBoxSizer(wxVERTICAL)
        ID_CONNECT_DISCONNECT = wxNewId()
        ID_TEXT = wxNewId()
        self.connect_button = wxButton(self, ID_CONNECT_DISCONNECT, "Connect", 
            wxDefaultPosition, wxDefaultSize)
#        self.text = wxStaticText(self, ID_TEXT, "", wxDefaultPosition, 
#            wxDefaultSize)
        vbox.Add(self.connect_button, 0) # don't stretch vertically (or horizontally)
#        vbox.Add(self.text, 1, wxEXPAND) # stretch in both directions


        EVT_BUTTON(self, ID_CONNECT_DISCONNECT, self.on_toggle_connection)
        self.SetAutoLayout(1)
        self.SetSizer(vbox)
        vbox.Fit(self)
        vbox.SetSizeHints(self)

    def on_toggle_connection(self, event):
        """event handler for connect/disconnect button
        """
        event.Skip()
# do button GUI toggle regardless
        if self.exiting:
# ignore if we're already exiting
            return
        if self.connection.is_connected():
            self.disconnect()
        else:
            self.connect()

    def disconnected(self):
        """method by which the frame notifies us that the mediator
        has disconnected

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        self.connection.disconnect()
        self.update_button()

    def update_button(self):
        """update text of connect/disconnect button"""
        if self.connection.is_connected():
            self.connect_button.SetLabel("Disconnect")
        else:
            self.connect_button.SetLabel("Connect")
        self.connect_button.Enable(1)

    def disconnect(self):
        """disconnect from server"""
        if not self.connection.is_connected():
            return 1
        dlg = wxMessageDialog(self, "Disconnect: Are you sure?",
            "Disconnect from Server", wxYES_NO | wxNO_DEFAULT)
        answer = dlg.ShowModal()
        dlg.Destroy()
        if answer == wxID_YES:
            self.parent.disconnect_editor()
            self.connection.disconnect()
            self.connect_button.SetLabel("Connect")
            return 1
        return 0

    def connect(self):
        """connect to server"""
        self.connect_button.Enable(0)
# hook that type to the app's on_data method
        self.parent.hook_data_event()
# unlike server, only one event per client, so we don't need to use
# SocketHasDataWX, which is designed to be constructed with a socket_ID
        event = thread_communication_WX.InterThreadEventWX(self.parent.app,
            wxEVT_SOCKET_DATA) 
        try:
            messengers = self.connection.connect(self.client_name, event,
                test_client = 1, host = self.host, listen_port =
                self.listen_port, talk_port = self.talk_port)
        except socket.error:
            messengers = None
        if messengers == None:
            dlg = wxMessageDialog(self, "Unable to connect to server",
                "Connection Error",
                wxICON_ERROR | wxOK)
            dlg.ShowModal()
            dlg.Destroy()
            self.connect_button.Enable(1)
            return
        self.connect_button.Enable(1)
        self.connect_button.SetLabel("Disconnect")
        talk, listen = messengers
        self.parent.editor_connected(talk, listen)

    def remove_other_references(self):
        self.exiting = 1
        self.connect_button.Destroy()
        Object.OwnerObject.remove_other_references(self)
#        self.text.Destroy()

class ClientEdSimFrame(wxFrame, Object.OwnerObject):
    """wxFrame for client EdSim

    **INSTANCE ATTRIBUTES**

    *ClientEdSimWX app* -- reference to the parent application

    *ClientEdSimPane pane* -- the child panel

    *BOOL exiting* -- a flag indicating that we are exiting, used to
    short-circuit certain event handlers which would cause exceptions
    or crashes if they were processed while the application was in the
    middle of exiting

    *STR* host -- name or IP address of the host on which the
    mediator server is running.  Defaults to a server running
    locally.

    *INT* listen_port -- port number on which the server expects
    new listen connections.  Defaults to VC_LISTENER_PORT

    *INT* talk_port -- port number on which the server expects
    new talk connections.  Defaults to VC_TALKER_PORT
    """

    def remove_other_references(self):
        self.exiting = 1
        Object.OwnerObject.remove_other_references(self)


    def disconnected(self):
        """method by which the application notifies us that the mediator
        has disconnected

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        self.pane.disconnected()

    def __init__(self, app, parent, ID, title, client_name, host = None,
            listen_port = None, talk_port = None, **args):
        self.deep_construct(ClientEdSimFrame,
                            {'app': app,
                             'pane': None,
                             'exiting': 0
                            }, args, exclude_bases = {wxFrame:1}) 
        wxFrame.__init__(self, parent, ID, title, wxDefaultPosition,
            wxSize(300, 80))
        self.name_parent('app')
        self.add_owned('pane')

        self.pane = ClientEdSimPane(self, -1, client_name, host = host,
            listen_port = listen_port, talk_port = talk_port) 
        EVT_CLOSE(self, self.on_close)        

    def on_close(self, event):
        if not self.exiting:
            should_close = self.pane.disconnect()
            if should_close:
                self.cleanup()
                event.Skip()        

    def disconnect_editor(self):
        """method by which the panel notifies us that a disconnected
        message should be sent to the editor
        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        self.app.disconnect_editor()

    def editor_connected(self, talk, listen):
        """method by which the frame notifies us that we should call the
        connected method of the editor
        
        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        self.app.editor_connected(talk, listen)

    def hook_data_event(self):
        """method by which the pane tells us to hook the 
        wxEVT_SOCKET_DATA event

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        self.app.hook_data_event()

class ClientEdSimWX(wxApp, Object.OwnerObject):
    """class for running the EdSim editor simulator as a TCP client, but
    using the event mechanism of wxPython for inter-thread communication

    **INSTANCE ATTRIBUTES**

    ClientEditorChangeSpec *editor* -- the client wrapper for the EdSim 
    instance

    BOOL *client_indentation* -- if true, use the name
    EdSimClientIndent when handshaking with the server, to ensure that
    the server will not override indentation on the server-side.

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, multiple = 0, client_indentation = 0,
        host = None, listen_port = None, talk_port = None, 
        **args):
        """
        **INPUTS**

        *BOOL multiple* -- should this EdSim allow for multiple open
        buffers?

        BOOL *client_indentation* -- if true, use the name
        EdSimClientIndent when handshaking with the server, to ensure that
        the server will not override indentation on the server-side.

        *STR* host -- name or IP address of the host on which the
        mediator server is running.  Defaults to a server running
        locally.

        *INT* listen_port -- port number on which the server expects
        new listen connections.  Defaults to VC_LISTENER_PORT

        *INT* talk_port -- port number on which the server expects
        new talk connections.  Defaults to VC_TALKER_PORT
        """
        self.deep_construct(ClientEdSimWX,
                            {
                             'client_indentation': client_indentation,
                             'host': host,
                             'listen_port': listen_port,
                             'talk_port': talk_port,
                             'editor': None
                            }, args, 
                            exclude_bases = {wxApp: 1})
        dummy = "ceswx.err"
        dummy = 0
        wxApp.__init__(self, dummy)

        underlying_editor = EdSim.EdSim(multiple = multiple)
        self.editor = tcp_client.ClientEditorChangeSpec(editor = underlying_editor, 
            owner = self, ID = 'dummy', owns_editor = 1)

    def app_closing(self, ID, unexpected = 0):
        """method called by ClientEditor when it gets a message from
        AppState saying that it is closing
        """
# ClientEditor expects this to be defined, but EdSim doesn't ever
# generate a close_app_cbk, so we don't ever expect to receive this.
        self.editor.disconnected()
        self.frame.disconnected()


    def mediator_closing(self, ID, unexpected = 0):
        """method called by editor when it gets a message from the
        mediator that it is closing (or if the connection is broken
        without warning"""
        self.editor.disconnected()
        self.frame.disconnected()

    def OnInit(self):
        client_name = 'EdSim'
        if self.client_indentation:
            client_name = 'EdSimClientIndent'
        frame = ClientEdSimFrame(self, NULL, -1, "ClientEdSim", client_name, 
            host = self.host, listen_port = self.listen_port, 
            talk_port = self.talk_port)
        frame.Show(true)
#        frame.pane.initial_show()
        self.SetTopWindow(frame)
        self.frame = frame
        self.add_owned('frame')
        return true

    def on_data(self, event):
        """event handler for data events
        """
        self.editor.mediator_cmd()

    def run(self):
        self.MainLoop()

    def disconnect_editor(self):
        """method by which the frame notifies us that a disconnected
        message should be sent to the editor

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        self.editor.disconnected()

    def editor_connected(self, talk, listen):
        """method by which the frame notifies us that we should call the
        connected method of the editor
        
        **INPUTS**

        *Messenger talk* -- the Messenger for the talk connection
        
        *Messenger listen* -- the Messenger for the listen connection

        **OUTPUTS**

        *none*
        """
        self.editor.connect(talk, listen)

    def hook_data_event(self):
        """method by which the frame tells us to hook the 
        wxEVT_SOCKET_DATA event

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        EVT_MINE(self, wxEVT_SOCKET_DATA, 
            self.on_data)

        


def run(multiple = 0, client_indentation = 0,
        host = None, listen_port = None, talk_port = None):
    app = ClientEdSimWX(multiple = multiple, 
        client_indentation = client_indentation, host = host,
        listen_port = listen_port, talk_port = talk_port)
    app.run()



if __name__ == '__main__':
    opts, args = util.gopt(['h', None, 'm', None, 'p', None,
                            'i', None,
                            'host=', None,
                            'talk=', None, 'listen=', None])
    if opts['h']:
        help()
    else:
#        print sys.modules
        host = opts['host']
        listen_port = opts['listen']
        talk_port = opts['talk']
        multiple = 0
        if opts['m']:
            multiple = 1
        client_indentation = 0
        if opts['i']:
            client_indentation = 1
        run(multiple, client_indentation, host, 
            listen_port, talk_port)

