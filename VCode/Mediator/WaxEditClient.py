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


import os, posixpath, re, select, socket
import SocketServer, string, sys, threading, time, whrandom 
import Object
import vc_globals


import AppStateGenEdit
import GenEdit

import debug
import messaging
import util
import Queue

import thread_communication_WX
import tcp_client
from tcp_threads import *
from wxPython.wx import *

from GenEditWX import *

debug.config_traces(status="on",
              active_traces={
#                             'get_mess':1,
#                             'send_mess': 1,
#                             'ClientEditor': 1
#                             'SourceBuffEdSim': 1
                             })

# debug.config_traces(status="on", active_traces='all')


def help():
    print """
Usage: python WaxEditClient.py -h

or

python WaxEditClient.py [OPTIONS]

where OPTIONS are 

[-m] [-i] [--host host] [--listen listen_port] [--talk talk_port]

runs an WaxEdit editor simulator as a TCP client to the mediator server

OPTIONS
-------

-h :

   print this help message.

-m :
   allow EdSim to have multiple buffers

-i :
   use client-side indentation

-c :
   add a command-line and log window to each frame, to assist in
   debugging

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

class DummyString:
    def __init__(self, value):
        self.value = value

class AttrValidator(wxPyValidator):
    """validator which merely copies the value of a text control to the
    attribute of a given object"""
    def __init__(self, obj = None):
        wxPyValidator.__init__(self)
        self.obj = obj
    def Clone(self):
        return AttrValidator(self.obj)
    def Validate(self, win):
        return 1
    def TransferToWindow(self):
        c = self.GetWindow()
        c.SetValue(self.obj.value)
        return 1
    def TransferFromWindow(self):
        c = self.GetWindow()
        self.obj.value = c.GetValue()
        return 1

class ConnectionSettingsDlg(wxDialog):
    """server connection dialog"""
    def __init__(self, parent, host, listen_string, talk_string):
        """
        **INPUTS**

        *DummyString host* -- the default host
        
        *DummyString listen_string, talk_string* -- string
        versions of the default (or most recent) listen_port and
        talk_port
        """

        wxDialog.__init__(self, parent, -1, "Mediator Server",
            wxDefaultPosition, (450, 350))
        vertical = wxBoxSizer(wxVERTICAL)
        vertical.SetMinSize((250, -1))
        host_row = wxBoxSizer(wxHORIZONTAL)
        host_row.Add(wxStaticText(self, -1, "Host name or IP: "), 0)
        host_text = wxTextCtrl(self, -1, host.value, 
            wxDefaultPosition, wxDefaultSize,
            validator = AttrValidator(host))
        host_row.Add(host_text, 1)
        vertical.AddSizer(host_row, 0, wxGROW)
        listen_row = wxBoxSizer(wxHORIZONTAL)
        listen_row.Add(wxStaticText(self, -1, "Listen port #: "), 0)
        listen_row.Add(wxTextCtrl(self, -1, str(listen_port), 
            wxDefaultPosition, wxDefaultSize,
            validator = AttrValidator(listen_string)), 1)
        vertical.AddSizer(listen_row, 0, wxGROW)
        talk_row = wxBoxSizer(wxHORIZONTAL)
        talk_row.Add(wxStaticText(self, -1, "talk port #: "), 0)
        talk_row.Add(wxTextCtrl(self, -1, str(talk_port), 
            wxDefaultPosition, wxDefaultSize,
            validator = AttrValidator(talk_string)), 1)
        vertical.AddSizer(talk_row, 0, wxGROW)
        button_row = wxBoxSizer(wxHORIZONTAL)
        okay_button = wxButton(self, wxID_OK, "OK")
        okay_button.SetDefault()
        button_row.Add(okay_button, 0)
        button_row.Add(wxButton(self, wxID_CANCEL, "Cancel"), 0)
        vertical.AddSizer(button_row, 0)
        self.SetAutoLayout(1)
        self.SetSizer(vertical)
        vertical.Fit(self)
        vertical.SetSizeHints(self)
        host_text.SetFocus()

        
        

class ClientFrameMixIn(Object.Object):
    """mix-in class which adds a Connection menu and related
    infrastructure to concrete subclasses of WaxFrameBasic (such as
    WaxFrame, and is designed to be used with WaxEditClient.

    **INSTANCE ATTRIBUTES**
    """
    def __init__(self, **args):
        """**NOTE:** some of the tasks which would normally be done 
        in __init__ if this were a subclass of WaxFrameBasic, rather than a 
        mix-in, are instead performed in finish_construction.  
        This is to ensure that those tasks are done after
        WaxFrameBasic's __init__ method is called.
        The common subclass of WaxFrameBasic and this mix-in must 
        call finish_construction after its deep_construct has called 
        WaxFrameBasic's __init__ method.
        """
        self.deep_construct(ClientFrameMixIn, 
                            {}, args)

    def finish_construction(self):
        """Finish constructing the client frame, by adding connection
        menus
        
        **NOTE:** this task would normally be done 
        in __init__ if this were a subclass of WaxFrameBasic, 
        rather than a mix-in.  Instead it is performed here,
        to ensure that it is done after
        WaxFrameBasic's __init__ method is called.
        The common subclass of WaxFrameBasic and this mix-in must 
        call finish_construction after its deep_construct has called 
        WaxFrameBasic's __init__ method.

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        connection_menu = wxMenu()
        ID_CONNECT = wxNewId()
        ID_CONNECT_TEST = wxNewId()
        ID_DISCONNECT = wxNewId()
        ID_HOST = wxNewId()
        connection_menu.Append(ID_CONNECT, "&Connect", 
            "Connect to the mediator server")
        connection_menu.Append(ID_CONNECT_TEST, "&Run regression tests", 
            "Connect to the mediator server for regression tests")
        connection_menu.Append(ID_DISCONNECT, "&Disconnect", 
            "Disconnect from the mediator server")
        connection_menu.Append(ID_HOST, "&Host and ports...", 
            "Change default host and ports")
        self.insert_menu(connection_menu, "&Connection", before = "Edit")
        self.update_connection_status()
        EVT_MENU(self, ID_CONNECT, self.on_connect)
        EVT_MENU(self, ID_CONNECT_TEST, self.on_connect_test)
        EVT_MENU(self, ID_DISCONNECT, self.on_disconnect)
        EVT_MENU(self, ID_HOST, self.on_host)

    def connected(self):
        """ checks whether we are connected to the mediator server
        
        **INPUTS**
        
        *none*
        
        **OUTPUTS **
        
        *BOOL* -- true if we are currently connected to  the mediator
        server
        """
        return self.owner.connected()

    def on_host(self, event):
        """prompt for new settings for host, listen and talk ports

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        if self.connected():
            return 0
        data = self.owner.connection_data()
        new_host = DummyString(data['host'])
        new_listen = DummyString(str(data['listen_port']))
        new_talk = DummyString(str(data['talk_port']))
        dlg = ConnectionSettingsDlg(self, new_host, new_listen, new_talk)
        success = dlg.ShowModal()
        dlg.Destroy()
        if success:
            i_listen = int(new_listen.value)
            i_talk = int(new_talk.value)
        else:
            return 0
        self.owner.set_connection_data(new_host.value, i_listen, i_talk)
    
    def connect(self, test_client = 0):
        """connect to the mediator server using current settings

        **INPUTS**

        *BOOL test_client* -- flag indicating whether we are connecting as
        a test_client

        **OUTPUTS**

        *BOOL* -- true if the connection was successfully established
        """
        if self.connected():
            return 0
        self.SetStatusText("Connecting...")
        success = self.owner.connect(test_client)
        self.update_connection_status()
        if success:
            self.SetStatusText("Connected to server")
            return 1
        self.SetStatusText("")
        dlg = wxMessageDialog(self, "Unable to connect to server",
            "Connection Error",
            wxICON_ERROR | wxOK)
        wxBell()
        dlg.ShowModal()
        dlg.Destroy()
        return 0

    def on_connect(self, event):
        """event handler for the Connect menu item

        **INPUTS**

        *wxCommandEvent event* -- the event indicating the menu item
        selected

        **OUTPUTS**

        *none*
        """
        self.connect()

    def on_connect_test(self, event):
        """event handler for the Connect menu item

        **INPUTS**

        *wxCommandEvent event* -- the event indicating the menu item
        selected

        **OUTPUTS**

        *none*
        """
        if self.owner.modified_buffers():
            msg = "Running regression tests will close all open\nbuffers without saving.  Are you sure?"
            proceed = wxMessageBox(msg, "Regression tests", 
                wxICON_EXCLAMATION | wxYES_NO | wxNO_DEFAULT, self)
            if proceed != wxYES:
                return
# mark any buffers created by the regression tests as scratch buffers
# not needing to be saved
        self.connect(test_client = 1)

    def on_disconnect(self, event):
        """event handler for the Connect menu item

        **INPUTS**

        *wxCommandEvent event* -- the event indicating the menu item
        selected

        **OUTPUTS**

        *none*
        """
        if not self.connected():
            return
        if self.owner.testing():
            msg = "We are still running regression tests.\nAre you sure you want to disconnect?"
            dlg = wxMessageDialog(self, msg, "Disconnect from Server", 
                wxYES_NO | wxNO_DEFAULT)
            answer = dlg.ShowModal()
            dlg.Destroy()
            if answer != wxID_YES:
                return
        self.owner.disconnect()
        self.update_connection_status()
    
    def update_connection_status(self):
        """updates the connection menu to reflect the current status

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        connected = self.connected()
        if not connected:
            self.SetStatusText("")
            self.set_instance_string("")
            self.update_title()
# if connecting or connected, message may vary, so don't change it here
        menu = self.get_menu_by_name("Connection")
        connect_item = self.find_item_by_label(menu, "Connect")
        connect_item.Enable(not connected)
        connect_test_item = self.find_item_by_label(menu, "Run regression tests")
        connect_test_item.Enable(not connected)
        disconnect_item = self.find_item_by_label(menu, "Disconnect")
        disconnect_item.Enable(connected)

class SimpleWaxClientFrame(SimpleWaxFrame, ClientFrameMixIn):
    """SimpleWaxFrame + client frame interface

    **INSTANCE ATTRIBUTES**

    *none*
    """
    def __init__(self, **args):
        self.deep_construct(SimpleWaxClientFrame, {}, args)
        self.finish_construction()

class WaxCmdClientFrame(WaxCmdFrame, ClientFrameMixIn):
    """WaxCmdFrame with command line and log + client frame interface

    **INSTANCE ATTRIBUTES**

    *none*
    """
    def __init__(self, **args):
        self.deep_construct(WaxCmdClientFrame, {}, args)
        self.finish_construction()

class ClientBase(GenEdit.ActivateEventMixIn, Object.OwnerObject):
    """Mix-in base class for WaxEdit clients, providing methods specific
    to a client editor

    **INSTANCE ATTRIBUTES**

    *WaxClientApp, wxApp app* -- the WaxClientApp object which owns this
    client and handles connections

    *STR app_name* -- name of the application
    """
    def __init__(self, app, app_name, **args):
        self.deep_construct(ClientBase, 
                            {'app': app,
                             'app_name': app_name
                            }, args)
        self.name_parent('app')

    def test_ending(self):
        """method by which the application can signal that we have
        finished regression testing.

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        for buff_name in self.open_buffers():
            self.mark_as_scratch(buff_name)

    def connection_data(self):
        """ returns current default connection data
        
        **INPUTS**
        
        *none*
        
        **OUTPUTS**
        
        *{STR:ANY}* -- a map containing entries for the host name or IP
        address, 'host', (STR), and the listen and talk ports, 
        'listen_port' and 'talk_port' (INT)
        """
        return self.app.connection_data()

    def set_connection_data(self, host = None, listen_port = None, 
        talk_port = None):
        """sets new connection data
        
        **INPUTS**

        *STR host* -- the host name or IP address

        *INT listen_port* -- the port on which the client will listen
        for messages from the mediator server, and reply to such
        messages
        
        *INT talk_port* -- the port on which the client will send
        messages to the mediator server, and receive replies
        
        **OUTPUTS**

        *none*
        """
        self.app.set_connection_data(host, listen_port, talk_port)

    def connected(self):
        """ checks whether we are connected to the mediator server
        
        **INPUTS**
        
        *none*
        
        **OUTPUTS **
        
        *BOOL* -- true if we are currently connected to  the mediator
        server
        """
        return self.app.connected()

    def testing(self):
        """tells whether we are connected to the mediator server and running
        regression tests

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL test_client* -- true if we are connected as
        a test_client
        """
        return self.app.testing()

    def connect(self, test_client = 0):
        """connect to the mediator server using current settings

        **INPUTS**

        *BOOL test_client* -- flag indicating whether we are connecting as
        a test_client

        **OUTPUTS**

        *BOOL* -- true if the connection was successfully established
        """
        return self.app.connect(test_client = test_client)

    def disconnect(self):
        """disconnect from the mediator server 

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        self.app.disconnect()

    def update_connection_status(self):
        """updates the connection menu to reflect the current status

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        for frame in self.frames.values():
            frame.update_connection_status()

class WaxClientBase(ClientBase):
    """Mix-in base class for WaxEdit clients using WaxFrame

    **INSTANCE ATTRIBUTES**

    *wxSize or (INT, INT) size* -- default size for frames
    *none*

    *BOOL cmd_line* -- include a command-line and log window in each
    frame?

    *{STR: ANY}* initial_cmd_space -- initial name space for user commands
    entered at the command line 
    """
    def __init__(self, frame_size = None, cmd_line = 0, 
        command_space = None, **args):
        self.deep_construct(WaxClientBase, 
                            {'frame_size': frame_size,
                             'cmd_line': cmd_line,
                             'initial_cmd_space': command_space}, args)
        if frame_size == None:
            if not cmd_line:
                self.frame_size = (600, 400)
            else:
                self.frame_size = (1000, 600)
#        if cmd_line:
#            self.initial_cmd_space['testing'] = 5

    def new_frame(self, buff_name, instance_string = ""):
        """creates a new frame of the appropriate concrete class
        open buffer and new window callbacks to the AppState interface

        **NOTE:** when adding a new frame with a buffer, you should call
        new_buffer first, followed by add_frame

        **INPUTS**

        *STR buff_name* -- the name of the initial buffer for the frame

        *STR instance_string* -- portion of the title string indicating 
        the name of this particular instance.

        **OUTPUTS**

        *GenEditFrame* -- the new frame 
        """
#        print 'i.s. is "%s"' % instance_string
        if self.cmd_line:
            return WaxCmdClientFrame(owner = self, app_name = self.app_name,
                ID = wxNewId(), size = self.frame_size, 
                init_buff_name = buff_name, 
                command_space = self.initial_cmd_space, 
                instance_string = instance_string)
        return SimpleWaxClientFrame(owner = self, app_name = self.app_name,
            ID = wxNewId(), size = self.frame_size, 
            init_buff_name = buff_name,
            instance_string = instance_string)

class WaxClientSingle(WaxClientBase, GenEdit.GenEditSingle):
    """WaxClient with a single frame

    **INSTANCE ATTRIBUTES**

    *none*
    """
    def remove_other_references(self):
        """additional cleanup to ensure that this object's references to
        its owned objects are the last remaining references

        **NOTE:** subclasses must call their parent class's 
        remove_other_references method, after performing their own duties.
        Also, a class inheriting from two OwnerObject classes MUST
        define remove_other_references and call both subclasses'
        versions

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
# subclasses must call their parent class's remove_other_references
# method, after performing their own duties
        GenEdit.GenEditSingle.remove_other_references(self)
        WaxClientBase.remove_other_references(self)


    def __init__(self, **args):
        self.deep_construct(WaxClientSingle, 
                            {
                            }, args)


class WaxClient(WaxClientBase, GenEdit.GenEditSimple):
    """WaxClient with multiple frames

    **INSTANCE ATTRIBUTES**

    *none*
    """
    def remove_other_references(self):
        """additional cleanup to ensure that this object's references to
        its owned objects are the last remaining references

        **NOTE:** subclasses must call their parent class's 
        remove_other_references method, after performing their own duties.
        Also, a class inheriting from two OwnerObject classes MUST
        define remove_other_references and call both subclasses'
        versions

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
# subclasses must call their parent class's remove_other_references
# method, after performing their own duties
        GenEdit.GenEditSimple.remove_other_references(self)
        WaxClientBase.remove_other_references(self)

    def __init__(self, **args):
        self.deep_construct(WaxClient, 
                            {
                            }, args)

class WaxClientAppBase(wxApp, Object.OwnerObject):
    """wxApp subclass for WaxEdit clients

    **INSTANCE ATTRIBUTES**

    *WaxClientBase GUI_editor* -- the WaxClientBase (or, rather, a 
    concrete subclass thereof, such as WaxClient or WaxClientSingle) 
    implementing the GenEdit editor for wxPython

    *AppStateGenEdit editor* -- the AppState wrapper interface to 
    GUI_editor

    *ClientConnection connection* -- client connection object used to
    connect to the meditator 

    *ClientEditor client* -- wrapper for a client editor, handling
    messaging and callbacks

    BOOL *client_indentation* -- if true, use the name
    WaxEditClientIndent when handshaking with the server, to ensure that
    the server will not override indentation on the server-side.

    *{STR:ANY} host_data* -- a map containing entries for the host name 
    or IP address, 'host', (STR), and the listen and talk ports, 
    'listen_port' and 'talk_port' (INT)

    *STR* host -- name or IP address of the host on which the
    mediator server is running.  Defaults to a server running
    locally.

    *INT* listen_port -- port number on which the server expects
    new listen connections.  Defaults to VC_LISTENER_PORT

    *INT* talk_port -- port number on which the server expects
    new talk connections.  Defaults to VC_TALKER_PORT

    *{STR:ANY} old_host_data* -- initial default host data, 
    or most recent working values: a map containing entries for the 
    host name or IP address, 'host', (STR), and the listen and talk ports, 
    'listen_port' and 'talk_port' (INT)

    *BOOL testing_flag* -- flag indicating that the current connection
    is running regression tests

    *BOOL cmd_line* -- include a command-line and log window in each
    frame?
    """
    def __init__(self, client_indentation = 0, 
            host = None, listen_port = None, 
            talk_port = None, cmd_line = 0, **args):
        self.deep_construct(WaxClientAppBase, 
                            {
                             'connection': tcp_client.ClientConnection(),
                             'host': host,
                             'listen_port': listen_port,
                             'talk_port': listen_port,
                             'old_host_data': {},
                             'client': None,
                             'editor': None,
                             'GUI_editor': None,
                             'client_indentation': client_indentation,
                             'cmd_line': cmd_line,
                             'testing_flag': 0
                            }, args, exclude_bases = {wxApp: 1})
        self.add_owned('GUI_editor')
        self.add_owned('editor')
        self.add_owned('client')
        if host == None:
            self.host = socket.gethostname()
        if listen_port == None:
            self.listen_port = tcp_client.VC_LISTENER_PORT
        if talk_port == None:
            self.talk_port = tcp_client.VC_TALKER_PORT
# store the same initial values in old_host_data
        self.backup_connection_data()
        wxApp.__init__(self, 0)
#        wxApp.__init__(self, 1, "appcrash")

    def OnInit(self):
        self.GUI_editor = self.create_editor()
        self.editor = AppStateGenEdit.AppStateGenEdit(self.GUI_editor)
        return 1

    def create_editor(self):
        """create the underlying editor
        
        **INPUTS**
        
        *none*
        
        **OUTPUTS**

        *GenEditFrames, WaxClientBase* -- the underlying WaxEdit 
        editor, which must be an instance of a concrete subclass 
        of WaxClientBase
        """
        debug.virtual('WaxClientApp.create_editor')

    def connection_data(self):
        """ returns current default connection data
        
        **INPUTS**
        
        *none*
        
        **OUTPUTS**
        
        *{STR:ANY}* -- a map containing entries for the host name or IP
        address, 'host', (STR), and the listen and talk ports, 
        'listen_port' and 'talk_port' (INT)
        """
        m = {}
        m['host'] = self.host
        m['listen_port'] = self.listen_port
        m['talk_port'] = self.talk_port
        return m

    def restore_connection_data(self):
        """restores old connection data
        
        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        self.host = self.old_host_data['host']
        self.listen_port = self.old_host_data['listen_port']
        self.talk_port = self.old_host_data['talk_port']

    def set_connection_data(self, host = None, listen_port = None, 
        talk_port = None):
        """sets new connection data
        
        **INPUTS**

        *STR host* -- the host name or IP address

        *INT listen_port* -- the port on which the client will listen
        for messages from the mediator server, and reply to such
        messages
        
        *INT talk_port* -- the port on which the client will send
        messages to the mediator server, and receive replies
        
        **OUTPUTS**

        *none*
        """
        if host != None:
            self.host = host
        if listen_port != None:
            self.listen_port = listen_port
        if talk_port != None:
            self.talk_port = talk_port

    def backup_connection_data(self):
        """copies connection data to old_host_data
        
        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        self.old_host_data['host'] = self.host
        self.old_host_data['listen_port'] = self.listen_port
        self.old_host_data['talk_port'] = self.talk_port

    def connected(self):
        """ checks whether we are connected to the mediator server
        
        **INPUTS**
        
        *none*
        
        **OUTPUTS**
        
        *BOOL* -- true if we are currently connected to the mediator
        server
        """
        return self.connection.is_connected()

    def testing(self):
        """tells whether we are connected to the mediator server and running
        regression tests

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL test_client* -- true if we are connected as
        a test_client
        """
        if self.connected():
            return self.testing_flag
        return 0

    def connect(self, test_client = 0):
        """connect to the mediator server using current settings

        **INPUTS**

        *BOOL test_client* -- flag indicating whether we are connecting as
        a test_client

        **OUTPUTS**

        *BOOL* -- true if the connection was successfully established
        """
        if self.connected():
            return 0
        app_name = 'WaxEdit'
        if self.client_indentation:
            app_name = 'WaxEditClientIndentation'
        event = thread_communication_WX.InterThreadEventWX(self,
            wxEVT_SOCKET_DATA) 
        messengers = self.connection.connect(app_name, event, host = self.host,
            listen_port = self.listen_port, talk_port = self.talk_port, 
            test_client = test_client)
#        print 'connected: ', self.connection.is_connected()
#        print messengers
        if messengers == None:
            self.restore_connection_data()
            return 0
# copy successful data to old data
        talk_msgr, listen_msgr = messengers
        self.backup_connection_data()
        self.client = \
            tcp_client.ClientEditorChangeSpec(editor = self.editor, 
                owner = self, ID = 'dummy', owns_editor = 0)
        self.client.connect(talk_msgr, listen_msgr)
        self.hook_data_event()
        self.testing_flag = test_client
        self.GUI_editor.update_connection_status()
        return 1

    def app_closing(self, ID, unexpected = 0):
        """method called by ClientEditor when it gets a message from
        AppState saying that it is closing
        """
        self.disconnect(AppState_initiated = 1)

    def mediator_closing(self, ID, unexpected = 0):
        """method called by editor when it gets a message from the
        mediator that it is closing (or if the connection is broken
        without warning"""
        self.disconnect(client_initiated = 0)

    def on_data(self, event):
        """event handler for data events
        """
        if self.connected():
            self.client.mediator_cmd()
        else:
            sys.stderr.write('ERROR: received data event while not connected\n')

    def disconnect(self, client_initiated = 1, AppState_initiated = 0):
        """disconnect from the mediator server 

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
#        print 'app told to disconnect'
        if self.connected():
#            print 'was connected'
            success = self.connection.disconnect()
            if success:
#                print 'success'
                if not AppState_initiated:
                    if client_initiated:
                        self.client.disconnect()
                    else:
                        self.client.disconnected()
                if self.testing_flag:
                    self.GUI_editor.test_ending()
                self.testing_flag = 0
                self.client = None
        self.GUI_editor.update_connection_status()

    def run(self):
        """starts the message loop.  Note: this function does not
        return until the GUI exits.

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        self.MainLoop()
        self.cleanup()

    def hook_data_event(self):
        """hook the wxEVT_SOCKET_DATA event up to our handler

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        EVT_MINE(self, wxEVT_SOCKET_DATA, self.on_data)

class WaxClientAppSingle(WaxClientAppBase):
    """concrete WaxClientApp subclass using a single WaxFrame 

    **INSTANCE ATTRIBUTES**

    *none*
    """
    def __init__(self, **args):
        self.deep_construct(WaxClientAppSingle, 
                            {
                            }, args)
    def create_editor(self):
        """create the underlying editor
        
        **INPUTS**
        
        *none*
        
        **OUTPUTS**

        *GenEditFrames, WaxClientBase* -- the underlying WaxEdit 
        editor, which must be an instance of a concrete subclass 
        of WaxClientBase
        """
        return WaxClientSingle(app = self, app_name = 'WaxEdit',
            curr_dir = vc_globals.test_data, cmd_line = self.cmd_line)

class WaxClientApp(WaxClientAppBase):
    """concrete WaxClientApp subclass using one or more WaxFrame 
    windows

    **INSTANCE ATTRIBUTES**

    *none*
    """
    def __init__(self, **args):
        self.deep_construct(WaxClientApp, 
                            {
                            }, args)
    def create_editor(self):
        """create the underlying editor
        
        **INPUTS**
        
        *none*
        
        **OUTPUTS**

        *GenEditFrames, WaxClientBase* -- the underlying WaxEdit 
        editor, which must be an instance of a concrete subclass 
        of WaxClientBase
        """
        return WaxClient(app = self, app_name = 'WaxEdit',
            curr_dir = vc_globals.test_data, cmd_line = self.cmd_line)

def run(multiple = 0, client_indentation = 0,
        host = None, listen_port = None, talk_port = None, cmd_line = 0):
    if multiple:
        app = WaxClientApp(client_indentation = client_indentation, 
            host = host, listen_port = listen_port, talk_port = talk_port, 
            cmd_line = cmd_line)
    else:
        app = WaxClientAppSingle(client_indentation = client_indentation, 
            host = host, listen_port = listen_port, talk_port = talk_port,
            cmd_line = cmd_line)
    app.run()



if __name__ == '__main__':
    opts, args = util.gopt(['h', None, 'm', None, 'p', None,
                            'i', None, 'c', None,
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
        cmd_line = 0
        if opts['c']:
            cmd_line = 1
        run(multiple, client_indentation, host, 
            listen_port, talk_port, cmd_line = cmd_line)


# defaults for vim - otherwise ignore
# vim:sw=4
