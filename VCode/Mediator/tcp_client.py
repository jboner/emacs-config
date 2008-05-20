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

"""classes for creating VoiceCode TCP/IP client as a counterpart to 
AppStateMessaging and communicating with it via the ServerMainThread classes in tcp_server.py
"""

import vc_globals

import os, posixpath, re, select, socket
import SocketServer, string, sys, threading, time, whrandom

import AppState

import debug
import EdSim
import messaging, Object
import util
import Queue

import thread_communication
from tcp_threads import *



# Uncomment this and add some entries to active_traces if you want to 
# activate some traces.
#debug.config_traces(status="on", active_traces={'get_mess':1, 'send_mess': 1})
#debug.config_traces(status="on", active_traces = 'all')
#debug.config_traces(status="on", active_traces = {'sr_interface':1},
#allow_trace_id_substrings = 1)

#debug.config_traces(status="on", active_traces = {'on_change':1},
#    allow_trace_id_substrings = 1)

# Uncomment this and add some entries to active_traces if you want to 
# activate some traces.
debug.config_traces(status="on",
              active_traces={
#                              'ClientEditor': 1,
#                             'get_mess':1,
#                             'send_mess': 1
#                             'SourceBuffEdSim': 1
                             })

#debug.config_traces(status="on", active_traces='all')


#
# Port numbers for the communication link
#
VC_LISTENER_PORT = 45770
VC_TALKER_PORT = 45771

class ClientConnection(Object.Object):
    """class for connecting to the TCP mediator server and listening for 
    incoming messages.

    connect will create a ListenAndQueueMsgsThread to listen 
    for data on the talk_sock (the mediator server talks on the talk_sock, 
    while we listen) and queue complete messages.  
    The LAQM Thread also notifies the main thread of a pending message using a
    SocketHasDataEvent.  

    **INSTANCE ATTRIBUTES**

    ListenAndQueueMsgsThread *data_thread* -- 
    thread which polls for data from the listen messenger

    BOOL *connecting* -- flag indicating that the server is in the
    process of connecting to the mediator

    BOOL *connected* -- flag indicating that the server is 
    connected to the mediator

    STR *ID* -- unique ID assigned to this connection

    Event *client_quitting* -- threading.Event 
    used to signal to the data thread that the connection is 
    ending, or the client is quitting
 

    **CLASS ATTRIBUTES**
    
    *none* -- 

    """
    
    def __init__(self, **args_super):
        self.deep_construct(ClientConnection, 
                            {
                             'data_thread': None,
                             'connecting': 0,
                             'connected': 0,
                             'ID': None,
                             'client_quitting': None
                             }, 
                            args_super)

    def new_data_thread_given_event(self, listen_sock, data_event):
        """creates a new ListenAndQueueMsgsThread to monitor the
        listen_sock
        
        **INPUTS**

        SocketHasDataEvent *data_event* -- the SocketHasDataEvent event
        to pass to the new thread

        STR *ID* -- The unique ID of the listen socket
        
        socket *listen_sock* -- The listen socket
        
        **OUTPUTS**
        
        [ListenAndQueueMsgsThread] -- the new threading.Thread object

        ..[ListenAndQueueMsgsThread] 
        file:///./tcp_server.ListenAndQueueMsgsThread.html"""        
        queue = Queue.Queue()
        broken_connection = ('broken_connection', {})
        self.client_quitting = threading.Event()
        sleeper = messaging.LightSleeper(self.client_quitting)
        a_msgr = messaging.messenger_factory(listen_sock, sleep = 0.05,
            sleeper = sleeper)
        thread = ListenAndQueueMsgsThread(a_msgr, queue, data_event,
           self.client_quitting, broken_connection)
        return thread

    def is_connected(self):
        """tells whether the client is currently connected or connecting
        to the mediator

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- true if the  client is currently connected (or
        connecting) to a mediator
        """
        return self.connecting or self.connected

    def open_vc_listener_conn(self, app_name, host, listen_port,
        test_client = 0):
        """Connects to VoiceCode on the listen port

        **INPUTS**

        *STR* app_name -- name of the application to give to the
        mediator

        *STR* host -- name or IP address of the host on which the
        mediator server is running.  Defaults to a server running
        locally.

        *INT* listen_port -- port number on which the server expects
        new listen connections

        *BOOL* test_client -- true if this connection is for a client
        expecting to run regression tests

        **OUTPUTS**

        *Messenger* -- a Messenger for the vc_listener connection, or 
        None if the connection was not made successfully
        """

        a_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        a_socket.connect((host, listen_port))
        
        #
        # Create a messenger
        #
        packager = messaging.MessPackager_FixedLenSeq()
        transporter = messaging.MessTransporter_Socket(sock=a_socket)
        encoder = messaging.MessEncoderWDDX()
        vc_listen_msgr = messaging.MessengerBasic(packager=packager, 
            transporter=transporter, encoder=encoder)

        debug.trace('ClientConnection.open_vc_listener_conn',
              'sending name of editor')
        
        #
        # Send name of editor
        #
        vc_listen_msgr.get_mess(expect=['send_app_name'])
        vc_listen_msgr.send_mess('app_name', {'value': app_name})


        debug.trace('ClientConnection.open_vc_listener_conn',
              'getting ID')
        
        #
        # Get unique identifier from VoiceCode
        #
        msg = vc_listen_msgr.get_mess(expect=['your_id_is'])
        self.ID = msg[1]['value']
        vc_listen_msgr.send_mess('ok')
        
        # indicate whether this is a test client expecting to run 
        # regression tests

        debug.trace('ClientConnection.open_vc_listener_conn',
              'test client query')

        msg = vc_listen_msgr.get_mess(expect=['test_client_query'])
        vc_listen_msgr.send_mess('test_client_query_resp', 
            {'value': test_client})

        debug.trace('ClientConnection.open_vc_listener_conn',
              'done')

        return vc_listen_msgr

    def open_vc_talker_conn(self, host, talk_port):
        """Connects to VoiceCode on the talk port
        
        **INPUTS**

        *STR* host -- name or IP address of the host on which the
        mediator server is running.  Defaults to a server running
        locally.

        *INT* talk_port -- port number on which the server expects
        new talk connections

        **OUTPUTS**

        *socket* -- socket for the talk connection, or None if the 
        connection was not made successfully
        """

        debug.trace('ClientConnection.open_vc_talker_conn', 'started')
        
        #
        # Open the socket
        #
        a_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        a_socket.connect((host, talk_port))

        debug.trace('ClientConnection.open_vc_talker_conn', 'socket opened')
        
        #
        # Create a temporary messenger
        #
        packager = messaging.MessPackager_FixedLenSeq()
        transporter = messaging.MessTransporter_Socket(sock=a_socket)
        encoder = messaging.MessEncoderWDDX()
        vc_talk_msgr = messaging.MessengerBasic(packager=packager, transporter=transporter, encoder=encoder)
        

        debug.trace('ClientConnection.open_vc_talker_conn', 'sending ID')
        
        #
        # Send the connection pair ID to the remote server
        #
        vc_talk_msgr.get_mess(expect=['send_id'])
        vc_talk_msgr.send_mess('my_id_is', {'value': self.ID})
        
        debug.trace('test_TCP_server.open_vc_talker_conn', 'done')
        return a_socket

    def connect(self, app_name, data_event,
            host = None, 
            listen_port = None,
            talk_port = None, test_client = 0):
        """connect to the mediator

        **INPUTS**

        *STR* app_name -- name of the application to give to the
        mediator

        SocketHasDataEvent *data_event* -- the SocketHasDataEvent event
        to pass to the new data thread so it can notify the main thread
        when there is a command waiting in the queue.  Unlike the
        server, there is only one connection and one data socket per client, 
        so we don't need to give the socket a unique ID and create a 
        data_event based on that ID.

        *STR* host -- name or IP address of the host on which the
        mediator server is running.  Defaults to a server running
        locally.

        *INT* listen_port -- port number on which the server expects
        new listen connections.  Defaults to VC_LISTENER_PORT

        *INT* talk_port -- port number on which the server expects
        new talk connections.  Defaults to VC_TALKER_PORT

        *BOOL* test_client -- true if this connection is for a client
        expecting to run regression tests

        **OUTPUTS**

        *(Messenger, MixedMessenger)* -- a tuple containing the Messenger 
        for the talk connection and the MixedMessenger for the listen
        connection, or None if the connection was not made successfully
        """
        if self.is_connected():
            return None
        self.connecting = 1
        if listen_port == None:
            listen_port = VC_LISTENER_PORT
        if talk_port == None:
            talk_port = VC_TALKER_PORT

        if host == None:
            host = socket.gethostname()
        # vc listener means the VoiceCode mediator is listening -- we're
        # talking
        try:
            talk_msgr = self.open_vc_listener_conn(app_name, host, listen_port, 
                test_client = test_client)
        except socket.error:
            talk_msgr = None
        if talk_msgr == None:
            self.connecting = 0
            return None
        # vc talker means the VoiceCode mediator is talking -- we're
        # listening
        try:
            listen_sock = self.open_vc_talker_conn(host, talk_port)
        except socket.error:
            talk_msgr = None
        if listen_sock == None:
            self.connecting = 0
            return None

        self.connecting = 0
        self.connected = 1

        listen_msgr = self.listen(listen_sock, data_event)
        return talk_msgr, listen_msgr

    def disconnect(self):
        """disconnect from the mediator.  Note that ClientEditor has the
        messengers, so it will have to be in charge of 
        notifying the mediator that we are disconnecting 
        (if it was client-initiated).  It should do so after this call.
        
        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- true if we disconnected successfully
        """
        if not self.is_connected():
            return 0
        self.connected = 0
        self.connecting = 0
        self.client_quitting.set()
        self.client_quitting = None
        return 1

    def listen(self, listen_sock, data_event):
        """creates and starts a data thread on the listen_sock
        
        **INPUTS**
        
        socket *listen_sock* -- The listen socket

        **OUTPUTS**
        
        *MixedMessenger* -- a MixedMessenger for the listen socket, which 
        sends messages via an underlying MessengerBasic, but receives them 
        from a Queue.
        """
        
        data_thread = self.new_data_thread_given_event(listen_sock,
            data_event)
        self.data_thread = data_thread
        messages = data_thread.message_queue()
        data_thread.setDaemon(1)
        data_thread.start()

        listen_response_msgr = messaging.messenger_factory(listen_sock, 
            sleep = 0.05)        
        listen_msgr = messaging.MixedMessenger(listen_response_msgr, messages)
 
        return listen_msgr

class ClientEditor(Object.OwnerObject, AppState.AppCbkHandler):
    """abstract base class for handling messages to and from the client editor

    **INSTANCE ATTRIBUTES**

    *BOOL* connected -- flag indicating whether we are connected to the
    mediator or not

    Messenger *talk_msgr* -- Messenger for sending commands to the mediator

    MixedMessenger *listen_msgr* -- MixedMessenger for receiving commands from 
    the mediator without blocking

    AppState *editor* -- the AppState interface to the underlying editor

    STR *editor_name* -- the string which the editor uses to identify
    itself to the ClientEditor

    *{STR:FCT}* msg_map -- map from message names to (unbound) methods
    taking *(self, {arg:val})* to handle that message

    *[STR] expect* -- list of commands expected from the mediator

    *BOOL ignore_callbacks* -- flag to indicate that callbacks are
    triggered by a mediator-initiated message and should be ignored

    *[{STR:ANY}] awaiting_response* -- list of responses to a
    mediator-initiated change, or None if we are not in the middle of a
    mediator-initiated change

    *owner* -- the owner of the ClientEditor, which should be notified
    if the mediator sends a 'mediator_closing' or 'terminating' message,
    or if the ClientEditor receives a close_app_cbk from AppState

    *STR ID* -- unique ID of this ClientEditor, so that it can identify
    itself during callbacks to its owner

    *BOOL mediator_closing* -- flag indicating that the ClientEditor has
    received a mediator_closing message, and shouldn't be surprised if
    it receives a broken_connection message.

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, editor, owner, ID, owns_editor = 0, **args):
        """if owns_editor is true, ClientEditor.cleanup will clean up
        the editor AppState.  If not, ClientEditor's owner is
        responsible for doing so.
        """
        self.deep_construct(ClientEditor,
                            {
                             'connected': 0,
                             'talk_msgr': None,
                             'listen_msgr': None,
                             'editor': editor,
                             'editor_name': None,
                             'owner': owner,
                             'ID': ID,
                             'mediator_closing': 0,
                             'msg_map': {},
                             'expect': [],
                             'ignore_callbacks': 0,
                             'awaiting_response': None,
                            },
                            args)
        if owns_editor:
            self.add_owned('editor')
        self.name_parent('owner')
        self.expect=['recog_begin', 'recog_end', 
            'get_pos_selection',
            'confirm_buffer_exists', 'list_open_buffers', 
            'set_selection', 'get_text', 'make_position_visible', 'len', 
            'set_text',
            'insert', 'delete', 'backspace',
            'goto', 'active_buffer_name', 
            'file_name',
            'indent', 'insert_indent', 
            'incr_indent_level',
            'decr_indent_level',
            'line_num_of', 'goto_line',
            'beginning_of_line', 'end_of_line',
            'process_active',
            'suspendable',
            'suspend_notification',
            'shared_window',
            'set_instance_string', 'get_instance_string',
            'title_escape',
            'multiple_windows',
            'multiple_buffers', 'bidirectional_selection', 'get_visible', 
            'language_name', 'newline_conventions', 
            'pref_newline_convention', 'open_file', 'close_buffer', 
            'save_file',
            'terminating', 'mediator_closing', 'updates',
            'broken_connection']
        for msg_name in self.expect:
            method_name = 'cmd_' + msg_name
#            msg_handler = self.__class__.__dict__[method_name]
            msg_handler = getattr(self, method_name)
            self.msg_map[msg_name] = msg_handler
        self.editor_name = self.editor.name()
        if self.editor_name == None:
            self.editor_name = 'client'
            self.editor.set_name(self.editor_name)
        self.editor.set_manager(self)
         

    def disconnect(self):
        """method to call to tell the ClientEditor to send a message 
        disconnecting from the mediator

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        if self.connected:
            self.talk_msgr.send_mess('editor_disconnecting')
        self.disconnected()

    def disconnected(self):
        """method to call to let the ClientEditor know that the client
        has disconnected from the mediator
        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        self.connected = 0
        self.talk_msgr = None
        self.listen_msgr = None
        self.editor.set_change_callback(None)

    def connect(self, talk_msgr, listen_msgr):
        """method to call to let the ClientEditor know that the client
        has connected to the mediator, and can communicate with it by
        means of the provided talk_msgr and listen_msgr.

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        self.connected = 1
        self.talk_msgr = talk_msgr
        self.listen_msgr = listen_msgr
        self.editor.set_change_callback(self.on_change)
        self.mediator_closing = 0
        
    def send_updates(self, update_list):
        """send a list of updates to the mediator

        **INPUTS**

        *[{STR: ANY}]* update_list -- list of update dictionaries (see
        messaging.py for format)

        **OUTPUTS**

        *none*
        """
        if self.connected:
            self.talk_msgr.send_mess('updates', {'value': update_list})
            
    def send_simple_response(self, name):
        """send a simple response without any arguments to a message 
        to the mediator via the listen_msgr in response to a message 
        from the mediator 

        **INPUTS**

        *STR* name -- name of the response

        **OUTPUTS**

        *none*
        """
        if self.connected:
            self.listen_msgr.send_mess(name)

    def send_updates_response(self, name, updates):
        """send a response to a message to the mediator via the 
        listen_msgr in response to a message from the mediator,

        **INPUTS**

        *STR* name -- name of the response

        *{STR: ANY}* updates -- dictionary with arguments of the updates
        message

        **OUTPUTS**

        *none*
        """
        self.send_response(name, updates, value_name = 'updates')

    def send_response(self, name, value, value_name = None):
        """send a response to a message to the mediator via the 
        listen_msgr in response to a message from the mediator 

        **INPUTS**

        *STR* name -- name of the response

        *{STR: ANY}* value -- dictionary with arguments of the message

        *STR value_name* --  name to use for the value (defaults to
        'value', but occasionally 'updates' is needed)

        **OUTPUTS**

        *none*
        """
        if self.connected:
            v_name = 'value'
            if value_name:
                v_name = value_name
            self.listen_msgr.send_mess(name, {v_name: value})

    def on_change(self, start, end, text, selection_start,
        selection_end, buff_name, program_initiated):
        """callback from AppState to notify us of a change to one of its
        buffers

        *INT start, end* -- character offsets of start and end of 
        changed region (before the change)

        *STR text* -- text with which that range was replaced

        *INT selection_start, selection_end* -- character offsets of the
        selection (after the change)

        *STR buff_name* -- buffer which was changed

        *BOOL program_initiated* -- flag indicating whether the change
        was program-initiated (i.e. in response to a command from the
        mediator via ClientEditor) or user-initiated
        """
        updates = {'range': (start, end), 'buff_name': buff_name}
        if start is end is None:
            del updates['range']
            updates['action'] = 'buff_contents'
            updates['text'] = text
        elif text == "":
            updates['action'] = 'delete'
        else:
            updates['action'] = 'insert'
            updates['text'] = text
        if program_initiated:
            debug.trace('ClientEditor.on_change', 'program-initiated change')
            if self.awaiting_response == None:
                err = 'error: ClientEditor received program_initiated '
                err = err + 'change callback:\n'
                err = err + '(%d, %d), [%s], %s' % (start, end, text, buff_name)
                err = err + '\nbut awaiting_response was None'
                sys.stderr.write(err)
            else:
                self.awaiting_response.append(updates)
        else:
            debug.trace('ClientEditor.on_change', 'user-initiated change')
            debug.trace('ClientEditor.on_change', 
              'range, text = (%d, %d), "%s"' % (start, end, text) )
            update_list = [updates] + self.pos_selection_update(buff_name)
            self.send_updates(update_list)

    def mediator_cmd(self):
        """method to call when the main thread receives a message
        from the data thread that a command from the mediator is waiting
        in the queue

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        if self.connected:
            cmd = self.listen_msgr.get_mess(expect = self.expect)
            if cmd:
                self.do_cmd(cmd)

    def do_cmd(self, cmd):
        """perform the appropriate action in response to the command
        from the mediator

        **INPUTS**

        (STR, {STR: STR}) cmd -- The message retrieved
        from external editor in *(mess_name, {arg:val})* format
 
        **OUTPUTS**

        *none*
        """

        debug.trace('ClientEditor.do_cmd', 'cmd=%s' % repr(cmd))
        handler = self.msg_map[cmd[0]]
        handler(cmd[1])

    def close_app_cbk(self, instance, unexpected = 0):
        """callback from AppState which indicates that the application has 
        closed 

        **INPUTS**

        *STR* instance -- name of the application instance to be removed
    
        **OUTPUTS**

        *none*
        """
        self.disconnect()
        self.owner.app_closing(self.ID)

    def close_buffer_cbk(self, instance, buff_name):
        """callback from AppState which notifies us that the application
        has closed a buffer

        **INPUTS**

        *STR* instance -- name of the application instance 

        *STR* buff_name -- name of the buffer which was closed

        **OUTPUTS**

        *none*
        """
        if self.editor_name == instance:
            if not self.ignore_callbacks:
                updates = {'action': 'close_buff', 'buff_name': buff_name}
# this doesn't make any sense -- the buffer doesn't
# exist any more
#                update_list = [updates] + self.pos_selection_update(buff_name)
                update_list = [updates]
                self.send_updates(update_list)

    def open_buffer_cbk(self, instance, buff_name):
        """callback from AppState which notifies us that the application
        has opened a new buffer 

        **INPUTS**

        *STR* instance -- name of the application instance 

        *STR* buff_name -- name of the buffer which was opened

        **OUTPUTS**

        *none*
        """
        if self.editor_name == instance:
            if not self.ignore_callbacks:
                updates = {'action': 'open_buff', 'buff_name': buff_name}
                update_list = [updates] + self.pos_selection_update(buff_name)
                self.send_updates(update_list)

    def curr_buff_name_cbk(self, instance, buff_name):
        """callback from AppState which notifies us that the current
        buffer has changed

        **INPUTS**

        *STR* instance -- name of the application instance 

        *STR* buff_name -- name of the newly current buffer 

        **OUTPUTS**

        *none*
        """
        if self.editor_name == instance:
            if not self.ignore_callbacks:
                updates = {'action': 'curr_buffer', 'buff_name': buff_name}
                update_list = [updates] + self.pos_selection_update(buff_name)
                self.send_updates(update_list)

    def rename_buffer_cbk(self, instance, old_buff_name, new_buff_name):
        """callback from AppState which notifies us that the application
        has renamed a buffer

        **INPUTS**

        *STR* instance -- name of the application instance 

        *STR* old_buff_name -- old name of the buffer 

        *STR* new_buff_name -- new name of the buffer 

        **OUTPUTS**

        *none*
        """
        if self.editor_name == instance:
            if not self.ignore_callbacks:
                updates = {'action': 'rename_buff', 
                           'old_buff_name': old_buff_name,
                           'new_buff_name': new_buff_name
                          }
                buff_name = self.editor.curr_buffer()
                update_list = [updates] + self.pos_selection_update(buff_name)
                self.send_updates(update_list)

    def new_window(self, instance):
        """called when the editor notifies us of a new window for the 
        specified instance

        **INPUTS**

        *STR* instance -- name of the application instance

        **OUTPUTS**

        *BOOL* -- true if window is added
        """
        if self.editor_name == instance:
            if not self.ignore_callbacks:
                buff_name = self.editor.app_active_buffer_name()
                updates = {'action': 'new_window'}
                update_list = [updates] + self.pos_selection_update(buff_name)
                self.send_updates(update_list)
              
    def suspend_cbk(self, instance):
        """called when the editor notifies us that its process is about
        to be suspended

        **INPUTS**

        *STR* instance -- name of the application instance

        **OUTPUTS**

        *none*
        """
        if self.connected:
            self.talk_msgr.send_mess('suspended')

    def resume_cbk(self, instance):
        """called when the editor notifies us that its process has 
        resumed after having been suspended 

        **INPUTS**

        *STR* instance -- name of the application instance

        **OUTPUTS**

        *none*
        """
        if self.connected:
            self.talk_msgr.send_mess('resuming')

    def cmd_process_active(self, arguments):
        value = self.editor.is_active()
        self.send_response('process_active_resp', value)

    def cmd_suspendable(self, arguments):
        value = self.editor.suspendable()
        self.send_response('suspendable_resp', value)
# technically, if ClientEditor is running in a separate process from the editor,
# this should also depend on whether ClientEditor can be suspended.
# However, for now, we just assume that the user won't do that.

    def cmd_suspend_notification(self, arguments):
        value = self.editor.suspend_notification()
        self.send_response('suspend_notification_resp', value)
# technically, if ClientEditor is running in a separate process from the editor,
# this should also depend on whether ClientEditor can detect if *it* is
# about to be suspended and notify the server.
# However, for now, we just assume that the user won't do that.

    def cmd_shared_window(self, arguments):
        value = self.editor.shared_window()
        self.send_response('shared_window_resp', value)

    def cmd_set_instance_string(self, arguments):
        instance_string = arguments['instance_string']
        value = self.editor.set_instance_string(instance_string)
        self.send_response('set_instance_string_resp', value)

    def cmd_get_instance_string(self, arguments):
        instance_string = self.editor.instance_string()
        self.send_response('get_instance_string_resp', instance_string)

    def cmd_title_escape(self, arguments):
        before = arguments['before']
        after = arguments['after']
        value = self.editor.title_escape_sequence(before, after)
        self.send_response('title_escape_resp', value)

    def cmd_multiple_windows(self, arguments):
        value = self.editor.multiple_windows()
        self.send_response('multiple_windows_resp', value)

    def cmd_multiple_buffers(self, arguments):
        value = self.editor.multiple_buffers()
        self.send_response('multiple_buffers_resp', value)

    def cmd_bidirectional_selection(self, arguments):
        value = self.editor.bidirectional_selection()
        self.send_response('bidirectional_selection_resp', value)

    def cmd_active_buffer_name(self, arguments):
        buff_name = self.editor.app_active_buffer_name()
        self.send_response('active_buffer_name_resp', buff_name)

    def cmd_recog_begin(self, arguments):
        window_id = messaging.messarg2int(arguments['window_id'])
        block = messaging.messarg2int(arguments['block'])
        value = self.editor.recog_begin(window_id, block)
        self.send_response('recog_begin_resp', value)

    def cmd_recog_end(self, arguments):
        value = self.editor.recog_end()
        self.send_simple_response('recog_end_resp')

    def cmd_get_pos_selection(self, arguments):
        buff_name = arguments['buff_name']
        pos, selection = self.editor.get_pos_selection(buff_name = buff_name)
        value = {}
        value['pos'] = pos
        value['selection'] = selection
        self.send_response('get_pos_selection_resp', value)

    def cmd_confirm_buffer_exists(self, arguments):
        buff_name = arguments['buff_name']
        value = self.editor.query_buffer_from_app(buff_name)
        self.send_response('confirm_buffer_exists_resp', value)

    def cmd_list_open_buffers(self, arguments):
        value = self.editor.open_buffers_from_app()
        self.send_response('list_open_buffers_resp', value)

    def pos_selection_update(self, buff_name = None):
        """create an update description for the current cursor location
        and selection for a given buffer

        **INPUTS**

        *STR buff_name* -- name of the buffer, or None for the current
        buffer

        **OUTPUTS**

        *[{STR:ANY}]* -- the update descriptions
        """
        if buff_name == None:
            buff_name = self.editor.app_active_buffer_name()
        buff = self.editor.find_buff(buff_name)
        updates = []
        if buff != None:
            pos, selection = buff.get_pos_selection()
            updates = [{'action': 'pos_selection', 'selection': selection,
                'pos': pos, 
                'buff_name': buff_name}]        

        return updates
        
    def cmd_set_selection(self, arguments):
        buff_name = arguments['buff_name']
        range = messaging.messarg2inttuple(arguments['range'])
        cursor_at = messaging.messarg2int(arguments['cursor_at'])
        self.editor.set_selection(range, cursor_at, buff_name = buff_name)
        updates =  self.pos_selection_update(buff_name)
        self.send_updates_response('set_selection_resp', updates)

    def cmd_get_text(self, arguments):
        buff_name = arguments['buff_name']
        start = messaging.messarg2int(arguments['start'])
        end = messaging.messarg2int(arguments['end'])
        value = self.editor.get_text(start = start, end = end, 
            buff_name = buff_name)
        self.send_response('get_text_resp', value)
 
    def cmd_set_text(self, arguments):
        buff_name = arguments['buff_name']
        text = arguments['text']
        start = messaging.messarg2int(arguments['start'])
        end = messaging.messarg2int(arguments['end'])
#        print start, end
        self.awaiting_response = []
        b_name = buff_name
        if b_name == None:
            b_name = self.editor.curr_buffer()
        self.editor.set_text(text, start = start, end = end, 
            buff_name = buff_name)
        updates = self.awaiting_response
        self.awaiting_response = None
        updates = updates + self.pos_selection_update(b_name)
        self.send_updates_response('set_text_resp', updates)

    def cmd_make_position_visible(self, arguments):
        buff_name = arguments['buff_name']
        self.editor.make_position_visible(buff_name = buff_name)
        self.send_simple_response('make_position_visible_resp') 

    def cmd_len(self, arguments):
        buff_name = arguments['buff_name']
        value = self.editor.len(buff_name = buff_name)
        self.send_response('len', value)

    def cmd_line_num_of(self, arguments):
        buff_name = arguments['buff_name']
#        print arguments
        position = messaging.messarg2int(arguments['position'])
        value = self.editor.line_num_of(position = position, buff_name = buff_name)
        self.send_response('line_num_of_resp', value)

    def cmd_beginning_of_line(self, arguments):
        buff_name = arguments['buff_name']
        pos = messaging.messarg2int(arguments['pos'])
        value = self.editor.beginning_of_line(pos = pos, buff_name = buff_name)
        self.send_response('beginning_of_line_resp', value)

    def cmd_end_of_line(self, arguments):
        buff_name = arguments['buff_name']
        pos = messaging.messarg2int(arguments['pos'])
        value = self.editor.end_of_line(pos = pos, buff_name = buff_name)
        self.send_response('end_of_line_resp', value)

    def cmd_indent(self, arguments):
        buff_name = arguments['buff_name']
        range = messaging.messarg2inttuple(arguments['range'])
        self.awaiting_response = []
        b_name = buff_name
        if b_name == None:
            b_name = self.editor.curr_buffer()
        self.editor.indent(range, buff_name = buff_name)
        self.editor.print_buff_if_necessary(buff_name = buff_name)
        updates = self.awaiting_response
        self.awaiting_response = None
        updates = updates + self.pos_selection_update(b_name)
        self.send_updates_response('indent_resp', updates)

    def cmd_insert(self, arguments):
        buff_name = arguments['buff_name']
        text = arguments['text']
        range = messaging.messarg2inttuple(arguments['range'])
        self.awaiting_response = []
        b_name = buff_name
        if b_name == None:
            b_name = self.editor.curr_buffer()
        self.editor.insert(text, range, buff_name = buff_name)
        self.editor.print_buff_if_necessary(buff_name = buff_name)
        updates = self.awaiting_response
        self.awaiting_response = None
        updates = updates + self.pos_selection_update(b_name)
        self.send_updates_response('insert_resp', updates)

    def cmd_insert_indent(self, arguments):
        buff_name = arguments['buff_name']
        code_bef = arguments['code_bef']
        code_after = arguments['code_after']
        range = messaging.messarg2inttuple(arguments['range'])
        self.awaiting_response = []
        b_name = buff_name
        if b_name == None:
            b_name = self.editor.curr_buffer()
        self.editor.insert_indent(code_bef, code_after, range, 
            buff_name = buff_name)
        self.editor.print_buff_if_necessary(buff_name = buff_name)
        updates = self.awaiting_response
        self.awaiting_response = None
        updates = updates + self.pos_selection_update(b_name)
        self.send_updates_response('insert_indent_resp', updates)

    def cmd_file_name(self, arguments):
        buff_name = arguments['buff_name']
        value = self.editor.file_name(buff_name = buff_name)
        self.send_response('file_name_resp', value)

    def cmd_backspace(self, arguments):
        buff_name = arguments['buff_name']
        n_times = messaging.messarg2int(arguments['n_times'])
        self.awaiting_response = []
        b_name = buff_name
        if b_name == None:
            b_name = self.editor.curr_buffer()
        self.editor.backspace(n_times, buff_name = buff_name)
        self.editor.print_buff_if_necessary(buff_name = buff_name)
        updates = self.awaiting_response
        self.awaiting_response = None
        updates = updates + self.pos_selection_update(b_name)
        self.send_updates_response('backspace_resp', updates)

    def cmd_delete(self, arguments):
        buff_name = arguments['buff_name']
        range = messaging.messarg2inttuple(arguments['range'])
        self.awaiting_response = []
        b_name = buff_name
        if b_name == None:
            b_name = self.editor.curr_buffer()
        self.editor.delete(range, buff_name = buff_name)
        self.editor.print_buff_if_necessary(buff_name = buff_name)
        updates = self.awaiting_response
        self.awaiting_response = None
        updates = updates + self.pos_selection_update(b_name)
        self.send_updates_response('delete_resp', updates)

    def cmd_incr_indent_level(self, arguments):
        buff_name = arguments['buff_name']
        levels = arguments['levels']
        range = messaging.messarg2inttuple(arguments['range'])
        self.awaiting_response = []
        b_name = buff_name
        if b_name == None:
            b_name = self.editor.curr_buffer()
        self.editor.incr_indent_level(levels, range, buff_name = buff_name)
        self.editor.print_buff_if_necessary(buff_name = buff_name)
        updates = self.awaiting_response
        self.awaiting_response = None
        updates = updates + self.pos_selection_update(b_name)
        self.send_updates_response('incr_indent_level_resp', updates)

    def cmd_decr_indent_level(self, arguments):
        buff_name = arguments['buff_name']
        levels = messaging.messarg2int(arguments['levels'])
        range = messaging.messarg2inttuple(arguments['range'])
        self.awaiting_response = []
        b_name = buff_name
        if b_name == None:
            b_name = self.editor.curr_buffer()
        self.editor.decr_indent_level(levels, range, buff_name = buff_name)
        self.editor.print_buff_if_necessary(buff_name = buff_name)
        updates = self.awaiting_response
        self.awaiting_response = None
        updates = updates + self.pos_selection_update(b_name)
        self.send_updates_response('decr_indent_level_resp', updates)
    
    def cmd_goto(self, arguments):
        buff_name = arguments['buff_name']
        pos = messaging.messarg2int(arguments['pos'])
        self.awaiting_response = []
        b_name = buff_name
        if b_name == None:
            b_name = self.editor.curr_buffer()
        self.editor.goto(pos, buff_name = buff_name)
        updates = self.awaiting_response
        self.awaiting_response = None
        updates = updates + self.pos_selection_update(b_name)
        self.send_updates_response('goto_resp', updates)

    def cmd_goto_line(self, arguments):
        buff_name = arguments['buff_name']
        linenum = messaging.messarg2int(arguments['linenum'])
        where = messaging.messarg2int(arguments['where'])
        self.awaiting_response = []
        b_name = buff_name
        if b_name == None:
            b_name = self.editor.curr_buffer()
        self.editor.goto_line(linenum, where, buff_name = buff_name)
        updates = self.awaiting_response
        self.awaiting_response = None
        updates = updates + self.pos_selection_update(b_name)
        self.send_updates_response('goto_line_resp', updates)
    
    def cmd_get_visible(self, arguments):
        buff_name = arguments['buff_name']
        value = self.editor.get_visible(buff_name = buff_name)
        self.send_response('get_visible_resp', value)

    def cmd_language_name(self, arguments):
        buff_name = arguments['buff_name']
#        print 'querying language for "%s"' % buff_name
        value = self.editor.language_name(buff_name = buff_name)
        self.send_response('language_name_resp', value)
#        print 'returning "%s"' % value

    def cmd_newline_conventions(self, arguments):
        buff_name = arguments['buff_name']
        value = self.editor.newline_conventions(buff_name = buff_name)
        self.send_response('newline_conventions_resp', value)

    def cmd_pref_newline_convention(self, arguments):
        buff_name = arguments['buff_name']
        value = self.editor.pref_newline_convention(buff_name = buff_name)
        self.send_response('pref_newline_convention_resp', value)

    def cmd_open_file(self, arguments):
        file_name = arguments['file_name']
        self.ignore_callbacks = 1
        new_buff_name = self.editor.tell_editor_to_open_file(file_name = file_name)
        self.ignore_callbacks = 0
        self.send_response('open_file_resp', new_buff_name, 
            value_name = 'buff_name')

    def cmd_close_buffer(self, arguments):
#        print 'client received close buffer command with arguments', arguments
        buff_name = arguments['buff_name']
        save = messaging.messarg2int(arguments['save'])
        self.ignore_callbacks = 1
        success = self.editor.app_close_buffer(buff_name, save)
        self.ignore_callbacks = 0
        self.send_response('close_buffer_resp', success)

    def cmd_save_file(self, arguments):
        file_name = arguments['full_path']
        no_prompt = arguments['no_prompt']
        self.ignore_callbacks = 1
        new_buff_name = self.editor.app_save_file(full_path = full_path,
            no_prompt = no_prompt)
        self.ignore_callbacks = 0
        self.send_response('save_file_resp', new_buff_name, 
            value_name = 'buff_name')

    def cmd_terminating(self, arguments):
        self.cmd_mediator_closing(arguments)

    def cmd_mediator_closing(self, arguments):
        self.mediator_closing = 1
        self.owner.mediator_closing(self.ID, unexpected = 0)

    def cmd_broken_connection(self, arguments):
        if not self.mediator_closing:
            self.mediator_closing = 1
            self.owner.mediator_closing(self.ID, unexpected = 1)

    def cmd_updates(self, arguments):
        debug.virtual('ClientEditor.cmd_updates')

class ClientEditorChangeSpec(ClientEditor):
    """implementation of ClientEditor for an editor which supports change
    specification events for all user-initiated changes

    **INSTANCE ATTRIBUTES**

    *none*

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, **args):
        self.deep_construct(ClientEditorChangeSpec,
                            {
                            },
                            args)

    def cmd_updates(self, arguments):
# editor supports change specification events for all user-initiated
# changes.  Therefore, on_change will already handle updates, except for
# selection and current buffer changes, so those are the only ones we
# need to send here
        updates =  self.pos_selection_update()
#        self.send_updates([updates])
        self.send_response('updates', updates)

class DummyDataEvent(thread_communication.SocketHasDataEvent):
    """dummy SocketHasDataEvent for UneventfulLoop
    """
    def notify(self):
# UneventfulLoop will poll for data, and has no event loops, so we 
# don't need to notify the main thread of anything.
        pass

class UneventfulLoop(Object.OwnerObject):
    """class for running the EdSim editor simulator as a TCP client

    Since EdSim has no user-interface, we don't need an event loop.
    Instead, we just have a loop to poll for
    messages from the server.

    As with test_TCP_server, we exit once the connection has been
    closed.

    **INSTANCE ATTRIBUTES**

    ClientConnection *connection* -- the connection to the mediator
    server

    ClientEditorChangeSpec *editor* -- the client wrapper for the EdSim 
    instance

    BOOL *client_indentation* -- if true, use the name
    EdSimClientIndent when handshaking with the server, to ensure that
    the server will not override indentation on the server-side.

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, multiple = 0, print_buff = 0, client_indentation = 0, 
        **args):
        """
        **INPUTS**

        *BOOL multiple* -- should this EdSim allow for multiple open
        buffers?

        *BOOL print_buff* -- should this EdSim call print_buff whenever
        the buffer changes?

        BOOL *client_indentation* -- if true, use the name
        EdSimClientIndent when handshaking with the server, to ensure that
        the server will not override indentation on the server-side.
        """
        self.deep_construct(UneventfulLoop,
                            {
                             'connection': ClientConnection(),
                             'client_indentation': client_indentation,
                             'editor': None
                            }, args)
        underlying_editor = EdSim.EdSim(multiple = multiple, 
           print_buff_when_changed = print_buff)
        self.editor = ClientEditorChangeSpec(editor = underlying_editor, 
            owner = self, ID = 'dummy', owns_editor = 1)
        self.add_owned('editor')

    def app_closing(self, ID):
# ClientEditor expects this to be defined, but EdSim doesn't ever
# generate a close_app_cbk, so we don't ever expect to receive this.
        sys.stderr.write('editor closed\n')
        self.quit_flag = 1

    def mediator_closing(self, ID, unexpected = 0):
        if unexpected:
            sys.stderr.write('mediator connection broken unexpectedly\n')
        else:
            sys.stderr.write('mediator disconnected\n')
        self.quit_flag = 1

    def run(self, host = None, listen_port = None, talk_port = None):
        client_name = 'EdSim'
        if self.client_indentation:
            client_name = 'EdSimClientIndent'
        messengers = self.connection.connect(client_name, DummyDataEvent(), 
            host = host, listen_port = listen_port, talk_port = talk_port, 
            test_client = 1)
        if messengers == None:
            sys.stderr.write('Unable to connect to server\n')
            return
        talk, listen = messengers
        self.editor.connect(talk, listen)
        self.quit_flag = 0
        while not self.quit_flag:
            if listen.receiver.empty():
                time.sleep(.05)
            else:
                self.editor.mediator_cmd()
        self.editor.disconnected()
        self.connection.disconnect()
        return
        
class ClientMainThread(Object.OwnerObject):
    """abstract base class for the main thread of an editor client to
    the mediator server.

    **INSTANCE ATTRIBUTES**

    ClientConnection *connection* -- the connection to the mediator
    server

    Queue *mediator_cmds* -- the queue to which commands from the
    mediator are added

    AppState, AppChangeSpec *editor* -- the editor, supporting the
    AppState interface, as well as the AppChangeSpec interface for

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, **args):
        self.deep_construct(ClientMainThread,
                            {
                             'connection': None,
                             'editor': None,
                             'mediator_cmds': None
                            },
                            args)






