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

"""A VoiceCode server that uses TCP/IP based messaging protocol to communicate with external editors.
"""

import vc_globals

import natlink, os, posixpath, pythoncom, re, select, socket
import SocketServer, string, sys, threading, time, whrandom, win32event

import AppStateEmacs, AppStateMessaging, auto_test, debug
import messaging, Object
import AppMgr, RecogStartMgr, SourceBuffMessaging, sb_services
import as_services
import sim_commands, sr_interface, util
import Queue
import regression

from tcp_threads import *
from thread_communication_win32 import *

#
# Port numbers for the communication link
#
VC_LISTEN_PORT = 45770
VC_TALK_PORT = 45771


def prompt_for_cmd():
    cmd = raw_input('Command> ')
    mediator.execute_command(cmd)                


class SB_MessExtEdSim(SourceBuffMessaging.SourceBuffMessaging):
    """Communicates with an external [EdSim] through a messaging link.

    This subclass of [SourceBuff] is designed to interact with an [EdSim]
    instance running in a different process.

    It is used mostly for debugging and regression testing purposes.
    
    **INSTANCE ATTRIBUTES**

    [SB_ServiceIndent] *indent_srv* -- Code indentation service used to
    provide indentation at the server level.

    {SB_ServiceLineManip] *lines_srv* -- Line numbering service.

    **CLASS ATTRIBUTES**
    
    *none* -- 

    ..[SB_ServiceLineManip] file:///./sb_services.SB_ServiceLineManip.html
    ..[SB_ServiceIndent] file:///./sb_services.SB_ServiceIndent.html
    ..[SourceBuff] file:///./SourceBuff.SourceBuff.html
    ..[EdSim] file:///./EdSim.EdSim.html"""
    
    def __init__(self, **args_super):
        self.deep_construct(SB_MessExtEdSim, 
                            {'indent_srv': sb_services.SB_ServiceIndent(buff=self, indent_level=3, indent_to_curr_level = 1),
                             'lines_srv': sb_services.SB_ServiceLineManip(buff=self)}, 
                            args_super, 
                            {})
        self.add_owned_list(['indent_srv', 'lines_srv'])

    def insert_indent(self, code_bef, code_after, range = None):
        self.indent_srv.insert_indent(code_bef, code_after, range)

    def uses_server_side_indent(self):
        return 1

    def indent(self, range = None):
        self.indent_srv.indent(range = range)
        
    def incr_indent_level(self, levels=1, range=None):
        self.indent_srv.incr_indent_level(levels=levels, range=range)

    def decr_indent_level(self, levels=1, range=None):
        self.indent_srv.decr_indent_level(levels=levels, range=range)
        
    def line_num_of(self, position = None):
        return self.lines_srv.line_num_of(position)

    def beginning_of_line(self, pos):
        return self.lines_srv.beginning_of_line(pos)

    def end_of_line(self, pos):    
        return self.lines_srv.end_of_line(pos)

    def goto_line(self, linenum, where=-1):
        self.lines_srv.goto_line(linenum, where)

class AS_MessExtEdSim(AppStateMessaging.AppStateMessaging):
    """Communicates with an external [EdSim] through a messaging link.

    This subclass of [AppState] is designed to interact with an [EdSim]
    instance running in a different process.

    It is used mostly for debugging and regression testing purposes.
    
    **INSTANCE ATTRIBUTES**
    
    *none*-- 

    **CLASS ATTRIBUTES**
    
    *none* -- 

    ..[AppState] file:///./AppState.AppState.html
    ..[EdSim] file:///./EdSim.EdSim.html"""
    
    def __init__(self, **args_super):
        self.deep_construct(AS_MessExtEdSim, 
                            {
                             'breadcrumbs_srv': 
                             as_services.AS_ServiceBreadcrumbs(app=self)
                            }, 
                            args_super, 
                            {})
        self.add_owned('breadcrumbs_srv')

    def new_compatible_sb(self, buff_name):
        buff = SB_MessExtEdSim(app=self, buff_name=buff_name)
        return buff
    def drop_breadcrumb(self, buff_name=None, pos=None):

        """Drops a breadcrumb

        *INT pos* is the position where to drop the crumb. *STR
         buff_name* is the name of the source buffer.
        
        If *pos* not specified, drop breadcrumb at cursor position.

        If *buff* not specified either, drop breadcrumb in current buffer
        """
        self.breadcrumbs_srv.drop_breadcrumb(buff_name, pos)


    def pop_breadcrumbs(self, num=1, gothere=1):
        """Pops breadcrumbs from the breadcrumbs stack

        *INT num* is the number of crumbs to pop. If None, then pop 1 crumb.

        if *BOOL gothere* is true, then move cursor to the last popped
        breadcrumb.
        """
        self.breadcrumbs_srv.pop_breadcrumbs(num, gothere)

        


class AppStateFactory(Object.Object):
    """factory which produces new instances of concrete subclasses of 
    AppState for the TCP server

    **INSTANCE ATTRIBUTES**

    *none*

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, **args):
        self.deep_construct(AppStateFactory,
                            {
                            },
                            args)

    def new_instance(self, app_name, id, listen_msgr, talk_msgr,
        listen_can_block = 0):
        """create a new AppState of the subclass appropriate to the given
        app_name.

        **INPUTS**
            
        STR *app_name* -- Name of the editor for which we want to
        create an [AppStateMessaging].

        STR *id* -- Unique ID of external editor connected to the [AppState]
        
        [Messenger] *listen_msgr* -- [Messenger] instance to use for the
        VC listener side of the connection.

        [Messenger] *talk_msgr* -- [Messenger] instance to use for the
        VC talker side of the connection.
      
        *BOOL listen_can_block* -- flag indicating that the listen_msgr to
        can block on get_mess if there is no message waiting.  This flag is
        provided only for compatibility with ServerSingleThread.  All other
        servers should be set up so that listen_msgr avoids blocking and 
        simply return None if there are no messages.

        **OUTPUTS**
        
        *AppStateMessaging* -- the new AppStateMessaging representing the
        editor instance 

        ..[AppState] file:///./AppState.AppState.html"""

        debug.virtual('AppStateFactory.new_instance')

class AppStateFactorySimple(AppStateFactory):
    """simple implementation of AppStateFactory for testing purposes

    **INSTANCE ATTRIBUTES**

    *none*

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, **args):
        self.deep_construct(AppStateFactorySimple,
                            { 
                            },
                            args)
    
    def new_instance(self, app_name, id, listen_msgr, talk_msgr,
        listen_can_block = 0):
        """create a new AppState of the subclass appropriate to the given
        app_name.

        **INPUTS**
            
        STR *app_name* -- Name of the editor for which we want to
        create an [AppStateMessaging].

        STR *id* -- Unique ID of external editor connected to the [AppState]
        
        [Messenger] *listen_msgr* -- [Messenger] instance to use for the
        VC listener side of the connection.

        [Messenger] *talk_msgr* -- [Messenger] instance to use for the
        VC talker side of the connection.
        
        *BOOL listen_can_block* -- flag indicating that the listen_msgr to
        can block on get_mess if there is no message waiting.  This flag is
        provided only for compatibility with ServerSingleThread.  All other
        servers should be set up so that listen_msgr avoids blocking and 
        simply return None if there are no messages.

        **OUTPUTS**
    
        **OUTPUTS**
        
        *AppStateMessaging* -- the new AppStateMessaging representing the
        editor instance 

        ..[AppState] file:///./AppState.AppState.html"""


        kw_args = {}
        if re.match('EdSim', app_name):
            as_class = AS_MessExtEdSim
        elif re.match('EdSimClientIndent', app_name):
            as_class = AppStateMessaging.AppStateInsertIndentMess
        elif re.match('WaxEdit', app_name):
            as_class = AS_MessExtEdSim
        elif re.match('WaxEditClientIndent', app_name):
            as_class = AppStateMessaging.AppStateInsertIndentMess
        elif re.match('emacs', app_name):
            as_class = AppStateEmacs.AppStateEmacs
# comment out the following line to use the old recog_begin scheme
            kw_args['use_ignored_key'] = 1
        else:
            print "WARNING: Unknown editor '%s'" % app_name
            print "Connection refused"
            return None
        
        kw_args['app_name'] = app_name
        kw_args['id'] = id
        kw_args['listen_msgr'] = listen_msgr
        kw_args['talk_msgr'] = talk_msgr
        kw_args['listen_can_block'] = listen_can_block
        app = apply(as_class, (), kw_args)
#        app.app_name = app_name
        return app




def vc_authentification(messenger):
    """Authentifies a VoiecCode user.

    For now, this function does nothing.

    **INPUTS**

    [Messenger] *messenger* -- Messenger to be used for the connection.

    [RecogStartMgr] *recog_mgr* -- Object responsible for
    dispatching recognition events to the various editors.
        

    **OUTPUTS**
        
    *none* -- 


    ..[Messenger] file:///./messaging.Messenger.html
    ..[RecogStartMgr] file:///./RecogStartMgr.RecogStartMgr.html"""
    

    pass

def parse_module_info(module_info):
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


##############################################################################
# Classes for new version of the server with abstract interfaces for
# sending messages between threads.
#
# calls to/from natlink are still handled by the main thread, like in
# ServerSingleThread.
##############################################################################

class ServerMainThread(Object.OwnerObject):
    """Abstract base class for the main thread of a TCP/IP based 
    VoiceCode server.

    ServerMainThread will launch several other threads:
    
    ListenNewEditorsThread listens for new editors to connect on the
    VC_LISTEN port.  It puts the new socket on the new_listen_socks
    Queue, and notifies ServerMainThread through a InterThreadEvent.
    This event will ensure that handshake_listen_socks is called to do
    the initial handshaking.  Once the listen socket has completed
    the initial handshaking, handshake_listen_socks appends it to the 
    pending_listen_socks list.  The editor will then open a 
    
    NewConnListThread listens for the talker connections on the VC_TALK
    port.  It appends the new socket to the new_talk_socks list and 
    notifies ServerMainThread through a InterThreadEvent.  This event
    will ensure that handshake_talk_socks is called.
    handshake_talk_socks will go through the new_talk_socks list and
    pair them up with the elements of the pending_listen_socks with
    matching IDs, and will call package_sock_pair.  
    
    package_sock_pair will create a ListenAndQueueMsgsThread to listen 
    for data on the listen_sock and queue complete messages.  
    package_sock_pair will also create an instance of a subclass of 
    AppStateMessaging, with a MixedMessenger which sends messages 
    directly on the talk_sock, but reads them from this queue.  
    The LAQM Thread also notifies ServerMainThread using a
    SocketHasDataEvent.  This event will ensure that
    process_ready_socks is called, which will call
    listen_one_transaction on the appropriate AppStateMessaging.
    AppStateMessaging will also get messages from the editor off of the
    queue when it synchronizes with the editor at recognition starting

    The communication between the subsidiary threads and the main
    thread requires the event objects described above and a message loop 
    which receives messages from these event objects and calls the proper 
    ServerMainThread methods.  Some subclasses of ServerMainThread
    (ServerOldMediatorWin32Evt) supply these events themselves, and 
    have an internal message loop.  Others require an external object,
    which owns ServerMainThread (directly or indirectly), to supply the 
    event objects and the message loop.  the events to be 
    
    This flexibility means that ServerMainThread relies on
    virtual functions to create the subsidiary threads.

    **INSTANCE ATTRIBUTES**

    [Queue] *new_listen_socks* -- Queue from which to get any new connections.
    Each item is [(socket, (STR, STR, (INT, STR, STR), BOOL))] 
    a 2ple consiting of a new (uninitialised) socket on the VC_LISTEN
    port, and data about that socket. The data is itself a 4ple
    consisting of: (a) identifier of external editor, (b) name of the
    external editor, (c) another tuple, consisting of window handle, title, 
    and module name of the active window, assumed to be the external editor, 
    and (d) a flag indicating whether the client is expecting to be used for
    regression testing. 

    *[(socket, (None, None, None, None))] new_talk_socks* -- the socket
    element of each 2ple is a new (uninitialised) socked
     on the VC_TALK port.  The data part of the 2ple is useless.

    [(socket, (STR, STR, (INT, STR, STR), BOOL)] 
    *pending_listen_socks=[]* -- Each entry is
    a 2ple consiting of a new (uninitialised) socket on the VC_LISTEN
    port, and data about that socket. The data is itself a 4ple
    consisting of: (a) identifier of external editor, (b) name of the
    external editor, (c) another tuple, consisting of window handle, title, 
    and module name of the active window, assumed to be the external editor, 
    and (d) a flag indicating whether the client is expecting to be used for
    regression testing. 
    Socks on the pending_listen_socks list have been through
    handshaking, but have not yet been packaged with corresponding
    talk_socks.
    
    *threading.lock new_socks_lock* -- Lock used to make sure that the
     main thread doesn't access the *new_talk_socks* list at the same 
     time as the thread that listen for new socket connections.

    [ListenNewEditorsThread] *new_listener_server* -- Thread that
    listens for new connections on the VC_LISTEN port.

    [NewConnListThread] *new_talker_server* -- Thread that
    listens for new connections on the VC_TALK port.

    {STR : ListenAndQueueMsgsThread} *data_threads* -- map from unique 
    socket IDs to threads which poll for data from the listen messenger
  
    {STR : Event} *connection_ending* -- map from each unique 
    socket IDs to a corresponding threading.Event 
    used to signal to the corresponding data thread that the connection is 
    ending, or the server is quitting

    *AppStateFactory editor_factory* -- factory for creating new
    AppStateMessaging instances
  
    **CLASS ATTRIBUTES**
    
    *none* -- 

    ..[AppStateMessaging] file:///./AppStateMessaging.AppStateMessaging.html
    """
    
    def __init__(self, editor_factory, **args_super):
        self.deep_construct(ServerMainThread, 
                            {'pending_listen_socks': [],
                             'new_talk_socks': [],
                             'new_socks_lock': threading.Lock(),
                             'new_listen_socks': Queue.Queue(5),
                             'data_threads': {},
                             'new_listener_server': None,
                             'new_talker_server': None,
                             'connection_ending': {},
                             'editor_factory': editor_factory
                             }, 
                            args_super)
      
    def remove_other_references(self):
        """Perform any cleanup prior to quitting.  Called when the main 
        thread has exited its event loop.  Subclasses which override
        this method should be sure to call their parent class's version
        after doing their own cleanup.

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        for id in self.data_threads.keys():
            self.deactivate_data_thread(id)
        Object.OwnerObject.remove_other_references(self)

    def is_test_server(self):
        """indicates whether this server running in test mode, and
        waiting for the next test client

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- true if the server was initialize in test mode
        """
        debug.virtual('ServerMainThread.is_test_server')

    def data_event(self, id):
        """virtual method which supplies a data_event for ServerMainThread 
        subclasses 
        
        **INPUTS**

        STR *id* -- The unique ID of the listen socket
        
        **OUTPUTS**
        
        *SocketHasDataEvent* -- the data event which will allow the
        data thread to ensure that process_ready_socks is called.
        """
        debug.virtual('ServerMainThread.data_event')

    def new_data_thread(self, id, listen_sock, connection_ending,
        testing = 0):
        """creates a new ListenAndQueueMsgsThread to monitor the
        listen_sock
        
        **INPUTS**

        STR *id* -- The unique ID of the listen socket
        
        socket *listen_sock* -- The listen socket

        Event *connection_ending* -- threading.Event 
        used to signal to the data thread that the connection is 
        ending, or the server is quitting

        *BOOL testing* -- indicates whether we are running in regression
        testing mode
        
        **OUTPUTS**
        
        [ListenAndQueueMsgsThread] -- the new threading.Thread object

        ..[ListenAndQueueMsgsThread] 
        file:///./tcp_server.ListenAndQueueMsgsThread.html"""        
        data_event = self.data_event(id)
        return self.new_data_thread_given_event(id, listen_sock, data_event, 
            connection_ending, testing = testing)

    def new_data_thread_given_event(self, id, listen_sock, data_event,
            connection_ending, testing = 0):
        """creates a new ListenAndQueueMsgsThread to monitor the
        listen_sock
        
        **INPUTS**

        STR *id* -- The unique ID of the listen socket
        
        socket *listen_sock* -- The listen socket

        SocketHasDataEvent *data_event* -- the SocketHasDataEvent event
        to pass to the new thread

        Event *connection_ending* -- threading.Event 
        used to signal to the data thread that the connection is 
        ending, or the server is quitting

        *BOOL testing* -- indicates whether we are running in regression
        testing mode
        
        **OUTPUTS**
        
        [ListenAndQueueMsgsThread] -- the new threading.Thread object

        ..[ListenAndQueueMsgsThread] 
        file:///./tcp_server.ListenAndQueueMsgsThread.html"""        
        sleeper = messaging.LightSleeper(connection_ending)
        sleepy_time = 0.05
        if testing:
            sleepy_time = 0.10
        a_msgr = messaging.messenger_factory(listen_sock, sleep = sleepy_time,
            sleeper = sleeper)
        queue = Queue.Queue(-1)
# a finite queue is causing problems for the text_mode regression test
#        queue = Queue.Queue(10)
        broken_connection = ('connection_broken', {})
        thread = ListenAndQueueMsgsThread( a_msgr, queue, data_event,
            connection_ending, broken_connection)
        return thread
      
    def deactivate_data_thread(self, id):
        """method to deactivate the data thread associated with a 
        given socket id.  **Note:** if the thread is daemonic (will not
        prevent the mediator process from ending), and the particular 
        thread class used doesn't provide a way to kill the thread, 
        this method may simply ensure that no messages from that thread
        are processed.

        **INPUTS**

        STR *id* -- The unique identifier assigned by VoiceCode to
        the socket pair.

        **OUTPUTS**

        *none*
        """
# we don't currently have a perfect way of deactivating the thread before 
# it puts any more messages on the Queue.  However,
# process_ready_socks does check to see if the id's it receives in the
# ready_socks argument are included in active_meds.keys().  Therefore,
# as long as the caller also removes the corresponding mediator, this
# may be sufficient.
        self.connection_ending[id].set()
        del self.connection_ending[id]
        try:
            del self.data_threads[id]
        except KeyError:
            pass

    def _new_instance(self, id, instance, window_info, test_client = 0):
        """add a new AppStateMessaging.  Called internally by
        package_sock_pair
        
        **INPUTS**
        
        STR *id* -- The unique ID of the listen socket

        AppStateMessaging *instance*  -- the new instance

        *(INT, STR, STR) window_info* -- the window id, title, and module name
        of the external editor, or None if not detected yet

        BOOL *test_client* -- flag indicating whether or not the client
        is expecting to be used for regression testing
        
        **OUTPUTS**
        
        *BOOL* -- true if the instance should be added to our list.
        false if we failed to create a new instance, or if we created
        it, but it should be destroyed because it has run
        the test suite and the server should exit 
        """
        debug.virtual('ServerMainThread._new_instance')
        
    def known_instance(self, id):
        """returns a reference to the AppStateMessaging instance 
        associated with  the given ID
        **INPUTS**
        
        STR *id* -- The unique ID of the listen socket

        **OUTPUTS**

        *AppStateMessaging* -- the corresponding instance, or None if
        the id is unknown
        
        *none*
        """
        debug.virtual('ServerMainThread.known_instance')

    def possible_editor_cleanup(self, app):
        """cleanup AppState corresponding to editor used in regression
        tests, unless we are using NewMediatorObject, where AppMgr will
        already have taken care of this.

        **INPUTS**

        *AppStateMessaging app* -- the editor interface

        **OUTPUTS**

        *none*
        """
        debug.virtual('ServerMainThread.possible_editor_cleanup')

        
    def user_message(self, message):
        """displays a user message via the appropriate channel
        (e.g. stdout, or a MediatorConsole status line, or an 
        editor-specific status line if supported.

        **INPUTS**

        *STR message* -- the message

        **OUTPUTS**

        *none*
        """
        print message

    def package_sock_pair(self, id, app_name, window_info, 
        listen_sock, talk_sock, test_client = 0):
        
        """Packages a listen and talk socket into an
        [AppStateMessaging] instance
        
        **INPUTS**
        
        STR *id* -- The unique identifier assigned by VoiceCode to
        that socket pair.

        STR *app_name* -- Name of the external editor.

        *(INT, STR, STR) window_info* -- the window id, title, and module name
        
        socket *listen_sock* -- The listen socket
        
        socket *talk_sock* -- The talk socket
        
        BOOL *test_client* -- flag indicating whether or not the client
        is expecting to be used for regression testing

        **OUTPUTS**
        
        *BOOL* -- false if the server should exit (because we're done
        running the test suite)

        ..[AppStateMessaging] file:///./messaging.AppStateMessaging.html"""        
        
        self.user_message('creating messengers')
        disconnect_event = threading.Event()
        self.connection_ending[id] = disconnect_event
        testing = test_client and self.is_test_server()
        data_thread = self.new_data_thread(id, listen_sock, 
            disconnect_event, testing = testing)
        messages = data_thread.message_queue()

        talk_msgr = messaging.messenger_factory(talk_sock)        
        listen_response_msgr = messaging.messenger_factory(listen_sock)        
        listen_msgr = messaging.MixedMessenger(listen_response_msgr, messages)
        an_app_state = self.editor_factory.new_instance(app_name, id, 
            listen_msgr, talk_msgr)

        data_thread.setDaemon(1)
        data_thread.start()

        #
        # Give external editor a chance to configure the AppStateMessaging
        #
        self.user_message('configuring from external')
        an_app_state.config_from_external()

        self.user_message('creating editor instance')
        stay_alive = self._new_instance(id, an_app_state, window_info, 
            test_client)
        if stay_alive:
#            self.user_message('created new editor instance')
            sys.stderr.write("successfully created new instance\n")
            self.data_threads[id] = data_thread
            return 1
        else:
#            sys.stderr.write("got 0 from _new_instance\n")
            self.deactivate_data_thread(id)
#            sys.stderr.write("sent signal to data thread\n")
#            data_thread.join(15.0)
#            sys.stderr.write("joined thread (or timed out)\n")
            del data_thread
#            sys.stderr.write("deleted our reference to data thread\n")
            self.possible_editor_cleanup(an_app_state)
#            sys.stderr.write("cleaned up app_state\n")
            if test_client and self.is_test_server():
# only if it was a test client and we ran the test suite should we quit now
                return 0
# with a regular client, we should still continue to run 
# even if we fail to create a new instance
            self.user_message('failed to create instance')
            return 1
        
    def handshake_listen_socks(self):
        """Invoked when a new socket connection was opened on VC_LISTEN port.
        
        **INPUTS**
        
        *none* -- 
        

        **OUTPUTS**
        
        *none* -- 
        """

        #
        # Get window handle of active application.
        #
        window_info = parse_module_info(natlink.getCurrentModule())


        #
        # Assume active window is the application that opened the last
        # socket conneciton in the list self.new_listen_socks.  This
        # may not hold if two editors connect to VoiceCode back to back
        # and rapidly.

#  handshake_listen_socks should never be called if there isn't a new listen 
#  sock, but just in case we catch the exception and ignore it
        try:
# 0 means don't block
            most_rec_sock = self.new_listen_socks.get(0)
        except Queue.Empty:
            return
        
        self.user_message('New connection')
        #
        # Create a temporary messenger for handshaking
        #
        a_messenger = messaging.messenger_factory(most_rec_sock)
               
        #
        # Get the external application name
        #
        a_messenger.send_mess('send_app_name')
        mess = a_messenger.get_mess(expect=['app_name'])
        app_name = mess[1]['value']
                
        self.user_message('New connection from %s' % app_name)
        #
        # Assign a random ID to the external editor, and send it on the socket
        # connection.
        #
        id = '%s_%s' % (app_name, repr(whrandom.random()))
        a_messenger.send_mess('your_id_is', {'value': id})
        a_messenger.get_mess(expect=['ok'])

        # query whether client expects to be used for regression tests
        a_messenger.send_mess('test_client_query')
        mess = a_messenger.get_mess(expect=['test_client_query_resp'])
        test_client = messaging.messarg2int(mess[1]['value'])
        
        #
        # Assign window_info, id and app_name to the last socket in the list of
        # new listen sockets
        #
        most_rec_data = (id, app_name, window_info, test_client)
        
# using this lock shouldn't be necessary, since only
# handshake_talk_socks is the only other one accessing
# pending_listen_socks, and it runs in the main thread just like we do,
# but just in case.
        self.new_socks_lock.acquire()

        self.pending_listen_socks.append((most_rec_sock, most_rec_data))

        self.new_socks_lock.release()
        self.user_message('awaiting corresponding talk connection')
        

    def handshake_talk_socks(self):
        
        """Does a handshake on a the new socket connection that were opened on
        VC_LISTEN port.
        
        **INPUTS**
        
        *none*

        **OUTPUTS**
        
        *BOOL* -- false if the server should exit (because we're done
        running the test suite)
        """

        stay_alive = 1
        self.new_socks_lock.acquire()
        self.user_message('received talk connection')

        #
        # Loop through list of new VC_TALK sockets, handshake with them
        # and package them into an AppState instance with their corresponding
        # VC_LISTEN sockets.
        #
        for sock in self.new_talk_socks:

            (talk_sock, dummy) = sock
        
            #
            # Shake hands with that VC_TALK socket.
            # Get the ID of external editor that started this VC_TALK socket
            #
            a_msgr = messaging.messenger_factory(talk_sock)
            a_msgr.send_mess('send_id')
            mess = a_msgr.get_mess(expect=['my_id_is'])
            id = mess[1]['value']

            #
            # Find the corresponding VC_LISTEN socket
            #
            found = None
            jj = 0
            while jj < len(self.pending_listen_socks):
                (listen_sock, listen_data) = self.pending_listen_socks[jj]
                (a_listen_id, app_name, window_info, test_client) = listen_data
                if a_listen_id == id:
                    #
                    # Found it. Remove the two sockets from the list of
                    # new connections.
                    #
                    found = (listen_sock, app_name, window_info, test_client)
                    del self.pending_listen_socks[jj]
                    break

                jj = jj + 1
                
            if found != None:
                stay_alive = self.package_sock_pair(id, app_name, 
                    window_info, listen_sock, talk_sock, test_client)
            else:
                talk_sock.close()
                self.user_message('no corresponding listen connection')
                        
# all new talk sockets will have been packaged or closed
        del self.new_talk_socks[:]
        self.new_socks_lock.release()        
#        if stay_alive == 0:
#            sys.stderr.write("got 0 from package_sock_pair\n")
        return stay_alive


    def process_ready_socks(self, ready_socks = None):
        """Processes socket connections that have received new data.
        
        **INPUTS**

        [STR] *ready_socks* -- List of IDs of sockets which may
        have messages waiting, or None to check all sockets in
        data_threads.keys()
        
        **OUTPUTS**
        
        *none* -- 
        """

        socks = ready_socks
        if ready_socks == None:
            socks = self.data_threads.keys()
        for id in socks:
            an_app_state = self.known_instance(id)
            if an_app_state != None:
                an_app_state.listen_one_transaction()

    def start_other_threads(self, listener_evt, talker_evt):
        """method called to start the secondary threads which
        monitor the VC_TALK and VC_LISTEN ports.  These threads communicate
        with the main thread by means of InterThreadEvent objects, to
        let the main thread know to initialize them.

        These tasks are handled by separate threads because they can
        block.   The secondary threads do not do the initialization
        directly because that involves invoking some natlink methods, 
        and Natlink does not behave well outside of the main thread.
        
        **INPUTS**
        
        *InterThreadEvent* listener_evt -- event object for the
        ListenNewEditorsThread to use to notify the main thread that a
        new editor has connected on the VC_LISTEN port, and that
        handshake_listen_socks should be called
        
        *InterThreadEvent* talker_evt -- event object for the
        NewConnListThread to use to notify the main thread that a
        new talker connection has been established on the VC_TALK port, 
        and that handshake_talk_socks should be called

        **OUTPUTS**
        
        *none* 
        """

        #
        # Start threads for monitoring socket connections.
        # We make the threads daemonic so that the program exits automaticall
        # when only those threads are left
        #

        #
        # This thread listens for new socket connections on VC_LISTEN port.
        # New connections are stored in a list, so that the main thread
        # can later on initialise them.
        #
        self.new_listener_server = \
           ListenNewEditorsThread(port = VC_LISTEN_PORT, event = listener_evt,
               new_socks=self.new_listen_socks)
        self.new_listener_server.setDaemon(1)
        self.new_listener_server.start()

        #
        # This thread listens for new socket connections on VC_TALK port.
        # New connections are stored in a list, so that the main thread
        # can later on initialise them.        
        #
        self.new_talker_server = \
           NewConnListThread(port = VC_TALK_PORT, event = talker_evt,
               new_socks=self.new_talk_socks,
               new_socks_lock=self.new_socks_lock)
        self.new_talker_server.setDaemon(1)        
        self.new_talker_server.start()

class DataEvtSource(Object.Object):
    """abstract class which supplies a data_event for ServerMainThread 
    subclasses with external message loops.

    **INSTANCE ATTRIBUTES**

    *none*
    """
    def __init__(self, **args_super):
        self.deep_construct(DataEvtSource, 
                            {}, 
                            args_super)
    def data_event(self, id):
        """virtual method which supplies a data_event for ServerMainThread 
        subclasses 
        
        **INPUTS**

        STR *id* -- The unique ID of the listen socket
        
        **OUTPUTS**
        
        *SocketHasDataEvent* -- the data event which will allow the
        data thread to ensure that process_ready_socks is called.
        """
        debug.virtual('DataEvtSource.data_event')

class DataEvtSourceWin32(Object.Object):
    """implementation of DataEvtSource using Win32 events

    **INSTANCE ATTRIBUTES**

    *PyHandle evt_sockets_ready* -- Win32 event raised when one of the
     active VC_LISTEN sockets has unread data.

    """
    def __init__(self, evt_sockets_ready, **args_super):
        self.deep_construct(DataEvtSourceWin32, 
                            {'evt_sockets_ready': evt_sockets_ready}, 
                            args_super)
    def data_event(self, id):
        """virtual method which supplies a data_event for ServerMainThread 
        subclasses 
        
        **INPUTS**

        STR *id* -- The unique ID of the listen socket
        
        **OUTPUTS**
        
        *SocketHasDataEvent* -- the data event which will allow the
        data thread to ensure that process_ready_socks is called.
        """
        return Win32SomeSocketHasDataEvent(self.evt_sockets_ready)

class ExtLoopWin32(Object.OwnerObject):
    """abstract class providing an external win32 message message loop for
    concrete subclasses of ServerMainThread using win32event

    **INSTANCE ATTRIBUTES**


    *PyHandle evt_new_listen_conn* -- Win32 event raised when a new
     socket connection is opened on VC_LISTEN port.
    
    *PyHandle evt_new_talk_conn* -- Win32 event raised when a new
     socket connection is opened on VC_TALK port.
    
    *PyHandle evt_sockets_ready* -- Win32 event raised when one of the
     active VC_LISTEN sockets has unread data.
    """
    def __init__(self, test_suite = None, **args_super):
        self.deep_construct(ExtLoopWin32, 
                            {'test_suite': test_suite,
                             'evt_new_listen_conn': 
                                 win32event.CreateEvent(None, 0, 0, None),
                             'evt_new_talk_conn': 
                                 win32event.CreateEvent(None, 0, 0, None),
                             'evt_sockets_ready': 
                                 win32event.CreateEvent(None, 0, 0, None),
                             'evt_quit': 
                                 win32event.CreateEvent(None, 0, 0, None),
                             }, 
                            args_super)

    def server(self):
        """returns a reference to the server

        **INPUTS**

        *none*

        **OUTPUTS**

        *ServerMainThread* -- the underlying server
        """
        debug.virtual('ExtLoopWin32.server')

    def data_event(self, id):
        """virtual method which supplies a data_event for ServerMainThread 
        subclasses 
        
        **INPUTS**

        STR *id* -- The unique ID of the listen socket
        
        **OUTPUTS**
        
        *SocketHasDataEvent* -- the data event which will allow the
        data thread to ensure that process_ready_socks is called.
        """
        return Win32SomeSocketHasDataEvent(self.evt_sockets_ready)

    def run(self):
        """Start the server as well as the ExtLoopWin32 message loop.

        The server will run 2 threads that raise Win32 events respectively 
        when:

        - a request for a new connections on VC_LISTEN port is
          received (*self.evt_new_listen_conn* event).

        - a request for a new connections on VC_PORT port is received
          (*self.evt_new_talk_conn* event)

        It will also runs an additional thread for each connected editor,
        once both the listen and talk sockets have been created and
        packaged by package_sock_pair, which uses the
        self.evt_sockets_ready event.

        These events (plus *self.evt_quit*) are handled inside an
        event loop run by this method.

        We need this event loop so that speech events can be processed
        (actually, I think the speech events are just forwarded to
        NatSpeak by calling pythoncom.PumpWaitingMessages()).

        Also, the reason why the above four events are not processed
        directly in the threads that raise them, is that processing
        the events involves invoking some natlink methods, and Natlink
        does not behave well outside of the main thread.
        
        **INPUTS**
        
        *none* -- 
        

        **OUTPUTS**
        
        *none* -- 
        """

        # Start threads for monitoring socket connections.

        listener_evt = Win32InterThreadEvent(self.evt_new_listen_conn)
        talker_evt = Win32InterThreadEvent(self.evt_new_talk_conn)
        server = self.server()
        sys.stderr.write('Starting server threads...\n')
        sys.stderr.flush()
        server.start_other_threads(listener_evt, talker_evt)

        #
        # This is the event loop. It is based on a recipe found at:
        # http://aspn.activestate.com/ASPN/Cookbook/Python/Recipe/82236        
        #
        TIMEOUT = 200  #msecs
#        TIMEOUT = 5000  #msecs
        counter = 0
        events = [self.evt_new_listen_conn, self.evt_new_talk_conn,
                         self.evt_sockets_ready, self.evt_quit]
        try:
            sys.stderr.write('Starting message loop...\n')
            try:
                while 1:
                    debug.trace('ExtLoopWin32.run', 'waiting for an event')
                    rc = win32event.MsgWaitForMultipleObjects(events,
#                        (self.evt_new_listen_conn, self.evt_new_talk_conn,
#                         self.evt_sockets_ready), 
#                         self.evt_sockets_ready, self.evt_quit), 
                                0, # wait for all = false
                                win32event.QS_ALLEVENTS, # type of input
                                TIMEOUT) #  (or win32event.INFINITE)


                    if rc == win32event.WAIT_OBJECT_0:
                        #
                        # A new VC_LISTEN connection was opened
                        #
                        debug.trace('ExtLoopWin32.run', 'got evt_new_listen_conn')
                        server.handshake_listen_socks()

                    elif rc == win32event.WAIT_OBJECT_0+1:
                        #
                        # A new VC_TALK connection was opened
                        #
                        debug.trace('ExtLoopWin32.run', 'got evt_new_talk_conn')
                        if not server.handshake_talk_socks():
#                    sys.stderr.write("got 0 from handshake_talk_socks\n")
                            break

                        
                    elif rc == win32event.WAIT_OBJECT_0+2:
                        #
                        # Some of the active VC_LISTEN sockets have received data
                        #
                        debug.trace('ExtLoopWin32.run', 'got evt_sockets_ready')
# ServerMainThread.process_ready_socks takes a list of sockets to check,
# but win32event.Event doesn't seem to provide any way of sending data
# with the event.  Really, I should add a ready_socks list and lock,
# like in ServerSingleThread.  However, for now, just check all sockets.
# process_ready_socks uses Queue's and avoids blocking if there are no
# messages, so this is safe, if slightly inefficient.
                        server.process_ready_socks()

                    elif rc == win32event.WAIT_OBJECT_0+3:
                        #
                        # Server is shutting down. Exit the event loop.
                        #
                        debug.trace('ExtLoopWin32.run', 'got evt_quit')
                        break
                        
                    elif rc == win32event.WAIT_OBJECT_0 + len(events):
                        # A windows message is waiting - take care of it.
                        # (Don't ask me why a WAIT_OBJECT_MSG isn't defined < WAIT_OBJECT_0)
                        # Note: this must be done for COM and other windowsy
                        #   things to work.
#                debug.trace('ExtLoopWin32.run', 'forwarding unknown message')
                        if pythoncom.PumpWaitingMessages():
                            break # wm_quit
                        
                    elif rc == win32event.WAIT_TIMEOUT:
                        # Our timeout has elapsed.
                        # Do some work here (e.g, poll something can you can't thread)
                        #   or just feel good to be alive.
                        # Good place to call watchdog(). (Editor's note: See my "thread lifetime" recepie.)
                        pass
#                debug.trace('ExtLoopWin32.run', 'nothing to do, counter=%s' % counter)
                    else:
                        raise RuntimeError( "unexpected win32wait return value")

                    counter = counter + 1
            except KeyboardInterrupt:
                msg = 'Received KeyboardInterrupt in message loop, exiting\n'
                sys.stderr.write(msg)

        finally:
            sys.stderr.write('Message loop ended, cleaning up\n')
            print 'cleanup method is ', self.cleanup
            self.cleanup()
            sys.stderr.write('ExtLoopWin32.run returning\n')

class ServerNewMediator(ServerMainThread):
    """implementation of ServerMainThread designed to work with the 
    new design of MediatorObject (currently called NewMediatorObject)

    **INSTANCE ATTRIBUTES**

    *NewMediatorObject mediator* -- the parent mediator

    *BOOL test_server* -- flag indicating that the server is set up to
    run regression tests when a test client connects

    *DataEvtSource data_events* -- object which provides data events
    for the data threads for new connections

    {STR : AppStateMessaging} *editors* -- map from unique socket IDs
    to AppStateMessaging interfaces to external edtiors.

    {STR : STR} *editor_names* -- map from AppMgr-assigned instance
    names to unique socket IDs
    """
    def __init__(self, data_events, test_server = 0, **args):
        self.deep_construct(ServerNewMediator,
                            {
                             'mediator': None,
                             'data_events': data_events,
                             'test_server': test_server,
                             'editors': {},
                             'editor_names': {}
                            }, args)
        self.name_parent('mediator')

    def remove_other_references(self):
        """Perform any cleanup prior to quitting.  Called when the main 
        thread has exited its event loop.  Subclasses which override
        this method should be sure to call their parent class's version
        after doing their own cleanup.

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        self.data_events = None
        ServerMainThread.remove_other_references(self)

    def is_test_server(self):
        """indicates whether this server running in test mode, and
        waiting for the next test client

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- true if the server was initialize in test mode
        """
        return self.test_server

    def set_mediator(self, mediator):
        """provides the server with a reference to the new
        MediatorObject, for new_instance callbacks

        **INPUTS**

        *NewMediatorObject mediator* -- the mediator

        **OUTPUTS**

        *none*
        """
        self.mediator = mediator
    
    def data_event(self, id):
        """virtual method which supplies a data_event for ServerMainThread 
        subclasses 
        
        **INPUTS**

        STR *id* -- The unique ID of the listen socket
        
        **OUTPUTS**
        
        *SocketHasDataEvent* -- the data event which will allow the
        data thread to ensure that process_ready_socks is called.
        """
        return self.data_events.data_event(id)

    def possible_editor_cleanup(self, app):
        """cleanup AppState corresponding to editor used in regression
        tests, unless we are using NewMediatorObject, where AppMgr will
        already have taken care of this.

        **INPUTS**

        *AppStateMessaging app* -- the editor interface

        **OUTPUTS**

        *none*
        """
# NewMediatorObject will already have made a delete_instance call to
# AppMgr, which will have cleaned up the editor, so there is nothing for
# us to do
        return

    def _new_instance(self, id, instance, window_info, test_client = 0):
        """add a new AppStateMessaging.  Called internally by
        package_sock_pair
        
        **INPUTS**
        
        STR *id* -- The unique ID of the listen socket

        AppStateMessaging *instance*  -- the new instance

        *(INT, STR, STR) window_info* -- the window id, title, and module name
        of the external editor, or None if not detected yet

        BOOL *test_client* -- flag indicating whether or not the client
        is expecting to be used for regression testing

        **OUTPUTS**
        
        *BOOL* -- true if the instance should be added to our list.
        false if we failed to create a new instance, or if we created
        it, but it should be destroyed because it has run
        the test suite and the server should exit 
        """
        instance_name = self.mediator.new_editor(instance, server = 1,
            check_window = 1, window_info = window_info, 
            test_editor = test_client)
        if instance_name == None:
            return 0
        else:
            self.editors[id] = instance
            self.editor_names[instance_name] = id
            return 1

    def known_instance(self, id):
        """returns a reference to the AppStateMessaging instance 
        associated with  the given ID
        **INPUTS**
        
        STR *id* -- The unique ID of the listen socket

        **OUTPUTS**

        *AppStateMessaging* -- the corresponding instance, or None if
        the id is unknown
        
        *none*
        """
        try:
            return self.editors[id]
        except KeyError:
            return None

    def delete_instance_cbk(self, instance_name, unexpected = 0):
        """callback from NewMediatorObject to notify us that the
        corresponding external editor has exited or disconnected from
        the mediator.

        **INPUTS**

        STR *id* -- The unique identifier assigned by VoiceCode to
        that socket pair.
      
        *BOOL unexpected* -- 1 if the editor broke the connection
        without first sending an editor_disconnecting message

        **OUTPUTS**

        *none*
        """
        debug.trace('ServerNewMediator.delete_instance_cbk', 
            'server received delete instance callback')
        try:
            id = self.editor_names[instance_name]
        except KeyError:
            return
        self.deactivate_data_thread(id)
        del self.editors[id]
        del self.editor_names[instance_name]

    def mediator_closing(self):
        """sends mediator_closing messages to all connected editors

        **INPUTS**
    
        *none*

        **OUTPUTS**

        *none*
        """
        for id, editor in self.editors.items():
            try:
                editor.mediator_closing()
            except messaging.SocketError:
                pass
            self.deactivate_data_thread(id)
        self.editors = {}
        self.editor_names = {}

    def start_other_threads(self, listener_evt, talker_evt):
        """method called to start the secondary threads which
        monitor the VC_TALK and VC_LISTEN ports.  These threads communicate
        with the main thread by means of InterThreadEvent objects, to
        let the main thread know to initialize them.

        These tasks are handled by separate threads because they can
        block.   The secondary threads do not do the initialization
        directly because that involves invoking some natlink methods, 
        and Natlink does not behave well outside of the main thread.
        
        **INPUTS**
        
        *InterThreadEvent* listener_evt -- event object for the
        ListenNewEditorsThread to use to notify the main thread that a
        new editor has connected on the VC_LISTEN port, and that
        handshake_listen_socks should be called
        
        *InterThreadEvent* talker_evt -- event object for the
        NewConnListThread to use to notify the main thread that a
        new talker connection has been established on the VC_TALK port, 
        and that handshake_talk_socks should be called

        **OUTPUTS**
        
        *none* 
        """
        if not self.mediator:
            msg = "can't start ServerNewMediator until you've set its mediator\n"
            raise RuntimeError(msg)
        return ServerMainThread.start_other_threads(self, 
            listener_evt, talker_evt)

    def user_message(self, message):
        """displays a user message via the appropriate channel
        (e.g. stdout, or a MediatorConsole status line, or an 
        editor-specific status line if supported.

        **INPUTS**

        *STR message* -- the message

        **OUTPUTS**

        *none*
        """
        if self.mediator:
            self.mediator.user_message(message)
        else:
            ServerMainThread.user_message(message)







