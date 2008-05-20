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

"""Classes for threads to monitor TCP/IP connections, used by tcp_server and 
tcp_client.
"""

import vc_globals

import os, re, select, socket
import string, sys, threading, time

import messaging
import Object

import debug
import messaging, Object
import util
import Queue

# from thread_communication_win32 import *


##############################################################################
# Classes for new version of the server with abstract interfaces for
# sending messages between threads.
#
# calls to/from natlink are still handled by the main thread, like in
# ServerSingleThread.
##############################################################################


class ListenAndQueueMsgsThread(threading.Thread, Object.Object):
    """class for a thread which listens for messages using a given 
    Messenger puts completed messages on a Queue.

    **INSTANCE ATTRIBUTES**

    [Messenger] *underlying* -- underlying messenger (usually
    [MessengerBasic]) used to receive and unpack them messages.

    Queue.Queue *completed_msgs* -- Queue on which to deposit the
    completed messages.

    SocketHasDataEvent *event* -- object used to notify the main thread
    that a socket has data

    Event *connection_ending* -- threading.Event object which will be set
    to true if the connection has been terminated and the thread should die


    (STR, {STR: STR}) conn_broken_msg -- message to put onto the Queue to 
    indicate that the connection was broken unexpectedly.  Note that
    because of thread timing issues, this can occur even if the
    mediator previously received an editor_disconnecting message, or
    even if the editor disconnected in response to a
    mediator_closing or terminating message.  However, in the former case, 
    AppStateMessaging should process the editor_disconnecting message
    first, and will therefore ignore any subsequent messages in the
    queue (including the conn_broken one).  In the latter case, the mediator 
    should already have left the message loop, so, again, the
    conn_broken message should not be processed.

    CLASS ATTRIBUTES**
    
    *none* --

    .. [Messenger] file:///./messenger.Messenger.html
    .. [MessengerBasic] file:///./messenger.MessengerBasic.html"""
    def __init__(self, underlying, completed_msgs, event,
            connection_ending, conn_broken_msg, **args_super):
        self.deep_construct(ListenAndQueueMsgsThread, 
                            {'underlying': underlying,
                             'completed_msgs': completed_msgs,
                             'event': event,
                             'connection_ending': connection_ending,
                             'conn_broken_msg': conn_broken_msg
                            }, 
                            args_super, 
                            exclude_bases={'threading.Thread': 1})
# provides debug messages
#        threading.Thread.__init__(self, verbose = 1)
        threading.Thread.__init__(self)

    def message_queue(self):
        """returns a reference to the message queue in which the thread
        puts completed messages

        **INPUTS**

        *none*

        **OUTPUTS**

        *Queue.Queue* -- the message queue
        """
        debug.trace('ListenAndQueueMsgsThread.message_queue', '** invoked, call stack is:')
#        debug.trace_call_stack('ListenAndQueueMsgsThread.message_queue', '**')
        return self.completed_msgs

    def get_mess(self):
        """Gets a message from the external editor.
        
        **INPUTS**

        *none*
        
        **OUTPUTS**
        
        (STR, {STR: STR}) name_argvals_mess -- The message retrieved
         from external editor in *(mess_name, {arg:val})* format.
         from external editor in *(mess_name, {arg:val})* format, or
         None if no message is available."""

        debug.trace('ListenAndQueueMsgsThread.get_mess', 'invoked')
        return self.underlying.get_mess()
        
    def notify_main(self):
        """notify the main thread that there is a new message waiting in 
        the Queue, and return asynchronously.
        
        **INPUTS**

        **OUTPUTS**

        *none*
        """
        debug.trace('ListenAndQueueMsgsThread.notify_main', 'self.event=%s' % self.event)
#        debug.trace_call_stack('ListenAndQueueMsgsThread.notify_main', '**')
        self.event.notify()

    def run(self):
        """Start listening for data.
        
        **INPUTS**
        
        *none* -- 
        
        **OUTPUTS**
        
        *none* -- 
        """
        debug.trace('ListenAndQueueMsgsThread.run', 
            'thread %s starting' % threading.currentThread().getName())
        while 1:
            try: 
                debug.trace('ListenAndQueueMsgsThread.run', '** getting a message')
                data = self.get_mess()
            except messaging.SocketError, err:
                if self.connection_ending.isSet():
#                    sys.stderr.write('SocketError, but connection_ending was set\n')
                    break
# connection broken unexpectedly (unless we just didn't get the
# connection_ending event in time)
#                sys.stderr.write('unexpected SocketError\n')
                self.completed_msgs.put(self.conn_broken_msg)
                self.notify_main()
                break
            except messaging.WokenUp:
                break

            if debug.tracing('ListenAndQueueMsgsThread.run'):
                debug.trace('ListenAndQueueMsgsThread.run', 
                    '** data=%s' % repr(data))                
            if data:
                debug.trace('ListenAndQueueMsgsThread.run', '** sending notification message that data was received.')
                self.completed_msgs.put(data)
                self.notify_main()
#            time.sleep(0.01)
#            time.sleep(1)
#           waits for timeout, or until connection_ending is set
            self.connection_ending.wait(0.01)
#            self.connection_ending.wait(1.0)
            if self.connection_ending.isSet():
#                sys.stderr.write('connection_ending detected\n')
                break
        self.underlying = None


class ListenNewConnThread(threading.Thread, Object.Object):
    """Abstract base class which listens for new socket connections on 
    a port number and uses an InterThreadEvent object to
    notify the main thread about the new connection.

    Concrete subclasses will define log_conn method to add each new socket 
    to a data structure containing uninitialised connections.
    
    **INSTANCE ATTRIBUTES**
    
    INT *port* -- Port on which to listen for connections.
    
    InterThreadEvent *event* -- object use to send event to the main
    thread

    **CLASS ATTRIBUTES**
    
    *none* -- 
    """
    
    def __init__(self, port, event, **args_super):
        self.deep_construct(ListenNewConnThread, 
                            {'port': port, 
                             'event': event},
                            args_super, 
                            exclude_bases={'threading.Thread': 1})
        threading.Thread.__init__(self)        
        
    def notify_main(self):
        """notify the main thread that there is a new connection waiting
        for a handshake, and return asynchronously.
        
        **INPUTS**

        **OUTPUTS**

        *none*
        """
#        debug.trace_call_stack('ListenNewConnThread.notify_main', '**')
        self.event.notify()

    def run(self):
        """Start listening for new connections.
        
        **INPUTS**
        
        *none* -- 
        

        **OUTPUTS**
        
        *none* -- 
        """

        server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        server_socket.bind((socket.gethostname(), self.port))
        server_socket.listen(5)

        while 1:

            # Accept a new connection
            (client_socket, address) = server_socket.accept()

            debug.trace('ListenNewConnThread.run', 'got new connection on port=%s' % self.port)
            
            #
            # Log it notify the main event loop that
            # it should shake hands with it
            #
            self.log_new_conn(client_socket)
            self.notify_main()

            #
            # When debugging, increase this if you want to see things happen
            # in slow motion
            #
            time.sleep(0.05)
#            time.sleep(1)


    def log_new_conn(self, client_socket):
        
        """Logs a newly received socket connection, so that main event
        loop can later shake hands with it.
        
        **INPUTS**
        
        *socket client_socket* -- Newly received socket connection
        
        **OUTPUTS**
        
        *none* -- 
        """
        debug.virtual('ListenNewConnThread.log_new_conn')


class NewConnListThread(ListenNewConnThread):
    """Listens for new socket connections on a port number
    and use an InterThreadEvent object to
    notify the main thread about the new connection.

    Adds each new socket to a list of uninitialised connections.

    This version is used for new talker sockets, because they may not
    come in in the same order as the previously connected new listener
    sockets, so you have to look through the whole list anyway.
    
    **INSTANCE ATTRIBUTES**
    
    [socket] *new_socks* -- List on which to add any new connection.
    
    [lock] *new_socks_lock* -- Lock on the new connection list.

    **CLASS ATTRIBUTES**
    
    *none* -- 
    """
    
    def __init__(self, new_socks, new_socks_lock, **args_super):
        self.deep_construct(NewConnListThread, 
                            {'new_socks': new_socks,
                             'new_socks_lock': new_socks_lock},
                            args_super)
        
    def log_new_conn(self, client_socket):
        
        """Logs a newly received socket connection, so that main event
        loop can later shake hands with it.
        
        **INPUTS**
        
        *socket client_socket* -- Newly received socket connection
        
        **OUTPUTS**
        
        *none* -- 
        """
        
        self.new_socks_lock.acquire()
        self.new_socks.append((client_socket, [None, None, None, None]))
        self.new_socks_lock.release()        

class ListenNewEditorsThread(ListenNewConnThread):
    """Listens for new socket editor connections on a port number
    and use an InterThreadEvent object to notify the main thread about 
    the new connection.

    Adds each new socket to a Queue of uninitialised connections.

    This version is used for new listener sockets, which should be
    processed in the order they come in.
    
    **INSTANCE ATTRIBUTES**
    
    [Queue] *new_socks* -- Queue to which to add any new connection.

    **CLASS ATTRIBUTES**
    
    *none* -- 
    """
    
    def __init__(self, new_socks, **args_super):
        self.deep_construct(ListenNewEditorsThread, 
                            {'new_socks': new_socks},
                            args_super)

    def log_new_conn(self, client_socket):
        
        """Logs a newly received socket connection, so that main event
        loop can later shake hands with it.
        
        **INPUTS**
        
        *socket client_socket* -- Newly received socket connection
        
        **OUTPUTS**
        
        *none* -- 
        """
        
        self.new_socks.put(client_socket)
    
