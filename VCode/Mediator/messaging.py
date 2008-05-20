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
# (C)2001, National Research Council of Canada
#
##############################################################################

"""Classes for communicating with an external editor through a messaging protocol"""

import socket
import re, sys, types
import copy
from xml.marshal.wddx import WDDXMarshaller, WDDXUnmarshaller
from debug import trace, tracing
import Queue
import time
import select
import threading

import debug, Object

class SocketError(RuntimeError):
    def __init__(self, msg):
        RuntimeError.__init__(self, msg)
#        self.msg = msg

# exception to allow receive_string to signal that it was woken from its
# sleep
class WokenUp:
    def __init__(self, msg):
        self.msg = msg

class LightSleeper(Object.Object):
    """class with a sleep method, like the time module, but which
    can be woken by another thread, by setting a threading.Event

    **INSTANCE ATTRIBUTES**

    *threading.Event wakeup_event* -- the underlying threading.Event
    which supplies the wait method and can be checked upon wakeup to see
    if the wait method timed out, or was stopped early.

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, wakeup_event, **args):
        """
        **INPUTS**

        *threading.Event wakeup_event* -- the underlying threading.Event
        which supplies the wait method and can be checked upon wakeup to see
        if the wait method timed out, or was stopped early.

        **NOTE:** if the creator wants to be able to wake up a thread
        sleeping with this object's sleep method, it must retain a
        reference to the wakeup_event object
        """
        self.deep_construct(LightSleeper,
                            {'wakeup_event': wakeup_event}, args)
    
    def sleep(self, timeout):
        """allows the calling thread to sleep for a given number of
        seconds, or until another thread sets the wakeup_event,
        whichever comes first.

        **INPUTS**

        *FLOAT timeout* -- the timeout in seconds, or None to wait
        indefinitely for the wakeup_event

        **OUTPUTS**

        *none*
        """
        self.wakeup_event.wait(timeout)

    def was_woken(self):
        """checks to see if the wakeup_event is set.  Note: since other
        threads may also be waiting on the same event, LightSleeper does
        not clear the wakeup_event.  Unless another thread does so,
        subsequent calls to sleep will terminate immediately

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- true if the wakeup_event is set
        """
        return self.wakeup_event.isSet()


class Messenger(Object.Object):
   
    """abstract base class for transporting messages betweeen external 
    editor and mediator, through some network communication pipe.

    **INSTANCE ATTRIBUTES**

    *none*

    CLASS ATTRIBUTES**
    
    *none* --

    """
    
    def __init__(self, **args_super):
        self.deep_construct(Messenger, 
                            {
                            }, 
                            args_super, 
                            {})


    def wrong_message(self, received, expected, just_name=1):
        
        """Send an error message when received message was not the
        expected one.
        
        **INPUTS**
        
        (STR, {STR: STR}) *received* -- The received message in
        (mess_name, {arg_name: arg_val} format.
        
        STR *expected* -- Name of the message that was expected. 

        BOOL *just_name* -- If *true* just print the name of the
        received message. Don't print argument values.

        **OUTPUTS**
        
        *none* -- 
        """

        if just_name:
            args = ''
        else:
            args = ', %s' % repr(received[1])
        sys.stderr.write("ERROR: Wrong message.\n\n   Received: '%s'%s\n\nExpected one of: %s\n\n" % (received[0], args, repr(expected)))




    def send_mess(self, mess_name, mess_argvals={}):
        
        """Sends a message to the external editor.

        **INPUTS**

        STR *mess_name* -- Identifier indicating what kind of message this is.
        
        {STR: STR} *mess_argvals* -- Dictionary of arguments and
        values for the message to be sent to the editor.
                
        **OUTPUTS**

        *none* response -- 
        """
        debug.virtual('Messenger.send_mess')

    def get_mess(self, expect=None):
        """Gets a message from the external editor.
        **NOTE:** get_mess may block if no message is available.
        
        **INPUTS**
        
        [STR] *expect* -- If not *None*, then make sure the
        message's name is listed in *expect*. If not, send an
        error message.

        **OUTPUTS**
        
        (STR, {STR: STR}) name_argvals_mess -- The message retrieved
         from external editor in *(mess_name, {arg:val})* format, or
         None if no message is available."""

        debug.virtual('Messenger.get_mess')


class MessengerBasic(Messenger):
   
    """This class transports messages betweeen external editor and
    mediator, through some network communication pipe.

    The class uses a three layer messaging protocol. The layers are:

    *Message transport* -- This layer is responsible for carrying
     bytes across the network.

     This is implemented by class [MessTransporter]

    *Message encoding* -- This layer is responsible for encoding the
     semantic content of the message into a string.

     This is implemented by class [MessEncoder]

    *Message packaging* -- This layer is responsible for packaging an
     encoded message (produced using the *Message encoding* layer)
     into a form that can be reliably sent over the net. Also
     responsible for sending the messages using the *Message
     Transport* layer.

     This is implemented by class [MessPackager]

     All three of [MessTransporter], [MessEncoder] and [MessPackager]
     are abstract classes. This allows [Messenger] to support a
     variety of different protocols by mixing appropriate concrete
     subclasses of those.
     

    *NOTE:* The functionality for packaging and shipping/receiving of
     messages are bundled together in [MessagePackager] because
     packaging generally affects the way you receive a message.

    For example, if you package the message as a fixed length string,
    you will receive it by reading that fixed length. If you package it
    as a length prefixed string, you will receive it by first reading
    the length and then reading that many characters.     

    
    **INSTANCE ATTRIBUTES**

    [MessPackager] *packager* -- Used to package and send messages, or
    receive and unpack them.

    [MessEncoder] *encoder* -- Used to translate a string
    message to a list of attribute/value pairs, and vice versa.
        
    [MessTransporter] *transporter* -- Transport channel used to carry
    bytes to to/from a connection.
    

    CLASS ATTRIBUTES**
    
    *none* --

    .. [MessTransporter] file:///./messenger.MessTransporter.html
    .. [MessEncoder] file:///./messenger.MessEncoder.html
    .. [MessPackager] file:///./messenger.MessPackager.html"""
    
    def __init__(self, packager, transporter, encoder, **args_super):
        self.deep_construct(MessengerBasic, 
                            {'packager': packager,
                             'encoder': encoder,
                             'transporter': transporter}, 
                            args_super, 
                            {})





    def send_mess(self, mess_name, mess_argvals=None):
        
        """Sends a message to the external editor.

        **INPUTS**

        STR *mess_name* -- Identifier indicating what kind of message this is.
        
        {STR: STR} *mess_argvals* -- Dictionary of arguments and
        values for the message to be sent to the editor.
                
        **OUTPUTS**

        *none* response -- 
        """

        trace_id = 'send_mess.%s' % mess_name
        if tracing(trace_id):
            trace(trace_id, 'self=%s, mess_name=\'%s\'' % (self, mess_name))
        if mess_argvals == None:
            tmp_args = {}
        else:
            tmp_args = copy.copy(mess_argvals)
        if tracing(trace_id):
            trace(trace_id, 'mess_argvals=\'%s\'' % tmp_args)        
        unpkd_mess = self.encoder.encode(mess_name, tmp_args)
        pkd_mess = self.packager.pack_mess(unpkd_mess)        
        self.packager.send_packed_mess(pkd_mess, self.transporter)


    def get_mess(self, expect=None):
        """Gets a message from the external editor.
        **NOTE:** get_mess may block if no message is available.
        
        **INPUTS**
        
        [STR] *expect* -- If not *None*, then make sure the
        message's name is listed in *expect*. If not, send an
        error message.

        **OUTPUTS**
        
        (STR, {STR: STR}) name_argvals_mess -- The message retrieved
         from external editor in *(mess_name, {arg:val})* format, or
         None if no message is available."""

        trace('get_mess', 'self=%s, expecting %s' % (self, repr(expect)))
        
        pkd_mess = self.packager.get_packed_mess(self.transporter)
        unpkd_mess = self.packager.unpack_mess(pkd_mess)
        name_argvals_mess = self.encoder.decode(unpkd_mess)

        if expect != None and (not (name_argvals_mess[0] in expect)):
            trace('get_mess', 'wrong_message %s, expecting %s' % \
                (repr(name_argvals_mess), repr(expect)))
            self.wrong_message(name_argvals_mess, expect)

        if tracing('get_mess.%s' % name_argvals_mess[0]):
            trace('get_mess.%s' % name_argvals_mess[0], 
                  'got one of %s! It was: %s' \
                  % (repr(expect), repr(name_argvals_mess)))
        
        return name_argvals_mess
        

class MixedMessenger(Messenger):
   
    """A class which sends messages through another Messenger (usually 
    MessengerBasic), but retrieves messages from a Queue.

    **INSTANCE ATTRIBUTES**

    [Messenger] *sender* -- Used to package and send messages

    Queue *receiver* -- Used to get unpacked messages
        

    CLASS ATTRIBUTES**
    
    *none* --

    """
    
    def __init__(self, sender, receiver, **args_super):
        self.deep_construct(MixedMessenger, 
                            {'sender': sender,
                             'receiver': receiver},
                            args_super, 
                            {})

    def send_mess(self, mess_name, mess_argvals={}):
        
        """Sends a message to the external editor.

        **INPUTS**

        STR *mess_name* -- Identifier indicating what kind of message this is.
        
        {STR: STR} *mess_argvals* -- Dictionary of arguments and
        values for the message to be sent to the editor.
                
        **OUTPUTS**

        *none* response -- 
        """
        self.sender.send_mess(mess_name, mess_argvals)

    def get_mess(self, expect=None):
        """Gets a message from the external editor.
        **NOTE:** In this version, get_mess won't block, but will return 
        None if no message is available.
        
        **INPUTS**
        
        [STR] *expect* -- If not *None*, then make sure the
        message's name is listed in *expect*. If not, send an
        error message.

        **OUTPUTS**
        
        (STR, {STR: STR}) name_argvals_mess -- The message retrieved
         from external editor in *(mess_name, {arg:val})* format, or
         None if no message is available."""

        trace('get_mess', 'self=%s, expecting %s' % (self, repr(expect)))        
        
        try:
            name_argvals_mess = self.receiver.get(block=0)
        except Queue.Empty:
            return None

        if expect != None and (not (name_argvals_mess[0] in expect)):
            self.wrong_message(name_argvals_mess, expect)

        if tracing('get_mess.%s' % name_argvals_mess[0]):
            trace('get_mess.%s' % name_argvals_mess[0], 
                  'got one of %s! It was: %s' \
                  % (repr(expect), repr(name_argvals_mess)))

        return name_argvals_mess
        

class MessPackager(Object.Object):
    """'Shipping and receiving department' for messages.
    
    This class is used to package string messages and send them over a
    transport channel.

    Also used to receive string messages on a transport channel, and
    unpackage them.

    This is an abstract class.


    **INSTANCE ATTRIBUTES**
    
    *none*-- 
    
    CLASS ATTRIBUTES**
    
    *none* -- 
    """
    
    def __init__(self, **args_super):
        self.deep_construct(MessPackager, \
                            {}, \
                            args_super, \
                            {})



    def send_packed_mess(self, pkd_mess, transporter):
        """Send a packaged message over a transport channel.
        
        **INPUTS**
        
        STR *pkd_mess* -- The packed message
        
        [MessTransporter] *transporter* -- Transport channel to be used
        

        **OUTPUTS**
        
        *none* --

        .. [MessTransporter] file:///./messaging.MessTransporter.html"""
        
        debug.virtual('send_pkd_mess')

    def get_packed_mess(self, transporter):
        """Receive a packaged message over a transport channel.
        
        **INPUTS**
                
        [MessTransporter] *transporter* -- Transport channel to be used
        

        **OUTPUTS**

        STR *pkd_mess* -- The packed message

        .. [MessTransporter] file:///./messaging.MessTransporter.html"""
        
        debug.virtual('get_pkd_mess')        



    def pack_mess(self, mess):
        
        """Packs a message into a string that can be carried on a
        transport connection.
        
        **INPUTS**
        
        STR *mess* -- The message as a raw string
        

        **OUTPUTS**
        
        *STR packed_mess* -- The packed message
        """
        
        debug.virtual('pack_mess')

    def unpack_mess(self, mess):
        
        """Unpacks a message to a raw string..
        
        **INPUTS**
        
        *STR* mess -- The packed message
        

        **OUTPUTS**
        
        *STR un_packed_mess* -- The message unpacked to a raw string.
        """
        
        debug.virtual('unpack_mess')


class MessPackager_FixedLenSeq(MessPackager):
    """"Packages messages as a sequence of fixed length chunks.

    Message is packaged as a string which consists of a sequence of
    strings of length *chunk_len* (called the chunks). Chunks are in
    the format:
            
        fXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

    where the first character f is 1 if this is the last chunk of the
    message and 0 otherwise. The last chunk in a message is padded
    with blanks on the right.
        

    **INSTANCE ATTRIBUTES**
    
    *none*-- 
    
    CLASS ATTRIBUTES**
    
    *none* -- 
    """
    
    def __init__(self, chunk_len=1024, **args_super):
        self.deep_construct(MessPackager_FixedLenSeq, 
                            {'chunk_len': chunk_len,
                             'large_white_space': None}, 
                            args_super, 
                            {})
        #
        # Create a large white space used for space padding
        #
        self.large_white_space = ''
        for ii in range(self.chunk_len):
            self.large_white_space = self.large_white_space + ' '



    def send_packed_mess(self, pkd_mess, transporter):
        """Send a packaged message as a sequence of fixed length chunks.
        
        **INPUTS**
        
        STR *pkd_mess* -- The packed message
        
        [MessTransporter] *transporter* -- Transport channel to be used
        

        **OUTPUTS**
        
        *none* --

        ..[MessTransporter] file:///./messaging.MessTransporter.html"""

        if tracing('send_packed_mess'):
            trace('send_packed_mess', 'pkd_mess="%s"' % pkd_mess)
        
        #
        # Nothing particular about how such messages need to be sent.
        #
        transporter.send_string(pkd_mess)

    def get_packed_mess(self, transporter):
        """Receive a message packed as a sequence of fixed length chunks.
        
        **INPUTS**
                
        [MessTransporter] *transporter* -- Transport channel to be used
        

        **OUTPUTS**

        STR *pkd_mess* -- The packed message        

        .. [MessTransporter] file:///./messaging.MessTransporter.html"""

        trace('messaging.MessPackager_FixedLenSeq.get_packed_mess:',
              'invoked')
        #
        # Read the fixed length messages until we get one that starts with 1
        #
        pkd_message = ''
        last_chunk = 0
        while not (last_chunk == '1'):
            a_chunk = transporter.receive_string(self.chunk_len)
            trace('messaging.MessPackager_FixedLenSeq.get_packed_mess:',
                  'read a_chunk="%s"' % a_chunk)
            
            pkd_message = pkd_message + a_chunk
            last_chunk = a_chunk[0]

        
        return pkd_message


    def pack_mess(self, mess):
        
        """Pack the message into a sequence of fixed length chunks.
        
        **INPUTS**
        
        STR *mess* -- The message as a raw string
        

        **OUTPUTS**
        
        *STR packed_mess* -- The packed message
        """
        packed_mess = ''
        while not mess == '':            
            #
            # Make sure you leave room for the single character prefix
            #
            a_chunk = mess[:self.chunk_len-1]

            #
            # If this is last chunk in message, pad it with blanks to the
            # right
            #
            num_padding = (self.chunk_len - 1) - len(a_chunk)
            a_chunk = a_chunk + self.large_white_space[:num_padding]
                                  
            #
            # Is this last chunk in the message?
            # 
            mess = mess[self.chunk_len-1:]
            prefix = int(mess == '')
            
            a_chunk = "%s%s" % (prefix, a_chunk)
            packed_mess = packed_mess + a_chunk
            
        trace('MessPackager_FixedLenSeq.pack_mess', 'returning packed_mess="%s"' % packed_mess)

        return packed_mess
            

    def unpack_mess(self, mess):
        
        """Unpacks a message encoded as a sequence of fixed length chunks.
        
        **INPUTS**
        
        *STR* mess -- The packed message
        

        **OUTPUTS**
        
        *STR unpacked_mess* -- The message unpacked to a raw string.
        """

        unpacked_mess = ''
        while mess != '':
            a_chunk = mess[:self.chunk_len]
            unpacked_mess = unpacked_mess + a_chunk[1:]
            mess = mess[self.chunk_len:]

        return unpacked_mess


class MessEncoder(Object.Object):
    """Encoding scheme for messages.
    
    Used to go translates messages between the *(name, {arg:val})*
    format and raw string format. In this format, *name* is the name
    of the message and *{arg:val}* is a dictionary giving the name and
    values of the various arguments of that message.

    Note that the *arg*s must be strings, but the *val*s can be of any
    encodable type. The encodable types are defined recursively as:

    STR -- a simple string

    [ENCODABLE] -- a list (or tuple) of encodable types.
    
    {STR:ENCODABLE} -- a dictionnary with string keys and encodable values
    
    
    **INSTANCE ATTRIBUTES**
    
    *none*-- 
    
    CLASS ATTRIBUTES**
    
    *none* -- 
    """
        
    def __init__(self, **args_super):
        self.deep_construct(MessEncoder, \
                            {}, \
                            args_super, \
                            {})



    def malformed_mess(self, mess, when, malformation=None):
        """Prints an error message for a malformed message.
        
        **INPUTS**
        
        STR *mess* -- The malformed message
        
        STR *when* -- 'd' for decoding, 'e' for encoding.

        STR *malformation* -- Specific reason why the message was malformed
        

        **OUTPUTS**
        
        *none* -- 
        """

        if when == 'd':
            when_str = 'decode'
        else:
            when_str = 'encode'
            
        print 'ERROR: trying to %s malformed message:\n%s' % when_str
        if malformation != None:
            print 'Malformation was: \'%s\'' % malformation


    def encode(self, mess_name, mess_argvals):
        """Encodes a message as a raw string
        
        **INPUTS**

        STR *mess_name* -- An identifier indicating what type of
        message this is.
        
        {STR: ANY} *mess_argvals* -- The content of the message in
        *{arg:val}* format.
        
        **OUTPUTS**
        
        *STR str_mess* -- The message encoded as a string
        """
        debug.virtual('MessEncoder.encode')


    def decode(self, str_mess):
        """Decodes a message to {arg:val} format.
      
        **INPUTS**
        
        *STR* str_mess -- The message in raw string format
        
        **OUTPUTS**
        
        *(STR, {STR: STR}) name_argvals_mess* -- First element is the
        message name, second element is message arguments in
        *(name, {arg:val})* format.  """

        debug.virtual('MessEncoder.decode')
        


class MessTransporter(Object.Object):
    """Used to send/receive strings on a connection.

    Virtual class.
    
    **INSTANCE ATTRIBUTES**
    
    *none*-- 
    
    CLASS ATTRIBUTES**
            
    *none* -- 
    """
    
    def __init__(self, **args_super):
        self.deep_construct(MessTransporter, \
                            {}, \
                            args_super, \
                            {})



    def send_string(self, a_string):
        """Sends a string on the connection.
        
        **INPUTS**
        
        *STR* a_string -- String to send
        

        **OUTPUTS**
        
        *none* -- 
        """
        
        debug.virtual('send_string')

    def receive_string(self, num_bytes):
        """Receives a string on the connection.
        
        **INPUTS**
        
        INT *num_bytes* -- Number of bytes to receive.
        

        **OUTPUTS**
        
        STR *a_string* -- The received string
        """
        
        debug.virtual('receive_string')        




    def close(self):
        """Close the connection.
        
        **INPUTS**
        
        *none* -- 
        

        **OUTPUTS**
        
        *none* -- 
        """
        
        debug.not_implemented('close')


class MessTransporter_Socket(MessTransporter):
    """Used to send/receive strings on a Socket connection.

    Virtual class.
    
    **INSTANCE ATTRIBUTES**
    
    *socket sock*-- The socket connection used to transport the bytes.

    *FLOAT sleep* -- number of seconds to sleep if before checking again
    if the socket has no data, or None to block when there is no data,
    and check repeatedly until the requested number of bytes are received.

    *LightSleeper sleeper* -- LightSleeper object used to make the
    thread sleep, but let it be woken early from another thread.  May be
    omitted if sleep == None
   
    CLASS ATTRIBUTES**
            
    *none* -- 
    """
    
    def __init__(self, sock, sleep = None, sleeper = None, **args_super):
        self.deep_construct(MessTransporter_Socket, \
                            {'sock': sock,
                             'sleep': sleep,
                             'sleeper': sleeper}, \
                            args_super, \
                            {})
#        sys.stderr.write('MessTransporter_Socket on socket %s being created\n' % repr(self.sock))



#    def __del__(self):
#        sys.stderr.write('MessTransporter_Socket on socket %s being deleted\n' % repr(self.sock))
    def send_string(self, a_string):
        """Sends a string on the Socket connection.
        
        **INPUTS**
        
        *STR* a_string -- String to send
        

        **OUTPUTS**
        
        *none* -- 
        """
        mess_len = len(a_string)
        totalsent = 0
        while totalsent < mess_len:
            try:
                sent = self.sock.send(a_string[totalsent:])
            except socket.error:
                sys.stderr.write('MessTransporter_Socket.send_string:')
                sys.stderr.write(' socket connection broken in %s' % \
                    threading.currentThread().getName())
                raise SocketError("socket connection broken (sending)")
            if sent == 0:
                sys.stderr.write('MessTransporter_Socket.send_string:')
                sys.stderr.write(' no data sent in %s' % \
                    threading.currentThread().getName())
                raise SocketError("socket connection broken (sending)")
#                raise SocketError, "socket connection broken"
            totalsent = totalsent + sent
        

    def receive_string(self, num_bytes):
        """Receives a string on the Socket connection.
        
        **INPUTS**
        
        INT *num_bytes* -- Number of bytes to receive.
        

        **OUTPUTS**
        
        STR *a_string* -- The received string
        """

        a_string = ''
        while len(a_string) < num_bytes:
            if self.sleep:
                while not self.data_available():
                    self.sleeper.sleep(self.sleep)
                    if self.sleeper.was_woken():
                        raise WokenUp("receive_string woken up")
#                    time.sleep(self.sleep)
            try:
                chunk = self.sock.recv(num_bytes - len(a_string))
                if tracing('receive_string'):
                    trace('receive_string', 'read chunk=\'%s\'' % chunk);
            except socket.error:
                chunk = ''
            if chunk == '':
                sys.stderr.write('MessTransporter_Socket.receive_string:')
                sys.stderr.write(' no data received in %s' % \
                    threading.currentThread().getName())
                raise SocketError("socket connection broken (receiving)")
#                raise SocketError, "socket connection broken"
            a_string = a_string + chunk

#        if tracing('receive_string'):
#            trace('receive_string', "received string '%s'" % a_chunk)
        return a_string         

    def data_available(self):
        """check whether the input socket has data

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- true if the select.select indicates that the socket
        has data
        """
# poll by using timeout = 0
        try:
            data, dummy, dummy2 = select.select([self.sock], [], [], 0)
        except socket.error:
            sys.stderr.write('MessTransporter_Socket.data_available:')
            sys.stderr.write(' socket connection broken in %s' % \
                threading.currentThread().getName())
            raise SocketError("socket connection broken (receiving)")
        return len(data) != 0



    def close(self):
        """Close the socket connection.
        
        **INPUTS**
        
        *none* -- 
        

        **OUTPUTS**
        
        *none* -- 
        """
        
        self.sock.close()


###############################################################################
# WDDX-based message encoder
###############################################################################

class MessEncoderWDDX(MessEncoder):
    """WDDX (an XML based protocol) Encoding scheme for messages.
    
    Used to go translates messages between the *(name, {arg:val})*
    format and raw string format. In this format, *name* is the name
    of the message and *{arg:val}* is a dictionary giving the name and
    values of the various arguments of that message.

    Note that the *arg*s must be strings, but the *val*s can be of any
    encodable type. The encodable types are defined recursively as:

    STR -- a simple string

    [ENCODABLE] -- a list (or tuple) of encodable types.
    
    {STR:ENCODABLE} -- a dictionnary with string keys and encodable values
    
    
    **INSTANCE ATTRIBUTES**
    
    [WDDXMarshaller] *marshaller* -- Marshaller for transforming
    Python data values into WDDX strings.

    [WDDXUnmarshaller] *unmarshaller* -- Unmarshaller for transforming
    WDDX strings into Python data values.
    
    CLASS ATTRIBUTES**
    
    *none* -- 

    """
        
    def __init__(self, **args_super):
        self.deep_construct(MessEncoderWDDX, 
                            {'marshaller': WDDXMarshaller(),
                             'unmarshaller': WDDXUnmarshaller()}, 
                            args_super, 
                            {})


    def encode(self, mess_name, mess_argvals):
        """Encodes a message as a raw string
        
        **INPUTS**

        STR *mess_name* -- An identifier indicating what type of
        message this is.
        
        {STR: ANY} *mess_argvals* -- The content of the message in
        *{arg:val}* format.
        
        **OUTPUTS**
        
        *STR str_mess* -- The message encoded as a string
        """

        # test for python 2.3 or later, where bool is its own type
        if type(1) is not type(True):
            for arg, val in mess_argvals.items():
                if val == bool(val) and type(val) is type(True):
                    mess_argvals[arg] = int(val)

        mess_argvals['message_name'] = mess_name
        str_mess = self.marshaller.dumps(mess_argvals)

        return str_mess


    def decode(self, str_mess):
        """Decodes a message to {arg:val} format.
      
        **INPUTS**
        
        *STR* str_mess -- The message in raw string format
        
        **OUTPUTS**
        
        *(STR, {STR: STR}) name_argvals_mess* -- First element is the
        message name, second element is message arguments in
        *(name, {arg:val})* format.  """

        if tracing('messaging.MessEncoderWDDX.decode'):
            trace('messaging.MessEncoderWDDX.decode',
                  'decoding str_mess="%s"' % str_mess)
        
        mess_argvals = self.unmarshaller.loads(str_mess)

        #
        # Name of message is one of the entries in the unmarshalled dictionnary.
        # Remove it from there.
        #
        mess_name = mess_argvals['message_name']
        del mess_argvals['message_name']
        return (mess_name, mess_argvals)
        



###############################################################################
# Functions for converting message arguments to certain data types
###############################################################################

def messarg_is_None(messarg):
    """Indicates whether or not a message argument looks like the value None.

    Returns true if the message argument is one of the
    following:

       *''* (the string)
       *'None'* (the string)
       *'[]'* (the list, not the string... this corresponds to the EmacsLisp
       *nil* value)
    
    **INPUTS**
    
    STR *messarg* -- The message argument to be converted.
    
    
    **OUTPUTS**
    
    BOOL *answer* -- True iif the message argument can be interpreted as the value None
    """

    if messarg == 'None' or messarg == '' or messarg == []:
        #
        # Note: MessEncoder_LenPrefArgs encodes None value as string 'None',
        # while MessEncoderWDDX encodes it as the empty string, and
        # EmacsLisp's nil value gets encoded as an empty list.
        # 
        if tracing('messarg_is_None'):
            trace('messarg_is_None', 'messarg=%s, returning 1' % messarg)       
        return 1
    else:
        if tracing('messarg_is_None'):
            trace('messarg_is_None', 'messarg=%s, returning 0' % messarg)
        return 0


def messarg2int(messarg):
    """Converts a message argument to an int.
    
    **INPUTS**
    
    STR *messarg* -- The message argument to be converted.
    
    
    **OUTPUTS**
    
    INT | None *as_int* -- The message argument converted to int.
    """

    if messarg_is_None(messarg):
        as_int = None
    else:
        as_int = int(float(messarg))
        return as_int

def messarg2str(messarg):
    """Converts a message argument to a str or None.
    
    **INPUTS**
    
    STR *messarg* -- The message argument to be converted.
    
    
    **OUTPUTS**
    
    INT | None *as_str* -- The message argument converted to str.
    """

    if messarg_is_None(messarg):
        as_str = None
    else:
        as_str = messarg
        return as_str

def messarg2intlist(messarg):
    """Converts a message argument to a list of integers.

    If the message argument is the string *'None'*, convert it to
    the *None* object.
        
    **INPUTS**
        
    'None' | [STR] *messarg* -- The message argument to be converted.
        

    **OUTPUTS**
        
    None | [INT] *as_intlist* -- The message argument converted a list of ints.
    """
        
    if messarg == 'None' or messarg == '':
        #
        # Note: MessEncoder_LenPrefArgs encodes None value as string 'None',
        # while MessEncoderWDDX encodes it as the empty string
        #
        return None

    intlist = []
    for ii in range(len(messarg)):
        intlist.append(messarg2int(messarg[ii]))
            
    return intlist

def messarg2inttuple(messarg):
    """Converts a message argument to a tuple of integers.

    If the message argument is the string *'None'*, convert it to
    the *None* object.
        
    **INPUTS**
        
    'None' | [STR] *messarg* -- The message argument to be converted.
        

    **OUTPUTS**
        
    None | [INT] *as_intlist* -- The message argument converted a list of ints.
    """
        
    if messarg == 'None' or messarg == '':
        #
        # Note: MessEncoder_LenPrefArgs encodes None value as string 'None',
        # while MessEncoderWDDX encodes it as the empty string
        #
        return None

    intlist = []
    for ii in range(len(messarg)):
        intlist.append(messarg2int(messarg[ii]))
            
    return tuple(intlist)

        
###############################################################################
# Length prefixed Encoder (now Obsolete and replaced by MessEncoderWDDX
# We keep it here for a while just in case
###############################################################################

class MessEncoder_LenPrefArgs(Object.Object):
    """Encoding scheme for messages with length prefixed argument values.
    
    Used to go translates messages between the (name, {arg:val})
    format and raw string format. The format for the message is:

       message ::= mess_name mess_content

    where:

       *mess_content* -- Is a stringified description of the arguments
        of the messsage and their values. The names of the arguments are
        assumed to be strings, but their values can be string or more
        complex encodable types (see [MessEncoder] documentation for
        details on the encodable types).

    The format of *mess_content* can be described as:

       mess_content ::= dict_descr
       dict_descr := length '{' [key '=' value_descr ',']* '}'
       value_descr ::= (string_descr|list_descr|dict|descr)       
       string_descr ::= length '<' string_content '>'
       list_descr ::= length '[' [value_descr ',']* ']'
       
    where:

       *length* -- the length of the string describing the content of
        a value (i.e. the part that's enclosed by <> (for string
        values), [] (for list values) or {} (for dict values).

       *<>, [], {}* -- balanced expressions indicating whether a value
        is a string, list or dict type (also improve human readability
        which is useful for debugging).

       *string_content* -- is just a string
       

    For example, the following string:

       *set_selection 37{cursor_at=1<1>, range=11[2<15>, 1<3>]}*

    Corresponds to a *set_selection* message with following arguments:

        *{'cursor_at': '1', 'range': ['15', '189']}*
        
    
    **INSTANCE ATTRIBUTES**
    
    *none*-- 
    
    CLASS ATTRIBUTES**
    
    *none* -- 

    ..[MessEncoder] file:///./messaging.MessEncoder.html"""
        
    def __init__(self, **args_super):
        self.deep_construct(MessEncoder_LenPrefArgs, \
                            {}, \
                            args_super, \
                            {})


    def encode(self, mess_name, mess_argvals):
        """Encodes a message as a raw string, with length prefixed argument values.

        See [MessEncoder_LenPrefArgs] for details of the encoded message. 
        **INPUTS**

        STR *mess_name* -- An identifier indicating what type of
        message this is.
        
        {STR: STR} *mess_argvals* -- Dictionnary describing the
         message in {argument:value} format
        
        **OUTPUTS**
        
        *STR str_mess* -- The message encoded as a string

        .. [MessEncoder_LenPrefArgs] file:///./messaging.MessEncoder_LenPrefArgs.html"""

        stringified = self.encode_data_item(mess_argvals)
        str_mess = '%s %s' % (mess_name, stringified)
        
        return str_mess


    def encode_data_item(self, item):
        
        """Encodes data item as a raw string, with length prefixed
        argument values.

        See [MessEncoder_LenPrefArgs] for details of the encoding of
        data items.
        
        **INPUTS**

        ENCODABLE *item* -- Some encodable item (see [MessEncoder] for
        details about encodable types).isinstance(value, types.intType)
        
        **OUTPUTS**
        
        *STR str_item* -- The data item encoded as a string

        .. [MessEncoder] file:///./messaging.MessEncoder.html
        .. [MessEncoder_LenPrefArgs] file:///./messaging.MessEncoder_LenPrefArgs.html"""

        trace('encode_data_item', 'item=\'%s\'' % repr(item))

        #
        # Convert numbers and 'None' to strings
        #
        if (isinstance(item, types.IntType) or
            isinstance(item, types.LongType) or
            isinstance(item, types.FloatType) or
            item == None):
            item = repr(item)
            trace('encode_data_item', 'item=\'%s\' converted to string from a number type' % repr(item))
        

        if isinstance(item, types.StringType):
            #
            # Data item is just a string
            #
            trace('encode_data_item', 'item=\'%s\' is a string' % repr(item))
            delims = ('<', '>')
            str_item = item
        elif isinstance(item, types.ListType) or isinstance(item, types.TupleType):
            #
            # Data item is a list
            #
            delims = ('[', ']')
            str_item = ''
            first = 1
            for an_entry in item:
                if not first:
                    str_item = str_item + ', '
                str_item = str_item + self.encode_data_item(an_entry)
                first = 0
        elif isinstance(item, types.DictType):
            delims = ('{', '}')
            str_item = ''
            first = 1
            sorted_keys = item.keys()
            sorted_keys.sort()
            for a_key in sorted_keys:
                if not first:
                    str_item = str_item + ', '
                str_item = str_item + a_key + '=' + self.encode_data_item(item[a_key])
                first = 0

        #
        #  Enclose data item with appropriate delimiters and prefix it by
        # its length
        #
        str_item = "%s%s%s%s" % (len(str_item), delims[0], str_item, delims[1])

        trace('encode_data_item', 'string item=\'%s\' yields str_item=\'%s\'' % (repr(item), str_item))

        return str_item
                


    def decode(self, str_mess):
        """Decodes a message with lenght prefixed argument values.

        Decodes it to the format (name, {arg:val}).

        See [MessEncoder_LenPrefArgs] for details of the encoded message. 
        
        **INPUTS**
        
        *STR* str_mess -- The message in raw string format
        

        **OUTPUTS**
        
        *(STR, {STR: STR}) name_argvals* -- First element is the
        message name, second element is message arguments in
        *(name, {arg:val})* format.

        .. [MessEncoder_LenPrefArgs] file:///./messaging.MessEncoder_LenPrefArgs.html"""

        #
        # Parse the name and description of the message
        #
        a_match = re.match('\s*([^\s]+)\s+([\s\S]*)', str_mess)
        mess_name = a_match.group(1)
        descr = a_match.group(2)

        #
        # Decode the message's description
        #
#        try:
        (rest, argvals) = self.decode_data_item(descr)
        if not re.match('^\s*$', rest):
            self.malformed_message(mess, 'd')
#        except Exception, err:
#            self.malformed_message(mess, 'd')
        
        return (mess_name, argvals)


    def decode_data_item(self, str_item):
        
        """Decodes the head of a raw string to a data item.

        See [MessEncoder_LenPrefArgs] for details of the encoding of data items.
        
        **INPUTS**

        *STR str_item* -- The data item encoded as a string        
        
        **OUTPUTS**

        (STR, ENCODABLE) (*rest_str, data_item*) -- *rest_str* is the
        rest of *str_item* after the data item has been
        parsed. *data_item* is the value of the data item (or
        message).
                

        .. [MessEncoder] file:///./messaging.MessEncoder.html
        .. [MessEncoder_LenPrefArgs] file:///./messaging.MessEncoder_LenPrefArgs.html"""

        
        #
        # Get type and length of the data item
        #
        trace('decode_data_item', 'str_item=\'%s\'' % str_item)
        a_match = re.match('\s*(\d+)\s*([<\\[{])', str_item)
        length = int(float(a_match.group(1)))
        delim_open = a_match.group(2)

        #
        # Read chop off the description of the item
        #
        str_item = str_item[a_match.end():]
        descr = str_item[:length]
        str_item = str_item[length:]

        trace('decode_data_item', 'descr=\'%s\'' % descr)
        
        #
        # Decode appropriate data type, depending on delimiter
        #
        if delim_open == '<':
            data_item = self.decode_string_descr(descr)
            delim_close = '>'
        elif delim_open == '[':
            data_item = self.decode_list_descr(descr)
            delim_close = ']'            
        elif delim_open == '{':
            data_item = self.decode_dict_descr(descr)
            delim_close = '}'            

        #
        # Read past the closing delimiter
        #
        trace('decode_data_item', 'looking for closing delimiter in \'%s\'' % str_item)
        a_match = re.match('\s*%s\s*' % delim_close, str_item)
        str_item = str_item[a_match.end():]
        
        return (str_item, data_item)

    def decode_string_descr(self, descr):
        """Parses the description of a string item.

        See [MessEncoder_LenPrefArgs] for details of the encoding of
        data items.  **INPUTS**
        
        STR *descr* -- The string description of the list
        
        **OUTPUTS**
        
        STR *the_string* -- The decoded string value.

        ..[MessEncoder_LenPrefArgs] file:///./messaging.MessEncoder_LenPrefArgs.html"""

        # the string description IS the string
        return descr

    def decode_list_descr(self, descr):
        """Parses the description of a string item.

        See [MessEncoder_LenPrefArgs] for details of the encoding of
        data items.

        **INPUTS**
        
        STR *descr* -- The string description of the list
        
        **OUTPUTS**
        
        [ENCODABLE] *the_list* -- The decoded list of encodable values.

        ..[MessEncoder_LenPrefArgs] file:///./messaging.MessEncoder_LenPrefArgs.html"""

        #
        # Parse description until it's all blanks
        #
        the_list = []
        while not re.match('^\s*$', descr):

            #
            # Parse an entry
            #
            (descr, an_entry) = self.decode_data_item(descr)
            the_list.append(an_entry)

            #
            # Parse passed the comma (if any)
            #
            a_match = re.match('\s*,\s*', descr)
            if a_match:
                descr = descr[a_match.end():]

        return the_list

        
    def decode_dict_descr(self, descr):
        """Parses the description of a dict item.

        See [MessEncoder_LenPrefArgs] for details of the encoding of
        data items.  **INPUTS**
        
        STR *descr* -- The string description of the list
        

        **OUTPUTS**
        
        (STR, {STR:ENCODABLE}) *(descr, the_dict)* -- First entry is the The decoded dict item. Keys are strings
        and values are encodable items.
        """

        the_dict = {}

        #
        # Parse description until it's all blanks
        #
        while not re.match('^\s*$', descr):
            trace('decode_dict_descr', 'descr=\'%s\'' % descr)

            #
            # Parse the key and value
            #
            a_match = re.match('\s*([^\s=]+)\s*=\s*', descr)
            a_key = a_match.group(1)
            descr = descr[a_match.end():]

            #
            # Decode the value
            #
            (descr, a_value) = self.decode_data_item(descr)
            the_dict[a_key] = a_value

            #
            # Read past the comma (if any)
            #
            a_match = re.match('\s*,\s*', descr)
            if a_match:
                descr = descr[a_match.end():]
            

        return the_dict

def messenger_factory(sock, sleep = None, sleeper = None):
    """Creates a messenger from proper compenents
    
    **INPUTS**
    
    *socket sock* -- Socket to use for creating the messenger
    
    
    **OUTPUTS**
    
    [Messenger] *a_messenger* -- The created [Messenger] instance

    ..[Messenger] file:///./messaging.Messenger.html"""
    

    #
    # Create a messenger
    #
    packager = MessPackager_FixedLenSeq()
    transporter = MessTransporter_Socket(sock=sock, sleep = sleep,
        sleeper = sleeper)
    encoder = MessEncoderWDDX()
    a_messenger = MessengerBasic(packager=packager, 
        transporter=transporter, encoder=encoder)        
    return a_messenger
